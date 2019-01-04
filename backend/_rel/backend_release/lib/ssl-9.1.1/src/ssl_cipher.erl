%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2007-2018. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%
%%----------------------------------------------------------------------
%% Purpose: Help functions for handling the SSL ciphers
%% 
%%----------------------------------------------------------------------

-module(ssl_cipher).

-include("ssl_internal.hrl").
-include("ssl_record.hrl").
-include("ssl_cipher.hrl").
-include("ssl_handshake.hrl").
-include("ssl_alert.hrl").
-include_lib("public_key/include/public_key.hrl").

-export([security_parameters/2, security_parameters/3, 
	 cipher_init/3, nonce_seed/2, decipher/6, cipher/5, aead_encrypt/5, aead_decrypt/6,
	 suites/1, all_suites/1,  crypto_support_filters/0,
	 chacha_suites/1, anonymous_suites/1, psk_suites/1, psk_suites_anon/1, 
         srp_suites/0, srp_suites_anon/0,
	 rc4_suites/1, des_suites/1, rsa_suites/1, 
         filter/3, filter_suites/1, filter_suites/2,
	 hash_algorithm/1, sign_algorithm/1, is_acceptable_hash/2, is_fallback/1,
	 random_bytes/1, calc_mac_hash/4,
         is_stream_ciphersuite/1]).

-compile(inline).

-type cipher_enum()        :: integer().

-export_type([cipher_enum/0]).

%%--------------------------------------------------------------------
-spec security_parameters(ssl_cipher_format:cipher_suite(), #security_parameters{}) ->
				 #security_parameters{}.
%% Only security_parameters/2 should call security_parameters/3 with undefined as
%% first argument.
%%--------------------------------------------------------------------

security_parameters(?TLS_NULL_WITH_NULL_NULL = CipherSuite, SecParams) ->
    security_parameters(undefined, CipherSuite, SecParams).

%%--------------------------------------------------------------------
-spec security_parameters(ssl_record:ssl_version() | undefined, 
                          ssl_cipher_format:cipher_suite(), #security_parameters{}) ->
				 #security_parameters{}.
%%
%% Description: Returns a security parameters record where the
%% cipher values has been updated according to <CipherSuite> 
%%-------------------------------------------------------------------
security_parameters(Version, CipherSuite, SecParams) ->
    #{cipher := Cipher, mac := Hash, 
      prf := PrfHashAlg} = ssl_cipher_format:suite_definition(CipherSuite),
    SecParams#security_parameters{
      cipher_suite = CipherSuite,
      bulk_cipher_algorithm = bulk_cipher_algorithm(Cipher),
      cipher_type = type(Cipher),
      key_size = effective_key_bits(Cipher),
      expanded_key_material_length = expanded_key_material(Cipher),
      key_material_length = key_material(Cipher),
      iv_size = iv_size(Cipher),
      mac_algorithm = mac_algorithm(Hash),
      prf_algorithm = prf_algorithm(PrfHashAlg, Version),
      hash_size = hash_size(Hash)}.

%%--------------------------------------------------------------------
-spec cipher_init(cipher_enum(), binary(), binary()) -> #cipher_state{}.
%%
%% Description: Initializes the #cipher_state according to BCA
%%-------------------------------------------------------------------
cipher_init(?RC4, IV, Key) ->
    State = crypto:stream_init(rc4, Key),
    #cipher_state{iv = IV, key = Key, state = State};
cipher_init(?AES_GCM, IV, Key) ->
    <<Nonce:64>> = random_bytes(8),
    #cipher_state{iv = IV, key = Key, nonce = Nonce, tag_len = 16};
cipher_init(?CHACHA20_POLY1305, IV, Key) ->
    #cipher_state{iv = IV, key = Key, tag_len = 16};
cipher_init(_BCA, IV, Key) ->
    #cipher_state{iv = IV, key = Key}.

nonce_seed(Seed, CipherState) ->
    CipherState#cipher_state{nonce = Seed}.

%%--------------------------------------------------------------------
-spec cipher(cipher_enum(), #cipher_state{}, binary(), iodata(), ssl_record:ssl_version()) ->
		    {binary(), #cipher_state{}}. 
%%
%% Description: Encrypts the data and the MAC using chipher described
%% by cipher_enum() and updating the cipher state
%% Used for "MAC then Cipher" suites where first an HMAC of the
%% data is calculated and the data plus the HMAC is ecncrypted.
%%-------------------------------------------------------------------
cipher(?NULL, CipherState, <<>>, Fragment, _Version) ->
    GenStreamCipherList = [Fragment, <<>>],
    {GenStreamCipherList, CipherState};
cipher(?RC4, CipherState = #cipher_state{state = State0}, Mac, Fragment, _Version) ->
    GenStreamCipherList = [Fragment, Mac],
    {State1, T} = crypto:stream_encrypt(State0, GenStreamCipherList),
    {T, CipherState#cipher_state{state = State1}};
cipher(?DES, CipherState, Mac, Fragment, Version) ->
    block_cipher(fun(Key, IV, T) ->
			 crypto:block_encrypt(des_cbc, Key, IV, T)
		 end, block_size(des_cbc), CipherState, Mac, Fragment, Version);
cipher(?'3DES', CipherState, Mac, Fragment, Version) ->
    block_cipher(fun(<<K1:8/binary, K2:8/binary, K3:8/binary>>, IV, T) ->
			 crypto:block_encrypt(des3_cbc, [K1, K2, K3], IV, T)
		 end, block_size(des_cbc), CipherState, Mac, Fragment, Version);
cipher(?AES_CBC, CipherState, Mac, Fragment, Version) ->
    block_cipher(fun(Key, IV, T) when byte_size(Key) =:= 16 ->
			 crypto:block_encrypt(aes_cbc128, Key, IV, T);
		    (Key, IV, T) when byte_size(Key) =:= 32 ->
			 crypto:block_encrypt(aes_cbc256, Key, IV, T)
		 end, block_size(aes_128_cbc), CipherState, Mac, Fragment, Version).

aead_encrypt(Type, Key, Nonce, Fragment, AdditionalData) ->
    crypto:block_encrypt(aead_type(Type), Key, Nonce, {AdditionalData, Fragment}).

aead_decrypt(Type, Key, Nonce, CipherText, CipherTag, AdditionalData) ->
    crypto:block_decrypt(aead_type(Type), Key, Nonce, {AdditionalData, CipherText, CipherTag}).

aead_type(?AES_GCM) ->
    aes_gcm;
aead_type(?CHACHA20_POLY1305) ->
    chacha20_poly1305.

build_cipher_block(BlockSz, Mac, Fragment) ->
    TotSz = byte_size(Mac) + erlang:iolist_size(Fragment) + 1,
    {PaddingLength, Padding} = get_padding(TotSz, BlockSz),
    [Fragment, Mac, PaddingLength, Padding].

block_cipher(Fun, BlockSz, #cipher_state{key=Key, iv=IV} = CS0,
	     Mac, Fragment, {3, N})
  when N == 0; N == 1 ->
    L = build_cipher_block(BlockSz, Mac, Fragment),
    T = Fun(Key, IV, L),
    NextIV = next_iv(T, IV),
    {T, CS0#cipher_state{iv=NextIV}};

block_cipher(Fun, BlockSz, #cipher_state{key=Key, iv=IV} = CS0,
	     Mac, Fragment, {3, N})
  when N == 2; N == 3 ->
    NextIV = random_iv(IV),
    L0 = build_cipher_block(BlockSz, Mac, Fragment),
    L = [NextIV|L0],
    T = Fun(Key, IV, L),
    {T, CS0#cipher_state{iv=NextIV}}.

%%--------------------------------------------------------------------
-spec decipher(cipher_enum(), integer(), #cipher_state{}, binary(), 
	       ssl_record:ssl_version(), boolean()) ->
		      {binary(), binary(), #cipher_state{}} | #alert{}.
%%
%% Description: Decrypts the data and the MAC using cipher described
%% by cipher_enum() and updating the cipher state.
%% Used for "MAC then Cipher" suites where first the data is decrypted
%% and the an HMAC of the decrypted data is checked
%%-------------------------------------------------------------------
decipher(?NULL, _HashSz, CipherState, Fragment, _, _) ->
    {Fragment, <<>>, CipherState};
decipher(?RC4, HashSz, CipherState = #cipher_state{state = State0}, Fragment, _, _) ->
    try crypto:stream_decrypt(State0, Fragment) of
	{State, Text} ->
	    GSC = generic_stream_cipher_from_bin(Text, HashSz),
	    #generic_stream_cipher{content = Content, mac = Mac} = GSC,
	    {Content, Mac, CipherState#cipher_state{state = State}}
    catch
	_:_ ->
	    %% This is a DECRYPTION_FAILED but
	    %% "differentiating between bad_record_mac and decryption_failed
	    %% alerts may permit certain attacks against CBC mode as used in
	    %% TLS [CBCATT].  It is preferable to uniformly use the
	    %% bad_record_mac alert to hide the specific type of the error."
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
    end;

decipher(?DES, HashSz, CipherState, Fragment, Version, PaddingCheck) ->
    block_decipher(fun(Key, IV, T) ->
			   crypto:block_decrypt(des_cbc, Key, IV, T)
		   end, CipherState, HashSz, Fragment, Version, PaddingCheck);
decipher(?'3DES', HashSz, CipherState, Fragment, Version, PaddingCheck) ->
    block_decipher(fun(<<K1:8/binary, K2:8/binary, K3:8/binary>>, IV, T) ->
			   crypto:block_decrypt(des3_cbc, [K1, K2, K3], IV, T)
		   end, CipherState, HashSz, Fragment, Version, PaddingCheck);
decipher(?AES_CBC, HashSz, CipherState, Fragment, Version, PaddingCheck) ->
    block_decipher(fun(Key, IV, T) when byte_size(Key) =:= 16 ->
			   crypto:block_decrypt(aes_cbc128, Key, IV, T);
		      (Key, IV, T) when byte_size(Key) =:= 32 ->
			   crypto:block_decrypt(aes_cbc256, Key, IV, T)
		   end, CipherState, HashSz, Fragment, Version, PaddingCheck).

block_decipher(Fun, #cipher_state{key=Key, iv=IV} = CipherState0, 
	       HashSz, Fragment, Version, PaddingCheck) ->
    try 
	Text = Fun(Key, IV, Fragment),
	NextIV = next_iv(Fragment, IV),
	GBC = generic_block_cipher_from_bin(Version, Text, NextIV, HashSz),
	Content = GBC#generic_block_cipher.content,
	Mac = GBC#generic_block_cipher.mac,
	CipherState1 = CipherState0#cipher_state{iv=GBC#generic_block_cipher.next_iv},
	case is_correct_padding(GBC, Version, PaddingCheck) of
	    true ->
		{Content, Mac, CipherState1};
	    false ->
		%% decryption failed or invalid padding,
		%% intentionally break Content to make
		%% sure a packet with invalid padding
		%% but otherwise correct data will fail
		%% the MAC test later
		{<<16#F0, Content/binary>>, Mac, CipherState1}
	end
    catch
	_:_ ->
	    %% This is a DECRYPTION_FAILED but
	    %% "differentiating between bad_record_mac and decryption_failed
	    %% alerts may permit certain attacks against CBC mode as used in
	    %% TLS [CBCATT].  It is preferable to uniformly use the
	    %% bad_record_mac alert to hide the specific type of the error."
            ?ALERT_REC(?FATAL, ?BAD_RECORD_MAC, decryption_failed)
    end.

%%--------------------------------------------------------------------
-spec suites(ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of supported cipher suites.
%%--------------------------------------------------------------------
suites({3, 0}) ->
    ssl_v3:suites();
suites({3, Minor}) ->
    tls_v1:suites(Minor);
suites({_, Minor}) ->
    dtls_v1:suites(Minor).

all_suites({3, _} = Version) ->
    suites(Version)
        ++ chacha_suites(Version)
	++ psk_suites(Version)
	++ srp_suites()
        ++ rc4_suites(Version)
        ++ des_suites(Version)
        ++ rsa_suites(Version);

all_suites(Version) ->
    dtls_v1:all_suites(Version).
%%--------------------------------------------------------------------
-spec chacha_suites(ssl_record:ssl_version() | integer()) ->
                           [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns list of the chacha cipher suites, only supported
%% if explicitly set by user for now due to interop problems, proably need
%% to be fixed in crypto.
%%--------------------------------------------------------------------
chacha_suites({3, _}) ->
    [?TLS_ECDHE_ECDSA_WITH_CHACHA20_POLY1305_SHA256,
     ?TLS_ECDHE_RSA_WITH_CHACHA20_POLY1305_SHA256,
     ?TLS_DHE_RSA_WITH_CHACHA20_POLY1305_SHA256];
chacha_suites(_) ->
    [].

%%--------------------------------------------------------------------
-spec anonymous_suites(ssl_record:ssl_version() | integer()) ->
                              [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the anonymous cipher suites, only supported
%% if explicitly set by user. Intended only for testing.
%%--------------------------------------------------------------------
anonymous_suites({3, N}) ->
    srp_suites_anon() ++ anonymous_suites(N);
anonymous_suites({254, _} = Version) ->
    dtls_v1:anonymous_suites(Version);
anonymous_suites(N)
  when N >= 3 ->
    psk_suites_anon(N) ++
    [?TLS_DH_anon_WITH_AES_128_GCM_SHA256,
     ?TLS_DH_anon_WITH_AES_256_GCM_SHA384,
     ?TLS_DH_anon_WITH_AES_128_CBC_SHA256,
     ?TLS_DH_anon_WITH_AES_256_CBC_SHA256,
     ?TLS_ECDH_anon_WITH_AES_128_CBC_SHA,
     ?TLS_ECDH_anon_WITH_AES_256_CBC_SHA,
     ?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DH_anon_WITH_RC4_128_MD5];
anonymous_suites(2 = N) ->
    psk_suites_anon(N) ++
    [?TLS_ECDH_anon_WITH_AES_128_CBC_SHA,
     ?TLS_ECDH_anon_WITH_AES_256_CBC_SHA,
     ?TLS_ECDH_anon_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DH_anon_WITH_DES_CBC_SHA,
     ?TLS_DH_anon_WITH_RC4_128_MD5];
anonymous_suites(N)  when N == 0;
			  N == 1 ->
    psk_suites_anon(N) ++
        [?TLS_DH_anon_WITH_RC4_128_MD5,
         ?TLS_DH_anon_WITH_3DES_EDE_CBC_SHA,
         ?TLS_DH_anon_WITH_DES_CBC_SHA
        ].

%%--------------------------------------------------------------------
-spec psk_suites(ssl_record:ssl_version() | integer()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the PSK cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
psk_suites({3, N}) ->
    psk_suites(N);
psk_suites(N)
  when N >= 3 ->
    [
     ?TLS_RSA_PSK_WITH_AES_256_GCM_SHA384,
     ?TLS_RSA_PSK_WITH_AES_256_CBC_SHA384,
     ?TLS_RSA_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA256
    ] ++ psk_suites(0);
psk_suites(_) ->
    [?TLS_RSA_PSK_WITH_AES_256_CBC_SHA,
     ?TLS_RSA_PSK_WITH_AES_128_CBC_SHA,
     ?TLS_RSA_PSK_WITH_3DES_EDE_CBC_SHA,
     ?TLS_RSA_PSK_WITH_RC4_128_SHA].

%%--------------------------------------------------------------------
-spec psk_suites_anon(ssl_record:ssl_version() | integer()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the anonymous PSK cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
psk_suites_anon({3, N}) ->
    psk_suites_anon(N);
psk_suites_anon(N)
  when N >= 3 ->
    [
     ?TLS_DHE_PSK_WITH_AES_256_GCM_SHA384,
     ?TLS_PSK_WITH_AES_256_GCM_SHA384,
     ?TLS_ECDHE_PSK_WITH_AES_256_CBC_SHA384,
     ?TLS_DHE_PSK_WITH_AES_256_CBC_SHA384,
     ?TLS_PSK_WITH_AES_256_CBC_SHA384,
     ?TLS_ECDHE_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_DHE_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_PSK_WITH_AES_128_GCM_SHA256,
     ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA256,
     ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA256,
     ?TLS_PSK_WITH_AES_128_CBC_SHA256
    ] ++ psk_suites_anon(0);
psk_suites_anon(_) ->
	[?TLS_DHE_PSK_WITH_AES_256_CBC_SHA,
	 ?TLS_PSK_WITH_AES_256_CBC_SHA,
	 ?TLS_ECDHE_PSK_WITH_AES_128_CBC_SHA,
	 ?TLS_DHE_PSK_WITH_AES_128_CBC_SHA,
	 ?TLS_PSK_WITH_AES_128_CBC_SHA,
	 ?TLS_ECDHE_PSK_WITH_3DES_EDE_CBC_SHA,
	 ?TLS_DHE_PSK_WITH_3DES_EDE_CBC_SHA,
	 ?TLS_PSK_WITH_3DES_EDE_CBC_SHA,
	 ?TLS_ECDHE_PSK_WITH_RC4_128_SHA,
	 ?TLS_DHE_PSK_WITH_RC4_128_SHA,
	 ?TLS_PSK_WITH_RC4_128_SHA].
%%--------------------------------------------------------------------
-spec srp_suites() -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the SRP cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
srp_suites() ->
    [?TLS_SRP_SHA_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_SRP_SHA_DSS_WITH_3DES_EDE_CBC_SHA,
     ?TLS_SRP_SHA_RSA_WITH_AES_128_CBC_SHA,
     ?TLS_SRP_SHA_DSS_WITH_AES_128_CBC_SHA,
     ?TLS_SRP_SHA_RSA_WITH_AES_256_CBC_SHA,
     ?TLS_SRP_SHA_DSS_WITH_AES_256_CBC_SHA].

%%--------------------------------------------------------------------
-spec srp_suites_anon() -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the SRP anonymous cipher suites, only supported
%% if explicitly set by user.
%%--------------------------------------------------------------------
srp_suites_anon() ->
    [?TLS_SRP_SHA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_SRP_SHA_WITH_AES_128_CBC_SHA,
     ?TLS_SRP_SHA_WITH_AES_256_CBC_SHA].

%%--------------------------------------------------------------------
-spec rc4_suites(Version::ssl_record:ssl_version() | integer()) -> 
                        [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the RSA|(ECDH/RSA)| (ECDH/ECDSA) 
%% with RC4 cipher suites, only supported if explicitly set by user. 
%% Are not considered secure any more. Other RC4 suites already
%% belonged to the user configured only category.
%%--------------------------------------------------------------------
rc4_suites({3, 0}) ->
    rc4_suites(0);
rc4_suites({3, Minor}) ->
    rc4_suites(Minor) ++ rc4_suites(0);
rc4_suites(0) ->
    [?TLS_RSA_WITH_RC4_128_SHA,
     ?TLS_RSA_WITH_RC4_128_MD5];
rc4_suites(N) when N =< 3 ->
    [?TLS_ECDHE_ECDSA_WITH_RC4_128_SHA,
     ?TLS_ECDHE_RSA_WITH_RC4_128_SHA,
     ?TLS_ECDH_ECDSA_WITH_RC4_128_SHA,
     ?TLS_ECDH_RSA_WITH_RC4_128_SHA].
%%--------------------------------------------------------------------
-spec des_suites(Version::ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the cipher suites
%% with DES cipher, only supported if explicitly set by user. 
%% Are not considered secure any more. 
%%--------------------------------------------------------------------
des_suites(_)->
    [?TLS_DHE_RSA_WITH_DES_CBC_SHA,
     ?TLS_RSA_WITH_DES_CBC_SHA,
     ?TLS_ECDHE_ECDSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_ECDHE_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DHE_RSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_DHE_DSS_WITH_3DES_EDE_CBC_SHA,
     ?TLS_ECDH_ECDSA_WITH_3DES_EDE_CBC_SHA,
     ?TLS_ECDH_RSA_WITH_3DES_EDE_CBC_SHA
    ].

%%--------------------------------------------------------------------
-spec rsa_suites(Version::ssl_record:ssl_version() | integer()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Returns a list of the RSA key exchange 
%% cipher suites, only supported if explicitly set by user. 
%% Are not considered secure any more. 
%%--------------------------------------------------------------------
rsa_suites({3, 0}) ->
    rsa_suites(0);
rsa_suites({3, Minor}) ->
    rsa_suites(Minor) ++ rsa_suites(0);
rsa_suites(0) ->
    [?TLS_RSA_WITH_AES_256_CBC_SHA,
     ?TLS_RSA_WITH_AES_128_CBC_SHA,
     ?TLS_RSA_WITH_3DES_EDE_CBC_SHA
    ];  
rsa_suites(N) when N =< 3 ->
    [
     ?TLS_RSA_WITH_AES_256_GCM_SHA384,
     ?TLS_RSA_WITH_AES_256_CBC_SHA256,
     ?TLS_RSA_WITH_AES_128_GCM_SHA256,
     ?TLS_RSA_WITH_AES_128_CBC_SHA256
    ].

%%--------------------------------------------------------------------
-spec filter(undefined | binary(), [ssl_cipher_format:cipher_suite()], 
             ssl_record:ssl_version()) -> [ssl_cipher_format:cipher_suite()].
%%
%% Description: Select the cipher suites that can be used together with the 
%% supplied certificate. (Server side functionality)  
%%-------------------------------------------------------------------
filter(undefined, Ciphers, _) -> 
    Ciphers;
filter(DerCert, Ciphers0, Version) ->
    OtpCert = public_key:pkix_decode_cert(DerCert, otp),
    SigAlg = OtpCert#'OTPCertificate'.signatureAlgorithm,
    PubKeyInfo = OtpCert#'OTPCertificate'.tbsCertificate#'OTPTBSCertificate'.subjectPublicKeyInfo,
    PubKeyAlg = PubKeyInfo#'OTPSubjectPublicKeyInfo'.algorithm,

    Ciphers = filter_suites_pubkey(
                ssl_certificate:public_key_type(PubKeyAlg#'PublicKeyAlgorithm'.algorithm),
                Ciphers0, Version, OtpCert),
    {_, Sign} = public_key:pkix_sign_types(SigAlg#'SignatureAlgorithm'.algorithm),
    filter_suites_signature(Sign, Ciphers, Version).

%%--------------------------------------------------------------------
-spec filter_suites([ssl_cipher_format:erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()], map()) ->
                           [ssl_cipher_format:erl_cipher_suite()] |  [ssl_cipher_format:cipher_suite()].
%%
%% Description: Filter suites using supplied filter funs
%%-------------------------------------------------------------------	
filter_suites(Suites, Filters) ->
    ApplyFilters = fun(Suite) ->
                           filter_suite(Suite, Filters)
                   end,
    lists:filter(ApplyFilters, Suites).
    
filter_suite(#{key_exchange := KeyExchange, 
               cipher := Cipher, 
               mac := Hash,
               prf := Prf}, 
             #{key_exchange_filters := KeyFilters,
               cipher_filters := CipherFilters, 
               mac_filters := HashFilters,
               prf_filters := PrfFilters}) ->
    all_filters(KeyExchange, KeyFilters) andalso
        all_filters(Cipher, CipherFilters) andalso
        all_filters(Hash, HashFilters) andalso
        all_filters(Prf, PrfFilters);
filter_suite(Suite, Filters) ->
    filter_suite(ssl_cipher_format:suite_definition(Suite), Filters).

%%--------------------------------------------------------------------
-spec filter_suites([ssl_cipher_format:erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()]) -> 
                           [ssl_cipher_format:erl_cipher_suite()] | [ssl_cipher_format:cipher_suite()].
%%
%% Description: Filter suites for algorithms supported by crypto.
%%-------------------------------------------------------------------
filter_suites(Suites) ->
    Filters = crypto_support_filters(),
    filter_suites(Suites, Filters).

all_filters(_, []) ->
    true;
all_filters(Value, [Filter| Rest]) ->
    case Filter(Value) of
        true ->
            all_filters(Value, Rest);
        false ->
            false
    end.
crypto_support_filters() ->
    Algos = crypto:supports(),
    Hashs =  proplists:get_value(hashs, Algos),
    #{key_exchange_filters => 
          [fun(KeyExchange) ->
                  is_acceptable_keyexchange(KeyExchange,  
                                            proplists:get_value(public_keys, Algos))
           end],
      cipher_filters => 
          [fun(Cipher) ->
                  is_acceptable_cipher(Cipher,  
                                       proplists:get_value(ciphers, Algos))
           end],
      mac_filters => 
          [fun(Hash) ->
                  is_acceptable_hash(Hash, Hashs)
          end],
      prf_filters => 
          [fun(Prf) ->
                  is_acceptable_prf(Prf,  
                                    proplists:get_value(hashs, Algos))
          end]}.

is_acceptable_keyexchange(KeyExchange, _Algos) when KeyExchange == psk;
                                                    KeyExchange == null ->
    true;
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == dh_anon;
                                                   KeyExchange == dhe_psk ->
    proplists:get_bool(dh, Algos);
is_acceptable_keyexchange(dhe_dss, Algos) ->
    proplists:get_bool(dh, Algos) andalso
        proplists:get_bool(dss, Algos);
is_acceptable_keyexchange(dhe_rsa, Algos) ->
    proplists:get_bool(dh, Algos) andalso
        proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == ecdh_anon;
                                                   KeyExchange == ecdhe_psk ->
    proplists:get_bool(ecdh, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == ecdh_ecdsa;
                                                   KeyExchange == ecdhe_ecdsa ->
    proplists:get_bool(ecdh, Algos) andalso
        proplists:get_bool(ecdsa, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == ecdh_rsa;
                                                   KeyExchange == ecdhe_rsa ->
    proplists:get_bool(ecdh, Algos) andalso
        proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(KeyExchange, Algos) when KeyExchange == rsa;
                                                   KeyExchange == rsa_psk ->
    proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(srp_anon, Algos) ->
    proplists:get_bool(srp, Algos);
is_acceptable_keyexchange(srp_dss, Algos) ->
    proplists:get_bool(srp, Algos) andalso
        proplists:get_bool(dss, Algos);
is_acceptable_keyexchange(srp_rsa, Algos) ->
    proplists:get_bool(srp, Algos) andalso
        proplists:get_bool(rsa, Algos);
is_acceptable_keyexchange(_KeyExchange, _Algos) ->
    false.

is_acceptable_cipher(null, _Algos) ->
    true;
is_acceptable_cipher(rc4_128, Algos) ->
    proplists:get_bool(rc4, Algos);
is_acceptable_cipher(des_cbc, Algos) ->
    proplists:get_bool(des_cbc, Algos);
is_acceptable_cipher('3des_ede_cbc', Algos) ->
    proplists:get_bool(des3_cbc, Algos);
is_acceptable_cipher(aes_128_cbc, Algos) ->
    proplists:get_bool(aes_cbc128, Algos);
is_acceptable_cipher(aes_256_cbc, Algos) ->
    proplists:get_bool(aes_cbc256, Algos);
is_acceptable_cipher(Cipher, Algos)
  when Cipher == aes_128_gcm;
       Cipher == aes_256_gcm ->
    proplists:get_bool(aes_gcm, Algos);
is_acceptable_cipher(Cipher, Algos) ->
    proplists:get_bool(Cipher, Algos).

is_acceptable_hash(null, _Algos) ->
    true;
is_acceptable_hash(aead, _Algos) ->
    true;
is_acceptable_hash(Hash, Algos) ->
    proplists:get_bool(Hash, Algos).

is_acceptable_prf(default_prf, _) ->
    true;
is_acceptable_prf(Prf, Algos) ->
    proplists:get_bool(Prf, Algos).

is_fallback(CipherSuites)->
    lists:member(?TLS_FALLBACK_SCSV, CipherSuites).


%%--------------------------------------------------------------------
-spec random_bytes(integer()) -> binary().

%%
%% Description: Generates cryptographically secure random sequence 
%%--------------------------------------------------------------------
random_bytes(N) ->
    crypto:strong_rand_bytes(N).

calc_mac_hash(Type, Version,
	      PlainFragment, #{sequence_number := SeqNo,
			       mac_secret := MacSecret,
			       security_parameters:=
				   SecPars}) ->
    Length = erlang:iolist_size(PlainFragment),
    mac_hash(Version, SecPars#security_parameters.mac_algorithm,
	     MacSecret, SeqNo, Type,
	     Length, PlainFragment).

is_stream_ciphersuite(#{cipher := rc4_128}) ->
    true;
is_stream_ciphersuite(_) ->
    false.
%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
mac_hash({_,_}, ?NULL, _MacSecret, _SeqNo, _Type,
	 _Length, _Fragment) ->
    <<>>;
mac_hash({3, 0}, MacAlg, MacSecret, SeqNo, Type, Length, Fragment) ->
    ssl_v3:mac_hash(MacAlg, MacSecret, SeqNo, Type, Length, Fragment);
mac_hash({3, N} = Version, MacAlg, MacSecret, SeqNo, Type, Length, Fragment)  
  when N =:= 1; N =:= 2; N =:= 3 ->
    tls_v1:mac_hash(MacAlg, MacSecret, SeqNo, Type, Version,
		      Length, Fragment).

bulk_cipher_algorithm(null) ->
    ?NULL;
bulk_cipher_algorithm(rc4_128) ->
    ?RC4;
bulk_cipher_algorithm(des_cbc) ->
    ?DES;
bulk_cipher_algorithm('3des_ede_cbc') ->
    ?'3DES';
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_cbc;
				   Cipher == aes_256_cbc ->
    ?AES_CBC;
bulk_cipher_algorithm(Cipher) when Cipher == aes_128_gcm;
				   Cipher == aes_256_gcm ->
    ?AES_GCM;
bulk_cipher_algorithm(chacha20_poly1305) ->
    ?CHACHA20_POLY1305.

type(Cipher) when Cipher == null;
		  Cipher == rc4_128 ->
    ?STREAM;

type(Cipher) when Cipher == des_cbc;
		  Cipher == '3des_ede_cbc';
		  Cipher == aes_128_cbc;
		  Cipher == aes_256_cbc ->
    ?BLOCK;
type(Cipher) when Cipher == aes_128_gcm;
		  Cipher == aes_256_gcm;
		  Cipher == chacha20_poly1305 ->
    ?AEAD.

key_material(null) ->
    0;
key_material(rc4_128) ->
    16;
key_material(des_cbc) ->
    8;
key_material('3des_ede_cbc') ->
    24;
key_material(aes_128_cbc) ->
    16;
key_material(aes_256_cbc) ->
    32;
key_material(aes_128_gcm) ->
    16;
key_material(aes_256_gcm) ->
    32;
key_material(chacha20_poly1305) ->
    32.

expanded_key_material(null) ->
    0;
expanded_key_material(rc4_128) ->
    16;
expanded_key_material(Cipher) when Cipher == des_cbc ->
    8;
expanded_key_material('3des_ede_cbc') ->
    24;
expanded_key_material(Cipher) when Cipher == aes_128_cbc;
				   Cipher == aes_256_cbc;
				   Cipher == aes_128_gcm;
				   Cipher == aes_256_gcm;
				   Cipher == chacha20_poly1305 ->
    unknown.  


effective_key_bits(null) ->
    0;
effective_key_bits(des_cbc) ->
    56;
effective_key_bits(Cipher) when Cipher == rc4_128;
				Cipher == aes_128_cbc;
				Cipher == aes_128_gcm ->
    128;
effective_key_bits('3des_ede_cbc') ->
    168;
effective_key_bits(Cipher) when Cipher == aes_256_cbc;
				Cipher == aes_256_gcm;
				Cipher == chacha20_poly1305 ->
    256.

iv_size(Cipher) when Cipher == null;
		     Cipher == rc4_128;
		     Cipher == chacha20_poly1305->
    0;

iv_size(Cipher) when Cipher == aes_128_gcm;
		     Cipher == aes_256_gcm ->
    4;

iv_size(Cipher) ->
    block_size(Cipher).

block_size(Cipher) when Cipher == des_cbc;
			Cipher == '3des_ede_cbc' -> 
    8;

block_size(Cipher) when Cipher == aes_128_cbc;
			Cipher == aes_256_cbc;
			Cipher == aes_128_gcm;
			Cipher == aes_256_gcm;
			Cipher == chacha20_poly1305 ->
    16.

prf_algorithm(default_prf, {3, N}) when N >= 3 ->
    ?SHA256;
prf_algorithm(default_prf, {3, _}) ->
    ?MD5SHA;
prf_algorithm(Algo, _) ->
    hash_algorithm(Algo).

mac_algorithm(aead) ->
    aead;
mac_algorithm(Algo) ->
    hash_algorithm(Algo).

hash_algorithm(null)   -> ?NULL;
hash_algorithm(md5)    -> ?MD5;
hash_algorithm(sha)   -> ?SHA; %% Only sha always refers to "SHA-1"
hash_algorithm(sha224) -> ?SHA224;
hash_algorithm(sha256) -> ?SHA256;
hash_algorithm(sha384) -> ?SHA384;
hash_algorithm(sha512) -> ?SHA512;
hash_algorithm(?NULL) -> null;
hash_algorithm(?MD5) -> md5;
hash_algorithm(?SHA) -> sha;
hash_algorithm(?SHA224) -> sha224;
hash_algorithm(?SHA256) -> sha256;
hash_algorithm(?SHA384) -> sha384;
hash_algorithm(?SHA512) -> sha512;
hash_algorithm(Other)  when is_integer(Other) andalso ((Other >= 7) and (Other =< 223)) -> unassigned;
hash_algorithm(Other)  when is_integer(Other) andalso ((Other >= 224) and (Other =< 255)) -> Other.

sign_algorithm(anon)  -> ?ANON;
sign_algorithm(rsa)   -> ?RSA;
sign_algorithm(dsa)   -> ?DSA;
sign_algorithm(ecdsa) -> ?ECDSA;
sign_algorithm(?ANON) -> anon;
sign_algorithm(?RSA) -> rsa;
sign_algorithm(?DSA) -> dsa;
sign_algorithm(?ECDSA) -> ecdsa;
sign_algorithm(Other) when is_integer(Other) andalso ((Other >= 4) and (Other =< 223)) -> unassigned;
sign_algorithm(Other) when is_integer(Other) andalso ((Other >= 224) and (Other =< 255)) -> Other.

hash_size(null) ->
    0;
%% The AEAD MAC hash size is not used in the context 
%% of calculating the master secret. See RFC 5246 Section 6.2.3.3.
hash_size(aead) ->
    0;
hash_size(md5) ->
    16;
hash_size(sha) ->
    20;
%% Uncomment when adding cipher suite that needs it
%hash_size(sha224) ->
%    28;
hash_size(sha256) ->
    32;
hash_size(sha384) ->
    48.
%% Uncomment when adding cipher suite that needs it
%hash_size(sha512) ->
%    64.

%% RFC 5246: 6.2.3.2.  CBC Block Cipher
%%
%%   Implementation note: Canvel et al. [CBCTIME] have demonstrated a
%%   timing attack on CBC padding based on the time required to compute
%%   the MAC.  In order to defend against this attack, implementations
%%   MUST ensure that record processing time is essentially the same
%%   whether or not the padding is correct.  In general, the best way to
%%   do this is to compute the MAC even if the padding is incorrect, and
%%   only then reject the packet.  For instance, if the pad appears to be
%%   incorrect, the implementation might assume a zero-length pad and then
%%   compute the MAC.  This leaves a small timing channel, since MAC
%%   performance depends to some extent on the size of the data fragment,
%%   but it is not believed to be large enough to be exploitable, due to
%%   the large block size of existing MACs and the small size of the
%%   timing signal.
%%
%% implementation note:
%%   We return the original (possibly invalid) PadLength in any case.
%%   An invalid PadLength will be caught by is_correct_padding/2
%%
generic_block_cipher_from_bin({3, N}, T, IV, HashSize)
  when N == 0; N == 1 ->
    Sz1 = byte_size(T) - 1,
    <<_:Sz1/binary, ?BYTE(PadLength0)>> = T,
    PadLength = if
		    PadLength0 >= Sz1 -> 0;
		    true -> PadLength0
		end,
    CompressedLength = byte_size(T) - PadLength - 1 - HashSize,
    <<Content:CompressedLength/binary, Mac:HashSize/binary,
     Padding:PadLength/binary, ?BYTE(PadLength0)>> = T,
    #generic_block_cipher{content=Content, mac=Mac,
			  padding=Padding, padding_length=PadLength0,
			  next_iv = IV};

generic_block_cipher_from_bin({3, N}, T, IV, HashSize)
  when N == 2; N == 3 ->
    Sz1 = byte_size(T) - 1,
    <<_:Sz1/binary, ?BYTE(PadLength)>> = T,
    IVLength = byte_size(IV),
    CompressedLength = byte_size(T) - IVLength - PadLength - 1 - HashSize,
    <<NextIV:IVLength/binary, Content:CompressedLength/binary, Mac:HashSize/binary,
      Padding:PadLength/binary, ?BYTE(PadLength)>> = T,
    #generic_block_cipher{content=Content, mac=Mac,
			  padding=Padding, padding_length=PadLength,
			  next_iv = NextIV}.

generic_stream_cipher_from_bin(T, HashSz) ->
    Sz = byte_size(T),
    CompressedLength = Sz - HashSz,
    <<Content:CompressedLength/binary, Mac:HashSz/binary>> = T,
    #generic_stream_cipher{content=Content,
			   mac=Mac}.

is_correct_padding(#generic_block_cipher{padding_length = Len,
					 padding = Padding}, {3, 0}, _) ->
    Len == byte_size(Padding); %% Only length check is done in SSL 3.0 spec
%% For interoperability reasons it is possible to disable
%% the padding check when using TLS 1.0, as it is not strictly required 
%% in the spec (only recommended), howerver this makes TLS 1.0 vunrable to the Poodle attack 
%% so by default this clause will not match
is_correct_padding(GenBlockCipher, {3, 1}, false) ->
    is_correct_padding(GenBlockCipher, {3, 0}, false);
%% Padding must be checked in TLS 1.1 and after  
is_correct_padding(#generic_block_cipher{padding_length = Len,
					 padding = Padding}, _, _) ->
    Len == byte_size(Padding) andalso
        binary:copy(?byte(Len), Len) == Padding.

get_padding(Length, BlockSize) ->
    get_padding_aux(BlockSize, Length rem BlockSize).

get_padding_aux(_, 0) ->
    {0, <<>>};
get_padding_aux(BlockSize, PadLength) ->
    N = BlockSize - PadLength,
    {N, binary:copy(?byte(N), N)}.

random_iv(IV) ->
    IVSz = byte_size(IV),
    random_bytes(IVSz).

next_iv(Bin, IV) ->
    BinSz = byte_size(Bin),
    IVSz = byte_size(IV),
    FirstPart = BinSz - IVSz,
    <<_:FirstPart/binary, NextIV:IVSz/binary>> = Bin,
    NextIV.

filter_suites_pubkey(rsa, CiphersSuites0, _Version, OtpCert) ->
    KeyUses = key_uses(OtpCert),
    NotECDSAKeyed = (CiphersSuites0 -- ec_keyed_suites(CiphersSuites0)) 
        -- dss_keyed_suites(CiphersSuites0),
    CiphersSuites = filter_keyuse_suites(keyEncipherment, KeyUses,
                                         NotECDSAKeyed,
                                         rsa_suites_encipher(CiphersSuites0)),
    filter_keyuse_suites(digitalSignature, KeyUses, CiphersSuites,
                         rsa_ecdhe_dhe_suites(CiphersSuites));
filter_suites_pubkey(dsa, Ciphers, _, OtpCert) ->  
    KeyUses = key_uses(OtpCert),
    NotECRSAKeyed =  (Ciphers -- rsa_keyed_suites(Ciphers)) -- ec_keyed_suites(Ciphers),
    filter_keyuse_suites(digitalSignature, KeyUses, NotECRSAKeyed,
                         dss_dhe_suites(Ciphers));
filter_suites_pubkey(ec, Ciphers, _, OtpCert) ->
    Uses = key_uses(OtpCert),
    NotRSADSAKeyed = (Ciphers -- rsa_keyed_suites(Ciphers)) -- dss_keyed_suites(Ciphers),
    CiphersSuites = filter_keyuse_suites(digitalSignature, Uses, NotRSADSAKeyed,
                                   ec_ecdhe_suites(Ciphers)),
    filter_keyuse_suites(keyAgreement, Uses, CiphersSuites, ec_ecdh_suites(Ciphers)).

filter_suites_signature(_, Ciphers, {3, N}) when N >= 3 ->
     Ciphers;
filter_suites_signature(rsa, Ciphers, Version) ->
    (Ciphers -- ecdsa_signed_suites(Ciphers, Version)) -- dsa_signed_suites(Ciphers, Version);
filter_suites_signature(dsa, Ciphers, Version) ->
    (Ciphers -- ecdsa_signed_suites(Ciphers, Version)) -- rsa_signed_suites(Ciphers, Version);
filter_suites_signature(ecdsa, Ciphers, Version) ->
    (Ciphers -- rsa_signed_suites(Ciphers, Version)) -- dsa_signed_suites(Ciphers, Version).


%% From RFC 5246 - Section  7.4.2.  Server Certificate
%% If the client provided a "signature_algorithms" extension, then all
%% certificates provided by the server MUST be signed by a
%% hash/signature algorithm pair that appears in that extension.  Note
%% that this implies that a certificate containing a key for one
%% signature algorithm MAY be signed using a different signature
%% algorithm (for instance, an RSA key signed with a DSA key).  This is
%% a departure from TLS 1.1, which required that the algorithms be the
%% same. 
%% Note that this also implies that the DH_DSS, DH_RSA,
%% ECDH_ECDSA, and ECDH_RSA key exchange algorithms do not restrict the
%% algorithm used to sign the certificate.  Fixed DH certificates MAY be
%% signed with any hash/signature algorithm pair appearing in the
%% extension.  The names DH_DSS, DH_RSA, ECDH_ECDSA, and ECDH_RSA are
%% historical.
%% Note: DH_DSS and DH_RSA is not supported
rsa_signed({3,N}) when N >= 3 ->
    fun(rsa) -> true;
       (dhe_rsa) -> true;
       (ecdhe_rsa) -> true;
       (rsa_psk) -> true;
       (srp_rsa) -> true;
       (_) -> false
    end;
rsa_signed(_) ->
    fun(rsa) -> true;
       (dhe_rsa) -> true;
       (ecdhe_rsa) -> true;
       (ecdh_rsa) -> true;
       (rsa_psk) -> true;
       (srp_rsa) -> true;
       (_) -> false
    end.
%% Cert should be signed by RSA
rsa_signed_suites(Ciphers, Version) ->
    filter_suites(Ciphers, #{key_exchange_filters => [rsa_signed(Version)],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).
ecdsa_signed({3,N}) when N >= 3 ->
    fun(ecdhe_ecdsa) -> true;
       (_) -> false
    end;
ecdsa_signed(_) ->
    fun(ecdhe_ecdsa) -> true;
       (ecdh_ecdsa) -> true;
       (_) -> false
    end. 

%% Cert should be signed by ECDSA
ecdsa_signed_suites(Ciphers, Version) ->
    filter_suites(Ciphers, #{key_exchange_filters => [ecdsa_signed(Version)],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

rsa_keyed(dhe_rsa) -> 
    true;
rsa_keyed(ecdhe_rsa) -> 
    true;
rsa_keyed(rsa) -> 
    true;
rsa_keyed(rsa_psk) -> 
    true;
rsa_keyed(srp_rsa) -> 
    true;
rsa_keyed(_) -> 
    false.

%% Certs key is an RSA key
rsa_keyed_suites(Ciphers) ->
   filter_suites(Ciphers, #{key_exchange_filters => [fun(Kex) -> rsa_keyed(Kex) end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

%% RSA Certs key can be used for encipherment
rsa_suites_encipher(Ciphers) ->
    filter_suites(Ciphers, #{key_exchange_filters => [fun(rsa) -> true; 
                                                         (rsa_psk) -> true; 
                                                         (_) -> false
                                                      end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

dss_keyed(dhe_dss) ->
    true;
dss_keyed(spr_dss) -> 
    true;
dss_keyed(_) -> 
    false. 

%% Cert should be have DSS key (DSA)
dss_keyed_suites(Ciphers) ->
    filter_suites(Ciphers, #{key_exchange_filters => [fun(Kex) -> dss_keyed(Kex) end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

%% Cert should be signed by DSS (DSA)
dsa_signed_suites(Ciphers, Version) ->
    filter_suites(Ciphers, #{key_exchange_filters => [dsa_signed(Version)],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).
dsa_signed(_) ->
    fun(dhe_dss) -> true;
       (_) -> false
    end.

dss_dhe_suites(Ciphers) ->
    filter_suites(Ciphers, #{key_exchange_filters => [fun(dhe_dss) -> true;
                                                         (_) -> false
                                                      end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

ec_keyed(ecdh_ecdsa) ->
    true;
ec_keyed(ecdh_rsa) ->
    true;
ec_keyed(ecdhe_ecdsa) ->
    true;
ec_keyed(_) -> 
    false.

%% Certs key is an ECC key
ec_keyed_suites(Ciphers) ->
    filter_suites(Ciphers, #{key_exchange_filters => [fun(Kex) -> ec_keyed(Kex) end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

%% EC Certs key usage keyAgreement
ec_ecdh_suites(Ciphers)->
    filter_suites(Ciphers, #{key_exchange_filters => [fun(ecdh_ecdsa) -> true;
                                                         (_) -> false
                                                      end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

%% EC Certs key usage digitalSignature
ec_ecdhe_suites(Ciphers) ->
    filter_suites(Ciphers, #{key_exchange_filters => [fun(ecdhe_ecdsa) -> true;
                                                         (ecdhe_rsa) -> true;
                                                         (_) -> false
                                                      end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).
%% RSA Certs key usage digitalSignature
rsa_ecdhe_dhe_suites(Ciphers) ->
    filter_suites(Ciphers, #{key_exchange_filters => [fun(dhe_rsa) -> true;
                                                         (ecdhe_rsa) -> true;
                                                         (_) -> false
                                                      end],
                             cipher_filters => [],
                             mac_filters => [],
                             prf_filters => []}).

key_uses(OtpCert) ->
    TBSCert = OtpCert#'OTPCertificate'.tbsCertificate, 
    TBSExtensions = TBSCert#'OTPTBSCertificate'.extensions,
    Extensions = ssl_certificate:extensions_list(TBSExtensions),
    case ssl_certificate:select_extension(?'id-ce-keyUsage', Extensions) of
	undefined ->
	    [];
	#'Extension'{extnValue = KeyUses} ->
            KeyUses
    end.

%% If no key-usage extension is defined all key-usages are allowed
filter_keyuse_suites(_, [], CiphersSuites, _) ->
    CiphersSuites;
filter_keyuse_suites(Use, KeyUse, CipherSuits, Suites) ->
    case ssl_certificate:is_valid_key_usage(KeyUse, Use) of
	true ->
	    CipherSuits;
	false ->
	    CipherSuits -- Suites
    end.
