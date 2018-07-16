-module(dcrypto).
-export([encode/1,decode/1]).


key(KeyPath) ->
        {ok, PemBin } = file:read_file(KeyPath),
        [ RSAEntry ] = public_key:pem_decode(PemBin),
        public_key:pem_entry_decode( RSAEntry).


encode(FILERAW) ->
	 PrivateKey = key("keys/public"),
	public_key:encrypt_public(FILERAW, PrivateKey).

decode(ENCRYPTED) ->
	PublicKey = key("keys/private"),
	Decoded = public_key:decrypt_private(ENCRYPTED, PublicKey).
