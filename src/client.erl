-module(client).
-export([send/2]).


send(File_name, encode) ->
    register(?MODULE, self()),
    {ok, FILE} = file:read_file(File_name),
    {naming, naming@localhost} ! {whoismaster, {?MODULE, node()}}, 
    MASTER = receive 
		{ok, Master } -> Master
	     end,
    {bully, MASTER } ! {'ENCODE', FILE,{?MODULE,  node()}},
    receive
       {ok, Encoded} -> Encoded
    after 30000 ->
            io:format("HOLIS")

    end;
send(DECRYPTED, decode) ->
    register(?MODULE, self()),
    {bully, master@localhost} ! {'DECODE', DECRYPTED, {?MODULE,  node()}},
    receive
       {ok, Decoded} -> Decoded
    after 30000 ->
            io:format("HOLIS")
    end.
