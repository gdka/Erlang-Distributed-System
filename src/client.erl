-module(client).
-export([send/4]).


send(File_name, Output, DNS, encode) ->
    register(?MODULE, self()),
    {ok, FILE} = file:read_file(File_name),
    {naming, DNS} ! {whoismaster, {?MODULE, node()}}, 
    MASTER = receive 
		{ok, Master } -> Master
	     end,
    {bully, MASTER } ! {'ENCODE', FILE,{?MODULE,  node()}},
    Encoded = receive
       {ok, Encoded1} -> Encoded1
    end,
    file:write_file(Output, Encoded);


send(FILE, Output, DNS, decode) ->
  {ok, DECRYPTED} = file:read_file(FILE),
    register(?MODULE, self()),
    {naming, DNS} ! {whoismaster, {?MODULE, node()}},
      MASTER = receive 
                {ok, Master } -> Master
             end,
    {bully, MASTER} ! {'DECODE', DECRYPTED, {?MODULE,  node()}},
    receive
       {ok, Decoded} -> Decoded
    end,
    file:write_file(Output, Decoded).
