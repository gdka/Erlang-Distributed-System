-module(client).
-export([send/2]).


send(File_name, decode) ->
    {ok, FILE} = file:read_file(File_name),
    {bully, master@localhost} ! {'DECODE', FILE, node()}; 

send(DECRYPTED, encode) ->
    {bully, master@localhost} ! {'ENCODE', DECRYPTED, node()}.
