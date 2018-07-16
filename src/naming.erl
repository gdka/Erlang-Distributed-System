-module(naming).


-export([start/1,start/0]).

-record(state, { namingnodes = [], master = null }).


start() ->
    register(?MODULE, self()),
    loop(#state{}).

start(NamingServer) ->
    net_kernel:connect(NamingServer),
    register(?MODULE, self()),
    {?MODULE, NamingServer} ! { iamnamingserver, node()},
    NewState = receive
           {ok, State} -> #state{ master = State#state.master, namingnodes = lists:append( [NamingServer], State#state.namingnodes) }
    after 150000 ->
           #state{}
    end,
    loop(NewState).
     
    
loop(State) ->
    io:format("current nodes ~n"),
    lists:foreach(fun(X) -> io:format("~s~n",[atom_to_list(X)]) end, State#state.namingnodes),
    io:format("current master: ~s~n",[State#state.master]),
    NewState = receive
                   {whoismaster, Client} -> send_master(State,Client);
		   {iammaster, Master} -> set_master(State, Master);
		   {heismaster, Master} -> io:format("changing_master~n"), change_master( Master);
                   {iamnamingserver, Node} -> ack_to_new_node(State, Node);
                   {newnode, Node} -> add_new_node(State,Node);
		   _ -> State
              end,
    loop(NewState).

send_master(State, Node) ->
    Master = State#state.master,
    Message = { ok, Master },
     Node ! Message,
    State.


set_master(State, Master) ->
    report_change_to_nodes( State, Master),
    #state{master = Master, namingnodes = State#state.namingnodes }.
    
report_change_to_nodes(State, Master) ->
    Message = {heismaster, Master},
    Nodes =  State#state.namingnodes,
    SendMaster = fun (Node) ->
                     {naming, Node} ! Message
                  end,
   lists:map( SendMaster, Nodes).

report_new_node(State, NewNode) ->
    Message = {newnode, NewNode},
    Nodes = State#state.namingnodes,
    SendNode = fun (Node) ->
                   {naming, Node} ! Message
               end,
     lists:map(SendNode,Nodes).

change_master( Master ) ->
    #state{ master = Master }.

add_node(State,Node) ->
    NewState = #state{master = State#state.master, namingnodes = lists:append([Node], State#state.namingnodes)},
    report_new_node(State,Node),
    NewState.

add_new_node(State, Node) ->
    #state{master = State#state.master, namingnodes = lists:append([Node], State#state.namingnodes)}.

ack_to_new_node(State,Node) ->
    {naming, Node} ! {ok, State},
    add_node(State,Node).
    



	
