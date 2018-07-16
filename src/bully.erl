
-module(bully).
-author("vovak").

%% API
-export([start/1, addMeToTheSystem/0]).


-define(ELECTION_MESSAGE, 'ELEC').
-define(ADD_NODE_MESSAGE, 'ADD').
-define(ADD_MENSSGE, 'WTF').
-define(ELECTION_MESSAGE_RESPONSE, 'OKAY').
-define(COORDINATOR_MESSAGE, 'BOSS').
-define(RESPONSE_TIMEOUT, 10).
-define(ENCODE_MESSAGE,'ENCODE').
-define(DECODE_MESSAGE,'DECODE').
-record(state, {timeout = infinity, knownnodes = [], coordinator = node(),dns=naming@localhost}).

start(Nodes) ->
  register(?MODULE, self()),
  io:format("Node ~s has a PId of ~s.~n", [node(), os:getpid()]),
  loop(startElection(#state{knownnodes = Nodes}, Nodes)).

loop(State) ->
  Timeout = State#state.timeout,
  Coordinator = State#state.coordinator,
  io:format("current coordinator: ~s~n",[atom_to_list(Coordinator)]),
  io:format("current nodes: ~n"),
  lists:foreach(fun(X) -> io:format("~s~n", [atom_to_list(X)]) end, State#state.knownnodes),
  NewState = receive
               {?DECODE_MESSAGE, File, Node} -> io:format("Someone wants to decode~n"), handle_decode(State,File,Node);
               {?ENCODE_MESSAGE, File, Node} -> io:format("Someone wants to encode~n"), handle_encode(State, File, Node);
               {?ELECTION_MESSAGE, Node} -> handleElectionMessage(State, Node);
               {?ELECTION_MESSAGE_RESPONSE, _} -> waitForCoordinatorMessage(State);
               {?COORDINATOR_MESSAGE, Node} -> setCoordinator(State, Node);
               {?ADD_NODE_MESSAGE, Node} -> addNodeStart(State, Node);
               {nodedown, Coordinator} -> setCoordinator(State,node()), startElection(State, State#state.knownnodes);
               {nodedown, _} -> State
             after
               Timeout -> becomeCoordinator(State)
             end,
  loop(NewState).


addMeToTheSystem() ->
    register(?MODULE, self()),
    {naming, naming@localhost} ! {whoismaster,{ ?MODULE,node()}},
    Node = receive
             { ok, Master } -> Master
           end, 
    erlang:monitor_node(Node, true),
    sendAddMessage(Node),
    CoordState = receive
                  {?ADD_MENSSGE , StateCoord } ->  io:format("Connected~n"), StateCoord
               end,
    ListAdd = lists:append([Node], CoordState#state.knownnodes),
    NewList = lists:delete(node(), ListAdd),
    NewState = CoordState#state{ knownnodes = NewList, coordinator = Node },
    loop(NewState).

addNodeStart(State, NewNode) when (node() == State#state.coordinator) ->
    net_kernel:connect(NewNode),
    SendAddMessage = fun (Node) ->
                            io:format("~s >>>>> ~s >>>>> ~s~n", [NewNode, ?ADD_NODE_MESSAGE , atom_to_list(Node)]),
                            {?MODULE, Node} ! {?ADD_NODE_MESSAGE, NewNode } end,
    lists:foreach(SendAddMessage, State#state.knownnodes),
    NewState = addNode(State, NewNode),
    sendAddReplyMessage(NewNode, NewState),
    NewState;

addNodeStart(State, Node) -> 
    NewState = addNode(State,Node),
    loop(NewState).

addNode(State, Node) -> 
  net_kernel:connect(Node),
  NewNodes = (State#state.knownnodes)++[Node],
  NewState = State#state{knownnodes = NewNodes },
  NewState.


startElection(State, Nodes) ->
  lists:foreach(fun sendElectionMessage/1, higherIds(Nodes)),
  NewState = State#state{timeout = ?RESPONSE_TIMEOUT},
  NewState.

sendElectionMessage(Node) ->
  sendMessageToNode(Node, ?ELECTION_MESSAGE).

sendOkMessage(Node) ->
  sendMessageToNode(Node, ?ELECTION_MESSAGE_RESPONSE).

sendAddMessage(Node) ->
  sendMessageToNode(Node, ?ADD_NODE_MESSAGE).

sendAddReplyMessage(Node, State) -> 
  sendMessageToMe(Node, State,  ?ADD_MENSSGE).

handleElectionMessage(State, Node) ->
  HigherNodes = higherIds(State#state.knownnodes),
  if
    length(HigherNodes) == 0 -> becomeCoordinator(State);
    true -> sendOkMessage(Node), startElection(State, State#state.knownnodes)
  end.
handle_decode(State,File,Client) ->
    Nodes = State#state.knownnodes,
    NextNode = lists:last(Nodes),
    io:format("~s will decode~n",[atom_to_list(NextNode)]),
    FirstNodes = lists:droplast(Nodes),
    DECODED = rpc:call(NextNode, dcrypto, decode, [File]),
    Client ! {ok, DECODED },
    NowNodes = [ NextNode ] ++ FirstNodes, 

    State#state{knownnodes = NowNodes}.
handle_encode(State,File, Client) ->
    Nodes = State#state.knownnodes,
    NextNode = lists:last(Nodes),
    io:format("~s will encode~n",[atom_to_list(NextNode)]),
    FirstNodes = lists:droplast(Nodes),
    ENCODED = rpc:call(NextNode, dcrypto, encode, [File]),
    Client ! {ok, ENCODED },
    NowNodes = [ NextNode ] ++ FirstNodes,
    State#state{knownnodes = NowNodes}.

waitForCoordinatorMessage(State) ->
  NewState = State#state{timeout = infinity},
  NewState.

setCoordinator(State,Node) ->
%%   monitor_node(#state.coordinator, false),
  monitor_node(Node, true),
  NewState = State#state{coordinator = Node, timeout = infinity},
  io:format("Node ~s now thinks ~s is the leader~n", [atom_to_list(node()), Node]),
  NewState.

becomeCoordinator(State) ->
  setCoordinator(State, node()),
  NewState = State#state{timeout = infinity},
  {naming, State#state.dns} ! {iammaster, node()},
  broadcastCoordinatorMessage(State),
  NewState.

broadcastCoordinatorMessage(State) ->
  lists:foreach(fun sendCoordinatorMessage/1, lowerIds(State#state.knownnodes)).

sendCoordinatorMessage(Node) ->
  sendMessageToNode(Node, ?COORDINATOR_MESSAGE).

higherIds(Nodes) ->
  lists:filter(fun(Node) -> Node > node() end, Nodes).

lowerIds(Nodes) ->
  lists:filter(fun(Node) -> Node < node() end, Nodes).

sendMessageToNode(Node, Message) ->
  io:format("~s >>>>> ~s >>>>> ~s~n", [node(), Message, atom_to_list(Node)]),
  {?MODULE, Node} ! {Message, node()}.

sendMessageToMe(Node, State, Message) ->
  io:format("~s >>>>> ~s >>>>> ~s~n", [node(), Message, atom_to_list(Node)]),
  {?MODULE, Node} ! {Message, State}.
