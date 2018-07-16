
-module(bully).
-author("vovak").

%% API
-export([start/1, addMeToTheSystem/1]).

-define(ELECTION_MESSAGE, 'ELEC').
-define(ADD_NODE_MESSAGE, 'ADD').
-define(ADD_MENSSGE, 'WTF').
-define(ELECTION_MESSAGE_RESPONSE, 'OKAY').
-define(COORDINATOR_MESSAGE, 'BOSS').
-define(RESPONSE_TIMEOUT, 10).

-record(state, {timeout = infinity, knownnodes = [], coordinator = node()}).

start(Nodes) ->
  register(?MODULE, self()),
  io:format("Node ~s has a PId of ~s.~n", [node(), os:getpid()]),
  loop(startElection(#state{knownnodes = Nodes}, Nodes)).

loop(State) ->
  Timeout = State#state.timeout,
  Coordinator = State#state.coordinator,
  NewState = receive
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

addMeToTheSystem(Node) ->
    sendAddMessage(Node),
    register(?MODULE, self()),
    receive
        {?ADD_MENSSGE , _ } ->  io:format("Connected~n")
    end,
    NewState = #state{ knownnodes = nodes(), coordinator = Node },
    loop(NewState).

addNodeStart(State, Node) when (node() == State#state.coordinator) ->
    lists:foreach(fun sendAddMessage/1, State#state.knownnodes),
    NewState = addNode(State, Node),
    sendAddReplyMessage(Node, NewState),
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