-module(gpio_srv).
-author('KingBrewer').

-behaviour(gen_server).

%% API functions
-export([start/3, stop/1,
         read/1, write/2]).

%% Functions for supervisor
-export([start_link/4]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, 
        {gpio_pin :: integer(),
         direction :: in | out,
         active_low :: boolean(),
         gpio_path :: string(),
         gpio_file_ref :: file:io_device()
        }).

-define(CHILD_ID(Pin),
    binary_to_atom(iolist_to_binary(io_lib:format("~p_~p", [gpio_srv, Pin])), utf8)).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server by calling the supervisor
%%
%% @spec start(integer(), in | out, true | false) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(Pin, Direction, ActiveLow) ->
    case supervisor:start_child(gpio_sup, [Pin, Direction, ActiveLow]) of
        %% to maintain backward compatibility of the original API
        %% initialization will return `Pin` number, that should be
        %% used as Ref. Formerly it was a Pid, but for all the consumers
        %% that should be completely transparent, as they had to pass the
        %% return value anyway to all other functions. It is obviously much more
        %% convinient for the new code to use GPIO number, rather than PID.
        {ok, _} ->
            Pin;
        {error, {already_started, _}} ->
            %% even here we don't want to reveal the Pid, as we don't want
            %% anyone to mess with the internal implementation
            {error, {already_started, Pin}};
        Err ->
            Err
    end.

stop(Pin) ->
    supervisor:terminate_child(gpio_sup, whereis(?CHILD_ID(Pin))).

read(Pin) ->
    gen_server:call(?CHILD_ID(Pin), read_value).

write(Pin, Value) ->
    gen_server:call(?CHILD_ID(Pin), {write_value, Value}).


%%%===================================================================
%%% Functions for supervisor
%%%===================================================================
start_link(GPIO_PATH, Pin, Direction, ActiveLow) when is_list(GPIO_PATH),
                                                      is_integer(Pin),
                                                      is_atom(Direction) ->
    ChildId = ?CHILD_ID(Pin),
    gen_server:start_link({local, ChildId}, ?MODULE,
                          [GPIO_PATH, Pin, Direction, ActiveLow], []).



%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Path, Pin, Direction, ActiveLow]) ->
    process_flag(trap_exit, true),
    PinFileRef = configure(Path, Pin, Direction, ActiveLow),
    State = #state{gpio_pin = Pin,
                   gpio_path = Path,
                   direction = Direction,
                   active_low = ActiveLow,
                   gpio_file_ref = PinFileRef},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(read_value, _From, #state{gpio_file_ref = Ref} = State) ->
    Reply = read_value(Ref),
    {reply, Reply, State};
handle_call({write_value, Value}, _From, #state{gpio_file_ref = Ref} = State) ->
    Reply = write_value(Ref, Value),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{gpio_pin = Pin, gpio_file_ref = Ref, gpio_path = Path} = _State) ->
    ok = file:close(Ref),
    ok = release(Path, Pin),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
% On some Pis, the GPIO interface can take a moment to set up. This
% tries for a while to open the direction file before giving up.
configure(GPIOPath, Pin, Direction, ActiveLow) ->
  % Export the GPIO pin
  ok = export_gpio_pin(GPIOPath, Pin),
  % Set pin's direction (in/out)
  ok = set_direction(GPIOPath, Pin, Direction),
  % Set active_low property (true -> active low, false -> active high)
  ok = set_active_low(GPIOPath, Pin, ActiveLow),
  {ok, RefVal} = file:open(GPIOPath ++ "/gpio" ++ integer_to_list(Pin) ++ "/value", [read, write]),
  RefVal.

release(GPIOPath, Pin) ->
  {ok, RefUnexport} = file:open(GPIOPath ++ "/unexport", [write]),
  ok = write_value(RefUnexport, Pin),
  file:close(RefUnexport).

write_value(Ref, Val) when is_list(Val) ->
  file:position(Ref, 0),
  file:write(Ref, Val);
write_value(Ref, Val) when is_integer(Val) ->
  write_value(Ref, integer_to_list(Val)).

read_value(Ref) ->
  file:position(Ref, 0),
  {ok, Data} = file:read(Ref, 1),
  Data.

export_gpio_pin(GPIOPath, Pin) when is_integer(Pin) ->
  {ok, RefExport} = file:open(GPIOPath ++ "/export", [write]),
  ok = write_value(RefExport, Pin),
  file:close(RefExport).

set_direction(GPIOPath, Pin, Direction) when is_integer(Pin) ->
  DirectionFile = GPIOPath ++ "/gpio" ++ integer_to_list(Pin) ++ "/direction",
  % It can take a moment for the GPIO pin file to be created.
  RefDirection = waitForDirectionFile(DirectionFile),
  ok = case Direction of
    in -> write_value(RefDirection, "in");
    out -> write_value(RefDirection, "out")
  end,
  file:close(RefDirection).

set_active_low(_GPIOPath, _Pin, undefined) ->
    ok;
set_active_low(GPIOPath, Pin, ActiveLow) when ActiveLow =:= true;
                                              ActiveLow =:= false ->
  ActiveLowFile = GPIOPath ++ "/gpio" ++ integer_to_list(Pin) ++ "/active_low",
  {ok, RefActiveLow} = file:open(ActiveLowFile, [write]),
  ok = case ActiveLow of
      true -> write_value(RefActiveLow, "1");
      false -> write_value(RefActiveLow, "0")
  end,
  file:close(RefActiveLow).

waitForDirectionFile(DirectionFile) ->
    WaitForFile = fun F(Count) when Count > 0 ->
                          { Result, Ref } = file:open(DirectionFile, [write]),
                          case { Result, Ref} of
                              { ok, _ } -> Ref;
                              { error, eacces } ->
                                  timer:sleep(250),
                                  F(Count - 1)
                          end;
                      F(0) -> throw({error, eacces})
                  end,
    WaitForFile(10).
