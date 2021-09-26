-module(gpio_sup).
-author('KingBrewer').

-behaviour(supervisor).

%% API functions
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
-define(GPIO(GPIO_PATH), #{id => gpio_srv,
                           start => {gpio_srv, start_link, [GPIO_PATH]},
                           restart => transient,
                           shutdown => 5000,
                           type => worker,
                           modules => [gpio_srv]}).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%%
%% @spec init(Args) -> {ok, {SupFlags :: sup_flags(),
%%                           [ChildSpec :: child_spec()]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 0,
                 period => 1},
    %% It is important to keep the paths unchanged during the application's run.
    %% If you want to control it -> change the settings and restart the application.
    %% This will keep the consistency and ensure we cleaned up after ourselves.
    GPIO_PATH = gpio_app:get_env(gpio_path),
    ChildSpecs = [?GPIO(GPIO_PATH)],
    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
