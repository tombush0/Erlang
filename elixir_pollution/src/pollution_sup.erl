%%%-------------------------------------------------------------------
%% @doc pollution top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pollution_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  SupFlags = #{strategy => one_for_all,
    intensity => 1,   % one restart
    period => 1},     % in one second
  ChildSpecs = [
    #{id=> 'poll_srv',
      start=> {pollution_gen_server, start_link, []},
      restart=> permanent,
      shutdown=>2000,
      type => worker,
      modules => [pollution_gen_server]
    }
  ],
  {ok, {SupFlags, ChildSpecs}}.

