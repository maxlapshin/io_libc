-module(io_libc_app).
-export([start/2, stop/1]).
-export([init/1]).

start(_, _) ->
  % Ensure module loads and start fake supervisor on success
  case code:load_file(io_libc) of
    {module, io_libc} ->
      supervisor:start_link({local, io_libc_sup}, ?MODULE, []);
    Other ->
      Other
  end.

stop(_) ->
  erlang:exit(erlang:whereis(?MODULE), shutdown).

init([]) ->
  {ok, {{one_for_one, 10, 10}, []}}.
