;; slim erlang templates
(provide 'slim-erlang)
(require 'tempo)

(tempo-define-template
 "slim-erlang-supervisor"
 '("-module().
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    AChild = {'AName', {'AModule', start_link, []},
              permanent, 2000, worker, ['AModule']},

    {ok, { {one_for_all, 0, 1}, 
         [AChild]}}."
   n))


(tempo-define-template
 "slim-erlang-gen-server"
 '("-module().
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, terminate/2, code_change/3
         handle_call/3, handle_cast/2, handle_info/2
        ]).
-define(SERVER, ?MODULE).
-record(state, {}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> 
  {ok, #state{}}.

handle_call(_Request, _From, State) -> 
  {reply, ok, State}.

handle_cast(_Msg, State) -> 
  {noreply, State}.

handle_info(_Info, State) -> 
  {noreply, State}.

terminate(_Reason, _State) -> 
  ok.

code_change(_OldVsn, State, _Extra) -> 
  {ok, State}."
   n))

(tempo-define-template
 "slim-erlang-ct-suite"
 '("-module().
-compile(export_all).

all() ->
    [ it_works ].

init_per_suite(C) ->
    C.

it_works(_) ->
    ok = (fun() -> not_ok end)()."
   n))
