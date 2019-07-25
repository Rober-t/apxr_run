%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc The DB is responsible for managing the database.
%%%----------------------------------------------------------------------------
-module(db).

-behaviour(gen_server).

%% Start/Stop
-export([
  start_link/0
]).

%% API
-export([
  read/2,
  write/2,
  delete/2,
  backup/0
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2
]).

%% Xref
-ignore_xref([
  start_link/0,
  read/2,
  write/2,
  delete/2,
  backup/0
]).

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(DIRS,
  [
    "experiment",
    "population",
    "specie",
    "agent",
    "cortex",
    "neuron",
    "sensor",
    "actuator",
    "substrate"
  ]
).

-define(DB, "/tmp/erocksdb").
-define(DB_BACKUPS, "/tmp/erocksdb_backups").

%%%============================================================================
%%% Types
%%%============================================================================

-record(db_state, {
  erocksdb
}).

-type state() :: #db_state{}.

%%%============================================================================
%%% API
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @doc Starts the AgentMgr server.
%% @end
%%-----------------------------------------------------------------------------
-spec start_link() -> {error, string()} | {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% @doc The read function accepts a table name and Id and reads
%%      the corresponding model from the DB.
%% @end
%%-----------------------------------------------------------------------------
-spec read(Key :: models:key(), Table :: models:table()) -> not_found | models:model().
read(Key, Table) ->
  gen_server:call(?MODULE, {read, Key, Table}).

%%-----------------------------------------------------------------------------
%% @doc The write accepts a table name and model and writes it to the DB.
%% @end
%%-----------------------------------------------------------------------------
-spec write(Model :: #{data := map()}, Table :: models:table()) -> ok.
write(#{data := _Data} = Model, Table) ->
  gen_server:call(?MODULE, {write, Model, Table}).

%%-----------------------------------------------------------------------------
%% @doc The delete function accepts a table name and Id and deletes
%%      the corresponding model from the DB.
%% @end
%%-----------------------------------------------------------------------------
-spec delete(Key :: models:key(), Table :: models:table()) -> ok.
delete(Key, Table) ->
  gen_server:call(?MODULE, {delete, Key, Table}).

%%-----------------------------------------------------------------------------
%% @doc The backup function backs up the DB.
%% @end
%%-----------------------------------------------------------------------------
-spec backup() -> ok.
backup() ->
  gen_server:call(?MODULE, {backup}).

%%%============================================================================
%%% gen_server callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the AgentMgr server.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  gen_server:cast(self(), init_phase2),
  InitState = #db_state{},
  logr:debug({db, init, ok, undefined, []}),
  {ok, InitState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle call messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok, state()}.
handle_call({read, Key, Table}, _From, State) ->
  Resp = do_read(Key, Table, State),
  {reply, Resp, State};

handle_call({write, Model, Table}, _From, State) ->
  ok = do_write(Model, Table, State),
  {reply, ok, State};

handle_call({delete, Key, Table}, _From, State) ->
  ok = do_delete(Key, Table, State),
  {reply, ok, State};

handle_call({backup}, _From, State) ->
  ok = do_backup(State),
  {reply, ok, State};

handle_call(Request, From, State) ->
  logr:warning({db, msg, error, "unexpected handle_call", [Request, From]}),
  {reply, ok, State}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Handle cast messages.
%% @end
%%-----------------------------------------------------------------------------
-spec handle_cast(init_phase2, state()) -> {noreply, state()}.
handle_cast(init_phase2, State) ->
  DBH = create_db(),
  NewState = State#db_state{erocksdb = DBH},
  {noreply, NewState}.

%%%============================================================================
%%% Internal functions
%%%============================================================================

do_read(Key, Table, S) ->
  % Filename = build_filename(Key, Table),
  % case file:read_file(Filename) of
  %   {ok, Binary} ->
  %     binary_to_term(Binary);
  %   {error, enoent} ->
  %     not_found
  % end.
  DBH = S#db_state.erocksdb,
  Filename = build_filename(Key, Table),
  case rocksdb:get(DBH, Filename, []) of
    {ok, Binary} ->
      binary_to_term(Binary);
    not_found ->
      not_found;
    Error ->
      logr:error({models, status, error, "read", [Error]}),
      not_found
  end.

do_write(#{data := _Data} = Model, Table, S) ->
  % Key = models:get(id, Model),
  % Filename = build_filename(Key, Table),
  % Blob = term_to_binary(Model),
  % ok = file:write_file(Filename, Blob, [raw, delayed_write]).
  DBH = S#db_state.erocksdb,
  Key = models:get(id, Model),
  Filename = build_filename(Key, Table),
  Blob = term_to_binary(Model),
  ok = rocksdb:put(DBH, Filename, Blob, [{sync, true}]).

do_delete(Key, Table, S) ->
  % Filename = build_filename(Key, Table),
  % ok = file:delete(Filename).
  DBH = S#db_state.erocksdb,
  Filename = build_filename(Key, Table),
  ok = rocksdb:delete(DBH, Filename, []).

create_db() ->
  %del_dir(?DB),
  %[ok = filelib:ensure_dir(filename:join([?DB, D, "ignore"])) || D <- ?DIRS].
  Options = [{create_if_missing, true}],
  {ok, DBH} = rocksdb:open(?DB, Options),
  DBH.

do_backup(S) when S#db_state.erocksdb =/= undefined ->
  DBH = S#db_state.erocksdb,
  Path = ?DB_BACKUPS,
  {ok, Backup} = rocksdb:open_backup_engine(Path),
  rocksdb:create_new_backup(Backup, DBH),
  rocksdb:verify_backup(Backup, 1),
  rocksdb:close_backup_engine(Backup),
  rocksdb:close(DBH),
  del_dir(?DB),
  ok;
do_backup(_S) ->
  del_dir(?DB),
  ok.

del_dir(Dir) ->
  lists:foreach(fun(D) -> ok = file:del_dir(D) end, del_all_files([Dir], [])).

del_all_files([], EmptyDirs) ->
   EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
   {ok, FilesInDir} = file:list_dir(Dir),
   {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                  Path = Dir ++ "/" ++ F,
                                  case filelib:is_dir(Path) of
                                    true ->
                                      {Fs, [Path | Ds]};
                                    false ->
                                      {[Path | Fs], Ds}
                                  end
                               end, {[], []}, FilesInDir),
  lists:foreach(fun(F) -> ok = file:delete(F) end, Files),
  del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

build_filename(Key, Table) when is_tuple(Key) ->
  L = lists:flatten(io_lib:format("~p", [Key])),
  build_filename(L, Table);
build_filename(Key, Table) when is_atom(Key) ->
  L = atom_to_list(Key),
  build_filename(L, Table);
build_filename(Key, Table) when is_integer(Key) ->
  L = integer_to_list(Key),
  build_filename(L, Table);
build_filename(Key, Table) when is_list(Key) ->
  term_to_binary(filename:join([?DB, Table, Key])).