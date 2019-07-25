%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Copyright (C) 2009 by Gene Sher, DXNN Research Group,
%%%   CorticalComputer@gmail.com
%%%
%%%   The original release of this source code was introduced and explained
%%%   in a book (available for purchase on Amazon) by Gene Sher:
%%%     
%%%     Handbook of Neuroevolution Through Erlang. Springer 2012,
%%%     print ISBN: 978-1-4614-4462-6 ebook ISBN: 978-1-4614-4463-6.
%%% 
%%%   Licensed under the Apache License, Version 2.0 (the "License");
%%%   you may not use this file except in compliance with the License.
%%%   You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%%   Unless required by applicable law or agreed to in writing, software
%%%   distributed under the License is distributed on an "AS IS" BASIS,
%%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%%   See the License for the specific language governing permissions and
%%%   limitations under the License.
%%%%%%%%%%%%%%%%%%%% Deus Ex Neural Network :: DXNN %%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%   Modified Copyright (C) 2018 ApproximateReality
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% @doc Forex trader.
%%% @end
%%%----------------------------------------------------------------------------
-module(fx).

-behaviour(private_scape).

%% API
-export([]).

%% Scape callbacks
-export([
  init/1,
  sense/2,
  actuate/4
]).

%%%============================================================================
%%% Configuration
%%%============================================================================

-define(ALL_TABLES, [metadata, 'EURUSD15']).
-define(FX_TABLES_DIR, "priv/fx_tables/").
-define(FEATURES, [
  open,
  high,
  low,
  close,
  volume,
  diff,
  ema6,
  ema14,
  ema26,
  ema50
]).

%%%============================================================================
%%% Types
%%%============================================================================

-record(fx_state, {
  table_name,
  feature,
  index_start,
  index_end,
  index,
  price_list = [],
  account
}).

-record(account, {
  leverage = 50,
  lot = 10000,
  spread = 0.000150,
  margin = 0,
  balance = 300.0,
  net_asset_value = 300.0,
  realized_pl = 0.0,
  unrealized_pl = 0.0,
  order
}).

-record(order, {
  pair,
  position,
  entry,
  current,
  units,
  change,
  percentage_change = 0,
  profit
}).

-record(technical, {
  id, % Key = {Year, Month, Day, Hour, Minute, Second, sampling_rate}
  open,
  high,
  low,
  close,
  volume,
  diff,
  ema6,
  ema14,
  ema26,
  ema50
}).

-record(metadata,{
  feature, % P = {Currency, Feature}
  first,
  last,
  avg,
  dev,
  stdev,
  quantile25,
  quantile50,
  quantile75,
  max,
  min
}).

-type state() :: #fx_state{}.

%%%============================================================================
%%% Scape callbacks
%%%============================================================================

%%-----------------------------------------------------------------------------
%% @private
%% @doc Initializes the sensor process.
%% @end
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  NewState = sim(),
  {ok, NewState}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Returns sensory input.
%% @end
%%-----------------------------------------------------------------------------
-spec sense(any(), state() | undefined) -> {[float()], state()}.
sense({TableName, Feature, Parameters, Start, Finish}, S) ->
  case S#fx_state.table_name of
    undefined ->
      sense(
        {TableName, Feature, Parameters, Start, Finish},
        init_state(S, TableName, Feature, Start, Finish)
      );
    TableName ->
      case Parameters of
        [HRes, VRes, graph_sensor] ->
          plane_encoded(HRes, VRes, S);
        [HRes, list_sensor] ->
          list_encoded(HRes, S)
      end
  end;
sense({internals, _Parameters}, S) ->
  % Internals are the current long/short/nothing position,
  % the buy price (or -1 if in do nothing state)
  A = S#fx_state.account,
  Result = case A#account.order of
    undefined ->
      [0.0, 0.0, 0.0];
    O ->
      Position = O#order.position,
      Entry = O#order.entry,
      PercentageChange = O#order.percentage_change,
      [Position, Entry, PercentageChange]
  end,
  {Result, S}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc Performs output action.
%% @end
%%-----------------------------------------------------------------------------
-spec actuate(atom(), {atom(), integer()}, models:agent_id(), state()) ->
{{[float()], integer()}, state()}.
actuate(trade, {_TableName, TradeSignal}, _AgentId, S) ->
  A = S#fx_state.account,
  UA = make_trade(S, A, TradeSignal),
  TotalProfit = A#account.balance + A#account.unrealized_pl,
  case (UA#account.balance + UA#account.unrealized_pl) =< 100 of
    true ->
      UA2 = #fx_state{account = #account{}},
      {{[0.0], 1}, UA2};
    false ->
      case update_state(S) of
        sim_over ->
          UA3 = #fx_state{account = #account{}},
          {{[TotalProfit], 1}, UA3};
        US ->
          UA4a = update_account(US, UA),
          logr:info({scape, actuate, ok, "net_asset_value", [UA4a#account.net_asset_value]}),
          UA4b = US#fx_state{account = UA4a},
          {{[0.0], 0}, UA4b}
      end
  end.

%%%============================================================================
%%% Internal functions
%%%============================================================================

sim() ->
  [init_table(TableName) || TableName <- ?ALL_TABLES],
  State = #fx_state{account = #account{}},
  State.

init_state(S, TableName, Feature, StartBL, EndBL) ->
  IndexEnd = case EndBL of
    last ->
      ets:last(TableName);
    _ ->
      prev(TableName, ets:last(TableName), prev, EndBL)
  end,
  IndexStart = prev(TableName, ets:last(TableName), prev, StartBL),
  NewState = S#fx_state{
    table_name = TableName,
    feature = Feature,
    index_start = IndexStart,
    index_end = IndexEnd,
    index = IndexStart
  },
  NewState.

update_state(S) ->
  NextIndex = next(S#fx_state.table_name, S#fx_state.index),
  case NextIndex == S#fx_state.index_end of
    true ->
      sim_over;
    false ->
      S#fx_state{index = NextIndex}
  end.
  
update_account(S, A) ->
  case A#account.order of
    undefined ->
      nothing_to_update,
      A;
    O ->
      TableName = S#fx_state.table_name,
      Index = S#fx_state.index,
      Row = lookup(TableName, Index),
      Close = Row#technical.close,
      Balance = A#account.balance,
      Position = O#order.position,
      Entry = O#order.entry,
      Units = O#order.units,
      Change = Close - Entry,
      PercentageChange = (Change / Entry) * 100,
      Profit = Position * Change * Units,
      UnrealizedPL = Profit,
      NetAssetValue = Balance + UnrealizedPL,
      UO = O#order{
        current = Close,
        change = Change,
        percentage_change = PercentageChange,
        profit = Profit
      },
      UA = A#account{
        unrealized_pl = UnrealizedPL,
        net_asset_value = NetAssetValue,
        order = UO
      },
      UA
  end.

make_trade(S, A, Action) ->
  case A#account.order of
    undefined ->
      case Action == 0 of
        true ->
          A;
        false ->
          open_order(S,A,Action)
      end;
    O ->
      case Action == 0 of
        true ->
          close_order(A);
        false ->
          CurrentPosition = O#order.position,
          case CurrentPosition == Action of
            true ->
              A;
            false ->
              UA = close_order(A),
              open_order(S, UA, Action)
          end
      end
  end.
  
open_order(S, A, Action) ->
  BuyMoney = 100,
  Spread = A#account.spread,
  Leverage = A#account.leverage,
  TableName = S#fx_state.table_name,
  Index = S#fx_state.index,
  Row = lookup(TableName, Index),
  Quote = Row#technical.close,
  Entry = Quote + Spread * Action,
  Units = round((BuyMoney * Leverage)/Entry),
  Change= Quote - Entry,
  PChange = (Change / Entry) * 100,
  Profit = Action * Change * Units,
  UnrealizedPL = Profit,
  NewOrder = #order{
    pair = TableName,
    position = Action,
    entry = Entry,
    current = Quote,
    units = Units,
    change = Change,
    percentage_change = PChange,
    profit = Profit
  },
  UA = A#account{
    unrealized_pl = UnrealizedPL,
    order = NewOrder
  },
  UA.

close_order(A) ->
  UBalance = A#account.balance + A#account.unrealized_pl,
  URealizedPL = A#account.realized_pl + A#account.unrealized_pl,
  UA = A#account{
    balance = UBalance,
    realized_pl = URealizedPL,
    unrealized_pl = 0.0,
    order = undefined
  },
  UA.

%%-----------------------------------------------------------------------------
%% FX Sensors
%%-----------------------------------------------------------------------------

list_encoded(HRes, S) ->
  Index = S#fx_state.index,
  CurrencyPair = S#fx_state.table_name,
  PriceListPs = S#fx_state.price_list,
  {UPriceListPs, UPList} = case lists:keyfind(HRes, 2, PriceListPs) of
    false ->
      TrailingIndex = prev(CurrencyPair, Index, prev, HRes - 1),
      UPL = fx_get_price_list(CurrencyPair, TrailingIndex, HRes, []),
      {[{UPL, HRes} | PriceListPs], UPL};
    {PList, HRes} ->
      R = lookup(CurrencyPair, Index),
      UPL = [{R#technical.open, R#technical.close, R#technical.high, R#technical.low} |
        lists:sublist(PList, HRes - 1)],
      {lists:keyreplace(HRes, 2, PriceListPs, {UPL, HRes}), UPL}
  end,
  US = S#fx_state{price_list = UPriceListPs},
  {[Close || {_Open, Close, _High, _Low} <- UPList], US}.
  
plane_encoded(HRes, VRes, S) ->
  Index = S#fx_state.index,
  CurrencyPair = S#fx_state.table_name,
  PriceListPs = S#fx_state.price_list,
  {UPriceListPs, UPList} = case lists:keyfind(HRes, 2, PriceListPs) of
    false ->
      TrailingIndex = prev(CurrencyPair, Index, prev, HRes - 1),
      UPL = fx_get_price_list(CurrencyPair, TrailingIndex, HRes, []),
      {[{UPL, HRes} | PriceListPs], UPL};
    {PList, HRes} ->
      R = lookup(CurrencyPair, Index),
      UPL = [{R#technical.open, R#technical.close, R#technical.high, R#technical.low} |
        lists:sublist(PList, HRes - 1)],
      {lists:keyreplace(HRes, 2, PriceListPs, {UPL, HRes}), UPL}
  end,
  LVMax1 = lists:max([High || {_Open, _Close, High, _Low} <- UPList]),
  LVMin1 = lists:min([Low || {_Open, _Close, _High, Low} <- UPList]),
  LVMax = LVMax1 + abs(LVMax1 - LVMin1) / 20,
  LVMin = LVMin1 - abs(LVMax1 - LVMin1) / 20,
  VStep = (LVMax - LVMin) / VRes,
  VStartPos = LVMin + VStep / 2,
  US = S#fx_state{price_list = UPriceListPs},
  {l2fx(HRes * VRes, {UPList, UPList}, VStartPos, VStep, []), US}.
  
fx_get_price_list(_Table, _EndKey, 0, Acc) ->
  Acc;
fx_get_price_list(_Table, 'end_of_table', _Index, _Acc) ->
  exit("fx_get_price_list, reached end_of_table");
fx_get_price_list(Table, Key, Index, Acc) ->
  R = lookup(Table, Key),
  fx_get_price_list(Table, next(Table, Key), Index - 1, [{R#technical.open, R#technical.close,
    R#technical.high, R#technical.low} | Acc]).
    
l2fx(Index, {[{Open, Close, High, Low} | VList], MemList}, VPos, VStep, Acc) ->
  {BHigh, BLow} = case Open > Close of
    true ->
      {Open, Close};
    false ->
      {Close, Open}
  end,
  O = case (VPos + VStep / 2 > BLow) and (VPos - VStep / 2 =< BHigh) of
    true ->
      1;
    false ->
      case (VPos + VStep / 2 > Low) and (VPos - VStep / 2 =< High) of
        true ->
          0;
        false ->
          -1
      end
  end,
  l2fx(Index -1, {VList, MemList}, VPos, VStep, [O | Acc]);
l2fx(0, {[], _MemList}, _VPos, _VStep, Acc) ->
  Acc;
l2fx(Index, {[], MemList}, VPos, VStep, Acc) ->
  l2fx(Index, {MemList, MemList}, VPos + VStep, VStep, Acc).

%%-----------------------------------------------------------------------------
%% Table Commands
%%-----------------------------------------------------------------------------

init_table(metadata) ->
  case ets:whereis(metadata) of
    undefined ->
      exit("Table not found - 1");
    _Tid ->
      ok
  end;
init_table(TableName) ->
  case ets:whereis(TableName) of
    undefined ->
      exit("Table not found - 2");
    _Table ->
      [insert(metadata, #metadata{feature = {TableName, Feature}}) || Feature <- ?FEATURES],
      case insert_forex_raw(filename:join([?FX_TABLES_DIR, (atom_to_list(TableName) ++ ".txt")])) of
        done ->
          ok;
        {error, _Error} ->
          exit("Failed to insert raw forex data.~n")
      end
  end.

lookup(TableName, Key) ->
  [R] = ets:lookup(TableName, Key),
  R.
  
insert(TableName, Record) ->
  ets:insert(TableName, Record).

next(TableName, Key) ->
  ets:next(TableName, Key).

prev(TableName, 'end_of_table', prev, _Index) ->
  ets:first(TableName);
prev(_TableName, Key, prev, 0) ->
  Key;
prev(TableName, Key, prev, Index) ->
  prev(TableName, ets:prev(TableName, Key), prev, Index - 1).

member(TableName, Key) ->
  ets:member(TableName, Key).

%%-----------------------------------------------------------------------------
%% Forex Data Insertion
%%-----------------------------------------------------------------------------

%%-----------------------------------------------------------------------------
%% @private
%% @doc Description:
%%        -record(forex_raw, {key, open, high, low, close, volume}).
%%          key = {currency_pair, Year, Month, Day, Hour, Minute, Second}
%%        Input: textfile/cvsfile
%%        date / time / open / high / low / close / volume
%%        2009.05.15,00:00,0.88880,0.89060,0.88880,0.88950,362
%%        URL = Dir/File
%%        File = FileName.FileExtension, FileName = [CPair][TimeFrame]
%% @end
%%-----------------------------------------------------------------------------
insert_forex_raw(URL) ->
  {_Dir, File} = extract_dir(URL),
  {FileName, _FileExtension} = extract_filename(File),
  {CurrencyPair, TimeFrame} = extract_cpair(FileName),
  TableName = CurrencyPair ++ TimeFrame,
  case lists:member(TableName, [atom_to_list(TN) || TN <- ?ALL_TABLES]) of
    true ->
      case file:read_file(URL) of
        {ok, Data} ->
          List = binary_to_list(Data),
          update_forex_db(list_to_atom(TableName), CurrencyPair, list_to_integer(TimeFrame), List);
        {error, _Error} ->
          {error, cant_read}
      end;
    false ->
      logr:error({scape, insert_forex_raw, error, "table_unknown, file rejected",
        [TableName]}),
      {error, table_unknown}
  end.
  
extract_dir(URL) ->
  extract_dir(URL, []).

extract_dir(List, DirAcc) ->
  case split_with(47, List) of % 47 == '/'
    {File, []} ->
      Dir = lists:concat(DirAcc),
      {Dir, File};
    {DirPart, Remainder} ->
      extract_dir(Remainder, lists:merge([DirPart, '/'], DirAcc))
  end.
  
extract_filename(File) ->
  split_with(46, File, []).   % . 46
    
extract_cpair(FileName) ->
  lists:split(6, FileName).

update_forex_db(_TableName, _CurrencyPair, _SamplingRate, []) ->
  done;
update_forex_db(TableName, CurrencyPair, SamplingRate, List) ->
  {YearL, Remainder1} = split_with(46, List),   % . 46
  {MonthL, Remainder2} = split_with(46, Remainder1),  % . 46
  {DayL, Remainder3} = split_with(44, Remainder2),    % , 44
  {HourL, Remainder4} = split_with(58, Remainder3),   % : 58
  {MinuteL, Remainder6} = split_with(44, Remainder4), % : 58
  {OpenL, Remainder7} = split_with(44, Remainder6),   % , 44
  {HighL, Remainder8} = split_with(44, Remainder7),   % , 44
  {LowL, Remainder9} = split_with(44, Remainder8),    % , 44
  {CloseL, Remainder10} = split_with(44, Remainder9), % , 44
  {VolumeL, Remainder11} = split_with(13, Remainder10), %\r 13
  [_ | Remainder] = Remainder11,        % gets rid of (\n 10)
  Year = list_to_integer(YearL),
  Month = list_to_integer(MonthL),
  Day = list_to_integer(DayL),
  Hour = list_to_integer(HourL),
  Minute = list_to_integer(MinuteL),
  Second = 0,
  Open = list_to_number(OpenL),
  High = list_to_number(HighL),
  Low = list_to_number(LowL),
  Close = list_to_number(CloseL),
  Volume = list_to_integer(VolumeL),
  Id = {Year, Month, Day, Hour, Minute, 0, SamplingRate},
  case (Second == 0) and ((Open+High+Low+Close) < 1000) and ((Open+High+Low+Close) > -1000) of
    true ->
      case member(TableName, Id) of
        false ->
          Record = #technical{
            id = Id,
            open = Open,
            high = High,
            low = Low,
            close = Close,
            volume = Volume
          },
          insert(TableName, Record),
          done;
        true ->
          done
      end;
    false ->
      done
  end,      
  update_forex_db(TableName, CurrencyPair, SamplingRate, Remainder).
  
split_with(Seperator, List) ->
  split_with(Seperator, List, []).

split_with(Seperator, [Char | List], ValAcc) ->
  case Char of
    Seperator->
      {lists:reverse(ValAcc), List};
    _ ->
      split_with(Seperator, List, [Char | ValAcc])
  end;
split_with(_Seperator, [], ValAcc) ->
  {lists:reverse(ValAcc), []}.
      
list_to_number(List) ->
  try list_to_float(List) of
    Float ->
      Float
  catch 
    _:_ ->
      list_to_integer(List)
  end.