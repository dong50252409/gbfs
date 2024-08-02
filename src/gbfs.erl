-module(gbfs).

-export([search/4]).

-type grid() :: {integer(), integer()}.
-type result() :: {max, Path :: [grid()]} | none| max_limited.
-type max_limit() :: {max_limit, non_neg_integer()}.
-type direction_num() :: {direction_num, 4|8}.
-type option() :: max_limit()|direction_num().
-type options() :: [option()].
-type valid_fun() :: fun((grid()) -> boolean()).

-define(DIRECTION_NUM, 8).
-define(MAX_LIMIT, 16#FFFF).

-spec search(StartGrid, EndGrid, ValidFun, Options) -> Result when
    StartGrid :: grid(), EndGrid :: grid(),
    ValidFun :: valid_fun(), Options :: options(),
    Result :: result().
search(StartGrid, EndGrid, ValidFun, Options) ->
    OpenGrids = insert(0, {StartGrid, []}, new()),
    VisitedGrids = #{StartGrid => []},
    DirectionNum = proplists:get_value(direction_num, Options, ?DIRECTION_NUM),
    Directions = directions(DirectionNum),
    MaxLimit = proplists:get_value(max_limit, Options, ?MAX_LIMIT),
    do_search(EndGrid, ValidFun, OpenGrids, VisitedGrids, Directions, MaxLimit).
%%======================================
%% Internal Function
%%======================================
do_search(EndGrid, ValidFun, OpenGrids, VisitedGrids, Directions, MaxLimit) when MaxLimit > 0 ->
    case take_min(OpenGrids) of
        {{EndGrid, Path}, _NewOpenGrids} ->
            {max, erlang:tl(lists:reverse([EndGrid | Path]))};
        {{Grid, Path}, NewOpenGrids} ->
            {UpOpenGrids, UpVisitedGrids} = get_neighbours(EndGrid, ValidFun, Grid, [Grid | Path], NewOpenGrids, VisitedGrids, Directions),
            do_search(EndGrid, ValidFun, UpOpenGrids, UpVisitedGrids, Directions, MaxLimit - 1)
    end;
do_search(_, _, _, _, _, _) ->
    max_limited.

get_neighbours({X2, Y2} = EndGrid, ValidFun, {X, Y} = CurGrid, Path, OpenGrids, VisitedGrids, [{DX, DY} | T]) ->
    X1 = X + DX,
    Y1 = Y + DY,
    NGrid = {X1, Y1},
    case not maps:is_key(NGrid, VisitedGrids) andalso ValidFun(NGrid) of
        true ->
            Source = erlang:abs(X1 - X2) + erlang:abs(Y1 - Y2),
            UpOpenGrids = insert(Source, {NGrid, Path}, OpenGrids),
            UpVisitedGrids = VisitedGrids#{NGrid => []},
            get_neighbours(EndGrid, ValidFun, CurGrid, Path, UpOpenGrids, UpVisitedGrids, T);
        false ->
            get_neighbours(EndGrid, ValidFun, CurGrid, Path, OpenGrids, VisitedGrids, T)
    end;
get_neighbours(_, _, _, _, OpenGrids, VisitedGrids, []) ->
    {OpenGrids, VisitedGrids}.
%%======================================
%% pairs_heap implement
%%======================================
new() ->
    {}.

insert(K, V, Heap) ->
    do_merge({K, V, []}, Heap).

take_min({}) ->
    empty;
take_min({_, V, SubHeaps}) ->
    {V, merge_pairs(SubHeaps)}.

do_merge(Heap1, {}) ->
    Heap1;
do_merge({}, Heap2) ->
    Heap2;
do_merge({K1, V1, SubHeap1}, Heap2) when K1 < element(1, Heap2) ->
    {K1, V1, [Heap2 | SubHeap1]};
do_merge(Heap1, {K2, V2, SubHeap2}) ->
    {K2, V2, [Heap1 | SubHeap2]}.

merge_pairs([SH1, SH2 | Rest]) ->
    do_merge(do_merge(SH1, SH2), merge_pairs(Rest));
merge_pairs([SubHeap]) ->
    SubHeap;
merge_pairs([]) ->
    {}.

directions(4) ->
    [{1, 0}, {0, 1}, {-1, 0}, {0, -1}];
directions(8) ->
    [{1, 0}, {0, 1}, {-1, 0}, {0, -1}, {1, 1}, {-1, 1}, {1, -1}, {-1, -1}].


