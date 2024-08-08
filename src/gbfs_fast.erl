-module(gbfs_fast).

-export([search/4]).

-type grid() :: {integer(), integer()}.
-type result() :: {ok, Path :: [grid()]} | none| max_limited.
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
    OpenGrids = new(),
    VisitedGrids = #{StartGrid => []},
    DirectionNum = proplists:get_value(direction_num, Options, ?DIRECTION_NUM),
    DirectionsFun = direction_fun(DirectionNum),
    MaxLimit = proplists:get_value(max_limit, Options, ?MAX_LIMIT),
    do_search(StartGrid, EndGrid, ValidFun, OpenGrids, VisitedGrids, DirectionsFun, [], MaxLimit).
%%======================================
%% Internal Function
%%======================================
do_search({CX, CY} = CurGrid, EndGrid, ValidFun, OpenGrids, VisitedGrids, DirectionsFun, Path, MaxLimit) when MaxLimit > 0 ->
    [{DX, DY} | T] = DirectionsFun(CurGrid, EndGrid),
    case {CX + DX, CY + DY} of
        EndGrid ->
            {ok, lists:reverse([EndGrid | Path])};
        NGrid ->
            UpVisitedGrids = VisitedGrids#{NGrid => []},
            case not maps:is_key(NGrid, VisitedGrids) andalso ValidFun(NGrid) of
                true ->
                    do_search(NGrid, EndGrid, ValidFun, OpenGrids, UpVisitedGrids, DirectionsFun, [NGrid | Path], MaxLimit - 1);
                false ->
                    {NewOpenGrids, NewVisitedGrids} = do_search_2(CurGrid, EndGrid, ValidFun, OpenGrids, UpVisitedGrids, Path, T),
                    case take_min(NewOpenGrids) of
                        {{NGrid1, Path1}, OpenGrids1} ->
                            do_search(NGrid1, EndGrid, ValidFun, OpenGrids1, NewVisitedGrids, DirectionsFun, Path1, MaxLimit - 1);
                        empty ->
                            none
                    end
            end
    end;
do_search(_, _, _, _, _, _, _, _) ->
    max_limited.

do_search_2({CX, CY} = CurGrid, {EndX, EndY} = EndGrid, ValidFun, OpenGrids, VisitedGrids, Path, [{DX, DY} | T]) ->
    X = CX + DX,
    Y = CY + DY,
    NGrid = {X, Y},
    case maps:is_key(NGrid, VisitedGrids) of
        true ->
            do_search_2(CurGrid, EndGrid, ValidFun, OpenGrids, VisitedGrids, Path, T);
        false ->
            UpVisitedGrids = VisitedGrids#{NGrid => []},
            case ValidFun(NGrid) of
                true ->
                    Source = erlang:abs(X - EndX) + erlang:abs(Y - EndY),
                    UpOpenGrids = insert(Source, {NGrid, [NGrid | Path]}, OpenGrids),
                    do_search_2(CurGrid, EndGrid, ValidFun, UpOpenGrids, UpVisitedGrids, Path, T);
                false ->
                    do_search_2(CurGrid, EndGrid, ValidFun, OpenGrids, UpVisitedGrids, Path, T)
            end
    end;
do_search_2(_, _EndGrid, _ValidFun, OpenGrids, VisitedGrids, _Path, []) ->
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

direction_fun(4) ->
    fun quadrilateral_directions/2;
direction_fun(8) ->
    fun octagonal_directions/2.

quadrilateral_directions({X1, Y1}, {X2, Y2}) ->
    if
        X1 > X2 ->
            if 
                Y1 > Y2 -> [{-1, 0}, {1,0}, {0, 1}, {0, -1}];
                true -> [{-1, 0}, {1,0}, {0, -1}, {0, 1}]
            end;
        X1 < X2 ->
            if
                Y1 > Y2 -> [{1, 0}, {-1, 0}, {0, 1}, {0, -1}];
                true -> [{1, 0}, {-1, 0}, {0, -1}, {0, 1}]
            end;
        true ->
            if
                Y1 > Y2 -> [{0, -1}, {0, 1}, {1, 0}, {-1, 0}];
                true -> [{0, 1}, {0, -1}, {1, 0}, {-1, 0}]
            end
    end.

octagonal_directions({X1, Y1}, {X2, Y2}) ->
    if
        X1 > X2 ->
            if
                Y1 > Y2 -> [{-1, -1}, {1, 0}, {0, 1}, {1, -1}, {-1, 1}, {0, -1}, {-1, 0}];
                Y1 < Y2 -> [{-1, 1}, {0, -1}, {1, 0}, {-1, -1}, {1, 1}, {-1, 0}, {0, 1}];
                true -> [{-1, 0}, {1, -1}, {1, 1}, {0, -1}, {0, 1}, {-1, -1}, {-1, 1}]
            end;
        X1 < X2 ->
            if
                Y1 > Y2 -> [{1, -1}, {-1, 0}, {0, 1}, {-1, -1}, {1, 1}, {0, -1}, {1, 0}];
                Y1 < Y2 -> [{1, 1}, {0, -1}, {-1, 0}, {1, -1}, {-1, 1}, {1, 0}, {0, 1}];
                true -> [{1, 0}, {-1, -1}, {-1, 1}, {0, -1}, {0, 1}, {1, -1}, {1, 1}]
            end;
        true ->
            if
                Y1 > Y2 -> [{0, -1}, {-1, 1}, {1, 1}, {-1, 0}, {1, 0}, {-1, -1}, {1, -1}];
                Y1 < Y2 -> [{0, 1}, {-1, -1}, {1, -1}, {-1, 0}, {1, 0}, {-1, 1}, {1, 1}];
                true -> [{0, 0}]
            end
    end.
