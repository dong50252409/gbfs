gbfs
=====

贪心最佳优先寻路算法，支持 4、8 方向寻路，支持限制最大搜索深度。

A purely Elang implementation of the Greedy Best-First Search algorithm for pathfinding.

构建 Build
-----

    $ rebar3 compile

单元测试 Eunit
----

    $ rebar3 eunit

如何使用 How to use
-----

    1> StartGrid = {1, 1}.
    2> EndGrid = {50, 50}.
    2> BlockList = [{47,1},{24,2}, {2,25}, {20,31}, {20,21}, {50,20}, ...].
    3> ValidFun = fun({X,Y}) -> not lists:member({X,Y}, BlockList) end.
    4> Options = [],
    5> {max, Path} = gbfs:search(StartGrid, EndGrid, ValidFun, Options).
