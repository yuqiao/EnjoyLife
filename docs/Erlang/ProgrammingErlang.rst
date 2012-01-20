============================
Erlang程序设计
============================

:author: yuqiao20@gmail.com
:version: 0.0.1
:Date:  12-01-19 17:57:09
         

.. contents::

2 入门
=======
2.1 学习阶段
------------------
1. 茫然无绪::

    学到如何启动系统，在shell里运行命令，边缘简单的程序。

2. 初窥门径::

    具备初步的知识来运用这个语言。

3. 观其大略，不求甚解::

    通读本书，不必苛求每一章能学得面面俱到。
    Erlang编程总是与一种编程范式形影不离：COP(面向并发编程）
    使用COP，可以分解问题，识别出它本身的并发模式。

4. 运用自如::
   
    灵活运用这一语言，编写一些有用的分布式程序。
    - Mnesia.
    - 如何与其它语言编写的代码镜像接口，如何使用内联驱动
    - 利用各种OTP行为来监控树（supervision tree)和启动脚步（start script)..
    - 如何在多核计算机上运行和优化你程序。

5. 重中之重: **编程乐在其中**

3 顺序型编程
===================
3.1 模块
------------
编辑模块文件geometry.erl::
    
    -module(geometry).
    -export([area/1]).
    area({rectangle,Width,Ht}) -> Width * Ht;
    area({circle, R})          -> 3.14159 * R * R.

在erl shell中， 编译：
    1> c(geometry).

标点符号使用:

- 逗号(,), 用于分割函数调用，数据构造器已经模式中的参数。
- 句号(.), 用于在shell中分割完整的函数或表达式。
- 分号(;), 用于分割子句，用到子句的地方：

    - 分段的函数定义
    - case语句
    - if语句
    - try ... catch语句
    - receive表达式

函数的目(arity)
    函数拥有的参数


3.4 fun
----------
匿名函数::

    Z = fun(X) -> 2*X end.
    Double = Z.

Erlang是一种函数式编程语言，除了极个别情况外，fun既可以作为函数的参数，也可以作为函数的结果。

高价函数(high-order function):
    能够返回fun或接受fun作为参数的函数.


3.4.1 以fun为参数的函数
~~~~~~~~~~~~~~~~~~~~~~~~~~~
list是标准库中一个模块，其中很多函数是以fun作为参数::

    L = [1,2,3,4].
    lists:map(Double,L).
    Even = fun(X) -> (X rem 2) =:= 0 end.
    lists:filter(Even, [1,2,3,4,5,6,7]).

3.4.2 返回fun的函数
~~~~~~~~~~~~~~~~~~~~~~~~~~~
for example::

    Fruit = [apple,pear,orange].
    MakeTest = fun(L) -> ( fun(X) -> lists:member(X,L) end) end.
    IsFruit = MakeTest(Fruit).
    IsFruit( pear ).
    lists:filter(IsFruit, [dog, orange, cat, pear]).

3.4.3 定义你自己的抽象流程控制
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for example::

    for(Max,Max,F) -> [F(Max)];
    for(I,Max)     -> [F(I)|for(I+1, Max, F) ].

3.5 简单的列表处理
--------------------
sum 实现::

    sum([H|T])  -> H + sum(T);
    sum([])     -> 0.

map 实现::
    
    map(_,[])   -> [];
    map(F,[H|T])-> [F(H) | map(F, T)].

如何写程序:

    写一点测试一点，从一个没多少函数的小模块开始，然后在shell中用几个命令编译和测试它们。
    得到测试令我满意，才会继续写其它函数，再对新代码编译测试，整个过程是这样展开的。

    通常，不会草率地决定程序需要什么样的数据结构。在运行简单例子的同时，会不断审视之前
    选择的数据结构是否合理。

    倾向于循序渐进地扩展代码，而不是在动手之前就已经完全构思出来。


3.6 列表解析
-------------
假设，有一列表::

    L = [1,2,3,4].

对每个元素加倍，方法一::

    lists:map(fun(X) -> 2*X end, L ).

列表解析::

    [ 2 * X || X <- L].

列表解析的生成器部分也可以像过滤器一样工作::

    [ X || {a, X} <- [{a,1},{b,2},{c,3},{a,4},hello,'wow'] ].

快速排序::

    qsort( [] ) -> [];
    qsort( [Pivot|T] ) -> 
            qsort([X || X <- T, X < Pivot] )
            ++ [Pivot] ++
            qsort([X || X <- T, X >= Pivot]).

毕达哥拉斯三元组::

    pythag(N) ->
        [ {A,B,C} ||
            A <- lists:seq(1,N),
            B <- lists:seq(1,N),
            C <- lists:seq(1,N),
            A+B+C =< N,
            A*A+B*B =:= C*C
        ].

变位词::

    perms([]) -> [[]];
    perms[L)  -> [ [H|T] || H <- L, T<-perms( L -- [H] )].

3.7 算术表达式
---------------

3.8 断言( guard )
-----------------
guard是一种用于强化模式匹配的结构。
for exanmple::

    max(X,Y) when X>Y ->X;
    max(X,Y)          ->Y.

断言序列：
    以分号(;)分隔断言集合，表示or逻辑。以逗号(,)分隔表示and逻辑。

断言谓词：

- is_atom(X)
- is_binary(X)
- is_constant(X)
- is_float(X)
- is_function(X)
- is_function(X,N)
- is_integer(X)
- is_list(X)
- is_number(X)
- is_pid(X)
- is_port(X)
- is_reference(X)
- is_tuple(X)
- is_record(X,Tag,N)
- is_record(X,Tag)

断言BIF(build-in function):

- abs(X)
- element(N,X)
- float(X)
- hd(X)
- length(X)
- node()
- node(X)
- round(X)
- self()
- size(X)
- trunc(X)
- tl(X)

断言样例::

    f(X,Y) when is_integer(X), X>Y, Y < 6 -> ...


3.9 记录
---------
记录(record)提供一种方法把一个名称与元祖中的一个元素对应起来。

::
    -record(Name, {
                    %% the next two keys have default values,
                    key1 = Default1,
                    key2 = Default2,
                    key3,
                    ...
                  }).

例子("records.hrl")::

    -record(todo, {status=reminder, who=joe, text}).

读取记录的定义::

    rr("records.hrl").

创建和更新记录::

    X = #todo{}.
    X1 = #todo{status=urgent, text="Fix errata in book"}.
    X2 = X1#todo{status=done}.

从记录中提取字段值::

    #todo{who=W, text=Txt} = X2.
    X2#todo.text.

在函数中对记录进行模式匹配::

    clear_status(#todo{status=S, who=W} = R) ->
        %% Inside theses function S and W are bound to the field
        %% values in the record.
        %% 
        %% R is the *entire* record
        R#todo{status=finished}.
        %% ...

    do_something(X) when is_record(X, todo) -》
        %% ...

记录只是元祖的伪装, 用rf/1可以释放todo的定义::

    X2.
    rf(todo).
    X2.


3.10 case/if表达式
----------------------
case 语法::

    case Expression of
        Pattern1 [when Guard1] -> Expr_seq1;
        Pattern2 [when Guard2] -> Expr_seq2;
        ...
    end

if 语法::

    if
        Guard1 ->
            Expr_seq1;
        Guard2 ->
            Expr_seq2;
        ...
    end

3.11 以自然顺序创建列表
----------------------------------------
1. 总是在列表头部添加元素。
2. 从一个输入列表的头部提取元素，然后把它们加在一个输出列表的头部。
3. 若顺序至关重要，那么调用经过高度优化的lists:reverse/1.
   
避免使用：List ++ [H] , 极为低效的操作。

3.12 累加器
----------------------------------------
看 lib_misc_

.. _lib_misc: lib_misc.erl

4 异常
============
4.2 抛出异常
----------------
- exit(Why). 系统向所有与当前经常相连接的进程广播{'EXIT',Pid,Why}消息。
- throw(Why).
- erlang:error(Why).

4.3 try ... catch
-------------------
语法::

    try FuncOrExpressionSequece of
        Pattern1 [when Guard1] -> Expression1;
        Pattern2 [when Guard2] -> Expression2;
        ...
    catch
        ExceptionType: ExPattern1 [when ExGuard1] -> ExExpressions1;
        ExceptionType: ExPattern2 [when ExGuard2] -> ExExpressions2;
        ...
    after
        AfterExpressions
    end

缩减版本::

    try F
    catch
        ...
    end

try ... cateh的编程惯例. 

4.4 catch
---------
使用catch原语，当你捕获一个异常时这个异常会转化为描述错误的一个元祖。

4.5 改进错误信息
----------------------
erlang：error的另一个用处是提高错误信息的质量。

4.6 try ... catch 的编程风格
-------------------------------

5 顺序型编程进阶
================
5.1 BIF
-----------
BIF(内建函数):

1. tuple_to_list
2. time

5.2 二进制数据
------------------
比特语法::

    <<>>
    <<E1, E2, ...,En>>

每个元素Ei代表一个单独区块， 有以下几种形式::

    Ei = Value |
         Value:Size |
         Value/TypeSpecifierList | 
         Value:Size/TypeSpecifierList

    
<<5,10,20>>

6 编译并运行程序
====================
6.2 配置开发环境
-----------------
获取当前加载路径::

    code:get_path().

操作加载路径::

    code:add_patha(Dir).
    code:add_pathz(Dir).

查看::
    
    code:all_loaded().
    code:clash().

或者::

    erl -pa Dir1 Dir2 ... -pz DirK1 -pz DirK2


6.4 makefile
------------
makefile模板::

    .SUFFIXES: .erl .beam

    .erl.beam:
        erlc -W $<

    ERL = erl -boot start_clean

    MODS = hello

    all: compile
        ${ERL} -pa '~/source/life/docs/Erlang' -s hello start

    compile: ${MODS:%=%.beam}

    clean:
        rm -rf *.beam erl_crash.dump

6.6 解决系统死锁
-------------------

8 并发编程
=============
8.1 并发原语
--------------
- Pid = spawn(Fun)
- Pid ! Message
- Pid1!Pid2!..!M
- receive ... end::

    receive

        Pattern1 [when Guard1] ->
            Expression1;
        Pattern2 [when Guard2] ->
            Expression2;
        ...
    after Time ->
        Expressions
    end

8.5 带超时的receive
sleep(T)::

    sleep(T) ->
        receive
        after T ->
            true
        end.

计数器::

    -module(stimer).
    -export([start/2, cancel/1]).

    start(Time, Fun) -> spawn(fun() -> timer(Time, Fun) end).

    cancel(Pid) -> Pid ! cancel.

    timer(Time, Fun) ->
        receive
            cancel ->
                void
        after Time ->
            Fun()
        end.

8.6 选择性接收
---------------
每一个进程都有与之相应的邮箱。当想进程放送消息时， 消息就送入邮箱之中。

receive的内部工作机制:

1. 进入一个receive语句时，就启动一个定时器（只在表达式中有after语句）.
#. 从邮箱中取出第一个消息时， 然后尝试对Pattern1,pattern2等进行模式匹配。若匹配程，消息就从邮箱中上次，对应模式之后的表达式就会被求值。
#. 若邮箱中的第一个消息不能匹配receive语句的任何一个模式，那么就会将第一个消息从邮箱中删除并送入一个'保存队列', 然后继续尝试邮箱中的第二个消息。这个过程会不断重复，直到找到匹配的消息或者邮箱中的所有消息全都检查完毕。
#. 若邮箱中所有的消息都不能匹配，那么就挂起经常，直到下一次又有新的消息进入邮箱时再对进程进行重新调度执行。
#. 一个消息若被匹配，那么存入保存队列中的所有消息就会按照它们到达进程的时间先后顺序重新放回到邮箱中。
#. 如果在我们等待一个消息时触发了计时器，那么对表达式ExpressionTimout求职后把存入保存队列中的所有消息按照它们到达进程的时间顺序放回邮箱中。


8.7 注册进程
----------------
管理注册进程BIF:

- register(AnAtom, Pid)
- unregister(AnAtom)
- whereis(AnAtom) -> Pid | undefiened
- registered() -> [AnAtom:atom()], 返回系统中所有已经注册的名称列表。


9 并发编程中的错误处理
=======================
