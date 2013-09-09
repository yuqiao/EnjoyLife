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

6.3 运行程序的不同方法
----------------------
1. 在Erlang shell中编译运行::

    $ erl
    1> c(hello).
    {ok,hello}

2. 在命令提示符下编译运行::

    $ erlc hello.erl
    $ erl -noshell -pa ~/souce/life/docs/Erlang/src -s helo start -s init stop

3. 把程序当做escript脚步运行, 创建这样的文件::

    #!/usr/bin/env escript
    main(_) ->
        io:format("Hello world\n").

4. 用命令行参数编程,需要修改文件, 例如fac1.erl::


    -module(fac1).
    -export([main/1]).

    main([A]) ->
        I = list_to_integer(atom_to_list(A)),
        F = fac(I),
        io:format("factorial ~w = ~w~n",[I,F,]),
        init:stop().

    fac(0) -> 1;
    fac(N) -> N*fac(N-1).

    -----------------------------
    编译运行它：
    $ erlc fac1.erl
    $ erl -noshell -s fac1 main 25

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
有时很难停止运行中的Erlang, 有以下几种原因:

- shell没有响应.
- Ctrl+C处理程序被禁止.
- Erlang启动时带有-detached选项，你可能难以察觉它运行.
- Erlang启动时带有-heart Cmd选项。这个选项会启动一个操作系统监视进程来监视系统中的Erlang进程，若Erlang进程死亡，监视进程会执行Cmd.通常Cmd会重庆Erlang系统。 应付这种情况，就是先杀掉心跳进程。
- 可能发生严重错误，导致一个Erlang僵尸进程被遗留在操作系统中。

6.7 如何应付故障
-------------------
1. 未定义/遗失代码。碰到一个undef的错误消息:

   - 系统不存在该模块
   - 系统存在该模块，但没有编译。
   - 存放模块beam的目录不在搜索路径下
   - 选择了模块的一个错误版本

2. Makefile不能工作

3. shell没有响应：

    可以按下Ctrl+G来中断当前shell。

6.8 获取帮助
---------------
- erl -man erl
- erl -man lists

6.9 调试环境
----------------
Erlang shell有一批内建命令，可以通过help()看到它们。

这些命令都放在模块shell_default中。

可以定义自己的命令，只需要创建一个叫user_default的模块，放在加载路径下::

    -module(user_default).
    -compile(export_all).

    hello() ->
        "Hello Joe how are you?"

    away(Time) -> 
        io:format("Joe is away and will be back in ~w mnutes ~n", [Time])

6.10 崩溃转储
----------------
Erlang有一个基于web的崩溃分析器. 启动::

    1> webtool:start().

访问http://localhost:8888/,就可以研究错误日志文件了。

7 并发
=========


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
9.1 链接程序
----------------
如果一个进程在某种程度上依赖于另一个进程，那么它就需要时刻紧盯第二个进程的运行状态.
可使用BIF link, 或者使用监视器。

当一个进程接收到退出信号：

- 默认是让该进程一并退出。
- 该进程捕获退出信号，该进程又被称为系统进程。

9.2 on_exit处理程序
----------------------
当进程退出时，执行一些动作, 编写程序on_exit(Pid, Fun), 会创建一个指向Pid进程的链接::

    on_exit(Pid,Fun) ->
        spawn( fun() ->
                        process_flag(trap_exit, true), %% 把创建的进程转换为一个系统进程
                        link(Pid),
                        receive
                            {'EXIT', Pid, Why} ->
                                Fun(Why)
                        end
               end).

9.3 远程错误处理
------------------

9.4 错误处理的细节
-------------------
Erlang错误处理的3种基础概念:

- **链接(link)** 。定义了一种在两个进程之间的传播路径， 若一个进程消亡，就会向另一个进程发送一个退出信号。
- **退出信号(exit signal)** 。进程消亡时，会产生一个叫做"退出信号"的东西。系统会向这个濒死进程的链接集的所有进程发送退出信号。
- **系统进程(system process)** . 当进程接收一个非正常的退出信号它自己会退出，除非他是"系统进程"

若把退出原因设为kill, 进程就会发送一个无法捕获的退出信号，无论是什么进程哪怕是系统进程，都会被终止。
OTP中的监管进程就是使用这种方式来终止僵尸进程的。

捕获退出的编程模式:

1. 不在乎创建的进程是否崩溃::

    Pid = spawn(fun() -> ... end)

2. 若我创建的进程崩溃我也执行退出::

    Pid = spawn_link(fun() -> ... end)

3. 若我创建的进程崩溃我需要处理错误::

    ...
    process_flag(trap_exit, true),
    Pid = spawn_link(fun() -> ... end),
    ...
    loop(...).

    loop(State) ->
        receive 
            {'EXIT', SomePid, Reason} ->
                %% do something with the error
                loop(State1);
            ....
        end

9.5 错误处理原语
------------------
- spawn_link(Fun) -> Pid
- process_flag(trap_exit, true)
- link(Pid) -> true
- unlink(Pi) -> true
- exit(Why) -> none()
- exit(Pid,Why) -> none()
  erlang:monitor(process, true) -> MonitorRef. 建立一个监视器，Item为Pid或进程的注册名。

如何构建一个容错系统:

    至少需要两台计算机，一台机器运行日常工作，由另外一台计算机监视第一台计算机并时刻准备在它崩溃时候接管工作。
    称为："工人-监工"模型。整个OTP库都构建于监控树的概念之上，而监控树真是基于这种思想来构建的。
    Erlang的整个容错特性及其体系从根本上依赖于link原语.
    弄清link机制，并学会如何在两台计算机之间互相访问，那么第一个容错系统就已经是万事俱备只欠东风了。

9.6 链接进程集
-----------------

9.7 监视器
----------
链接是对称的，而监视器是一个非对称的链接。

9.8 存活进程
---------------
永远存活的进程-- 无论因为什么原因消亡，都会被立即重启 ::

    keep_alive(Name, Fun) ->
        register(Name, Pid =spawn(Fun) ),
        on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end ).

这其中有个微妙错误： 进程有可能在on_exit之前死亡。

10 分布式编程
===============
分布式编程是针对网络上仅通过消息传递来完成互相协作的计算机集群所设计的程序。需求:

- 效率
- 可靠性
- 可伸缩性
- 天生需要分布式的应用程序
- 乐趣

分成以下几个步骤:

1. 现在一个非分布式Erlang环境中编写和测试程序。
#. 然后会在同一台机器上的两个不同节点上测试程序。
#. 最好在同一个网络或者因特网上的两台互相独立的集群上开启不同Erlang节点来测试程序。

10.2 分布式原语
-----------------
分布式Erland的核心概念是节点，一个节点就是一个自给自足的系统，他是一个包含地址空间和独立进程集的完整虚拟机。

- spawn(Node, Fun) -> Pid.
- spawn(Node, Mod, Func, ArgList) -> Pid
- spawn_link
- disconnect_node(Node)
- monitor_node(Node, Flag)
- node()/node(Arg)
- nodes()
- is_alive
- {RegName, Node} ! Msg

examples::

    -module(dist_demo).
    -export([rpc/4,start/1]).

    start(Node) ->
        spawn(Node, fun() -> loop() end).

    rpc(Pid, M, F, A) ->
        Pid ! {rpc, self(), M, F, A},
        receive
            {Pid, Response} ->
                Response
        end.

    loop() ->
        receive
            {rpc, Pid, M,F,A} ->
                Pid ! {self(), (catch apply(M,F,A))},
                loop()
        end.

11 IRC Lite
================
5个组成部分:

- 用户界面: 用于将接收到的信息显示出来的GUI窗口组件。
- 聊天客户端: 负责管理消息。
- 群管理器: 负责管理单个聊天组。若群控制器接收打一个消息，它会将这条消息广播到组中所有成员。
- 聊天服务器：负责持续跟踪所有群组控制器。
- 中间人：负责管理系统之间的数据传输。
        
11.1 消息序列图
---------------------

11.2 用户界面
--------------
提供接口:

- io_widget:start(Pid) -> Widget
- io_widget:set_title(Widget, Str)
- io_widget:set_state(Widget, State)
- io_widget:insert_str(Widget, Str)
- io_widget:set_handler(Widget, Fun)

窗口会产生消息： 
- {Widget, State, Parse}
- {Widget,destroyed}

11.3 客户端程序
-----------------
有3个进程： 用户界面， 聊天客户端， 中间人进程。



12 接口技术
============
Erlang的运行时系统通过二进制的通信通道与外部程序交互。

Erlang端有一个Erlang端口(端口连接进程)负责管理这样的通信。
所有向外部程序发送的消息的目标地址必须是端口连接的PID，所有从外部程序传入的消息都直接送入端口连接进程。

12.1 端口
-------------
创建一个端口::

    Port = open_port(PortName, PortSettings)
    Port ! {PidC,{command,Data}}  %%
    Port ! {PidC, {connect, Pid1}}
    Port ! {PidC, close}

通过下面的方法接收来自外部程序的消息::

    receive
        {Port, {data,Data}} ->
            ... Data comes from external process ...

12.2 为一个外部C程序添加接口
------------------------------

13 对文件编程
=================
13.1 库的组织结构
------------------
4个模块:
- file模块。包含文件打开，关闭，读取，写入和目录列表等功能的函数。
- filename模块。以平台独立的形式提供了一套操作文件的函数。
- filelib模块， file模块的扩展，提供一套辅助函数用于生成文件列表，检验文件类型等操作。
- io模块，提供一系列对已打开的文件进行操作的函数。

13.2 读取文件的不同方法
-----------------------
data1.dat 原始数据::

    {person, "joe", "armstrong",
        [{occupation, programmer},
         {favoriateLanguage, erlang}]}.

    {cat, {name, "zorro"},
          {owner,"joe"}}.

1. 从文件中读取所有Erlang数据项::

    file:consult("data1.dat"). %% 返回{ok,[Term]}; 失败返回{error, Reason}.

#. 从文件的数据项中一次读取一项::

    {ok, S} = file:open("data1.dat", read).
    io:read(S,'').
    io:read(S,'').
    file:close(S).

#. 从文件中一次读取一行数据::

    {ok, S} = file:open("data1.dat", read).
    io:get_line(S,'').

#. 将整个文件的内容读入到一个二进制数据中::

    file:read_file("data1.dat").

#. 随机读取一个文件::

    {ok, S} = file:open("data1.dat", read).
    file:pread(S,22,46).
    file:pread(S,1,10).

    




