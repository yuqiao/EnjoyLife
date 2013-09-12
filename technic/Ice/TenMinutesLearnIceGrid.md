# 十分钟学习IceGrid

翻译自: http://doc.zeroc.com/display/Doc/Teach+Yourself+IceGrid+in+10+Minutes

## 避免端口硬编码

有可能这样写Ice服务端代码::

        // ...
        Ice.ObjectAdapter adapter =    communicator.createObjectAdapterWithEndpoints("MyAdapter", "tcp -p 10000");
        // .

这是最简单直接的方式创建一个object adpater. 不幸的是, 它有以下缺点:

- 服务端将endpoints信息硬编码在源代码中. 后果是如果你由于某种原因, 要迁移服务到其它端口, 你需要重新编译代码.
- 需要人工的管理这些端口信息, 因为不同的服务不能使用同一端口. 如果你有很多服务, 这种管理会很单调乏味.

为了解决这个问题, 你可以传送端口信息给 createObjectAdapterWithEndpoints调用, 例如::


        // ...
        Ice.ObjectAdapter adapter =
            communicator.createObjectAdapterWithEndpoints("MyAdapter", args[0]);
        // ...


使用命令参数将端口端口信息传给程序, 虽然解决了硬编码的问题, 但是它还是有以下缺点:


- Ice已经有内部机制来完成同样的事情.
- 若你使用这种方式,你不能使用IceGrid location和服务激活机制. 

Ice内部机制提供了一个正确的方式::

        // ...
        Ice.ObjectAdapter adapter = communicator.createObjectAdapter("MyAdapter");
        // ...

这里使用createObjectAdpter来替代createObjectAdapterWithEndpoints, 代码并没有指明adpater的端口信息.
Ice运行时刻必须使用其他方式来确定端口信息. createObjectAdpter以以下方式实现:
- 若属性 MyAdapter.Endpoints 没有被设置, 运行时间创建adapter时见不绑定端口. 
- 否则运行时将使用 MyAdapter.Endpoints 作为端口.


我们可以通过命令字来控制服务端的adapter::


    $ java MyServer.Main --Ice.Config=config


假设 MyAdapter.Endpoints 在配置文件中的设置::


.. note::
     
     可以设置ICE_CONFIG环境变量来设置配置文件路径.

使用配置文件, 客户端使用通常方式来创建proxy,只要客户端知道object identity和端口.

## 使用IceGrid位置服务
将object adapter信息移出源码, 我们获取相应的灵活性: 我们很容易地运行服务在不同的端口,而不需要重新编译代码.
但是, 如果有很多服务, 我们还需要人工去管理那些被服务使用的端口. 更有甚者, 客户端需要指明服务的端口, 一旦我们修改服务端的地址和端口,
我也必须更新所有客户端的配置.

位置(Location)服务内建于IceGrid, 提供了一个简洁的解决方案.
IceGrid位置服务允许客户端动态获取服务的当前endpoint. 同样的,服务端也不需要配置端口.
可以运行服务在任一机器并让os选择一个空闲的端口给服务.

在客户端, 位置服务将使用"符号名字"替换具体的endpoint信息. 例如::

     Object1@MyAdapter

这种proxy相当于间接Proxy. 类似DNS查找ip地址.

Ice运行时使用多种方式优化并缓存, 避免间接寻址成为性能瓶颈.
简单概括, 每个客户端只需要连接locator一次: 第一次它绑定到具体的endpoint. 过后, 调用将直接传递给具体的服务.

server一直与位置服务连接, 将保证locator实时更新位置信息.

为了让所有工作正常, 客户端和服务必须使用同一个位置服务, 所以有相同的配置属性Ice.Default.Locator::

        Ice.Default.Locator=IceGrid/Locator:tcp -h registryhost.xyz.com -p 12000

为打开间接绑定, 需要启动位置服务, 可通过运行icegridregistry进程, registry至少需要以下配置(config.registry文件):


        IceGrid.Registry.Client.Endpoints=tcp –p 12000 
        IceGrid.Registry.Server.Endpoints=tcp 
        IceGrid.Registry.Internal.Endpoints=tcp 
        IceGrid.Registry.Data=db/registry 
        IceGrid.Registry.DynamicRegistration=1


- IceGrid.Registry.Client.Endpoints属性确定位置服务的端口. 服务端和客户端的Ice.Default.Locator配置必须和 IceGrid.Registry.Client.Endpoints一致.
- IceGrid.Registry.Client.Endpoints本身必须指定固定端口.对于Server和Internal的Endpoints, 你只需要设置 协议,不需要指定特殊端口.
客户端和服务在运行时会通过locator找到真实的endpoints.
- IceGrid.Registry.Data属性指明注册信息数据库文件夹.
- IceGrid.Registry.DynamicRegistration必须设置为非0. 没有这个设置, 不允许服务注册它们的adapter endpoins,除非它们被显式部署了.

指明这些属性配置后, 我们可以启动registry::

    $ icegridregistry --Ice.Config=config.registry

对于server,只需要设置两个属性,到locator注册服务::    

        MyAdapter.Endpoints=tcp
        MyAdapter.AdapterId=MyAdapter

可以注意到,MyAdapter.Endpoints有所变化,只需要注明协议, 而不需要指明port.必须设定 MyAdapter.AdapterId, 比如::

        MyAdapter.AdapterId=FooBar
        
在这种情况下, 客户端可以这样绑定使用proxy::

        Object1@FooBar
        

<adapter-name>.AdpterId属性控制了三件事情:
- 告诉服务端运行时使用locator注册adapter 
- 设置了ID
- 提供间接proxies

第二个关键点:它允许两个不同的服务使用相同的adapter名字,譬如MyAdapter, 但并不会找出名字冲突: 设置不同adapterId即可.     


根据这两个属性设置, 一旦启动服务, 服务将联系registry并告知MyAdapter的endpoint细节. 当客户端使用proxy例如Object1@MyAdapter, 它将绑定到这个服务,
不考虑服务允许的地址和端口. 你可以设置Ice.Trace.Locator=2属性在客户端, 这样可以显示间接proxies绑定幕后活动.


只需要对demo/Ice/hello做一点修改, 就可以看到这种机制的活动. 这个demo是使用直接绑定. 
改成间接绑定, 需要做以下事情:
- 服务端,修改 Hello.Endpoints属性.
     原来是: tcp -p 10000:udp -p 10000:ssl -p 10001
   改动为: tcp:udp:ssl
- 服务端添加AdapterId设置: Hello.AdapterId=HelloAdapter
- 客户端修改 Hello.Proxy属性:
  原来是: hello:tcp -p 10000:udp -p 10000:ssl -p 10001 
    改动为: hello@HelloAdapter
- 在客户端和服务端都设置属性: Ice.Default.Locator 
   




自动激活服务
================
使用刚讨论的配置, 我们可以启动某一服务,通过使用icegrid,间接绑定endpoint信息. 而不需要自己人工管理机器名和端口号.
但是,这些工作都依赖于服务已经运行. 一般来说,这不是一个问题:当机器启动,就让服务启动(通过配置/etc/rc.d或者windows注册表).
但,以上方式还是存在以下几个缺点:
- 维护服务的初始化脚本和注册, 随着服务数量增多, 就会变得乏味.
- 服务会耗尽系统资源, 即使它处于空闲状态.
- 服务可能会崩溃或发生故障.


第二个缺点并不严重, 服务空闲时占用的进程号,一些文件描述符以及swap空间,都是够用的.
第三个缺点就需要引起重视了:
-  服务本身可能有问题.
- 系统swap空间耗尽, 导致内存分配出错.
这些都依赖于人工重启. 

Icegrid提供了一种方式: 根据需要激活服务, 但有客户端第一次调用时激活.

服务激活由IceGrid节点负责. 你必须运行IceGrid节点在每一台你需要激活服务的机器撒谎那个.
并且, 你必须运行单一的IceGrid registry.
IceGrid节点负责: 在相应机器上激活服务; 监控服务状态; 汇报服务状态给registry.

通常, 你会将icegrid gegistry和某一台icegrid节点的机器部署在一起.
通过设置IceGrid.Node.CollacateRegistry属性, IceGrid允许将registry和node放入同一个进程. 
IceGrid节点比registry属性多出一些配置属性. 例如(文件confgi.icegrid)::

        Registry configuration (as before)
        IceGrid.Registry.Client.Endpoints=tcp -p 12000
        IceGrid.Registry.Server.Endpoints=tcp
        IceGrid.Registry.Internal.Endpoints=tcp
        IceGrid.Registry.Data=db/registry

        # Only required if you want servers to register
        # themselves without explicit deployment. If all
        # servers are deployed explicitly, this property
        # can be left unset.
        IceGrid.Registry.DynamicRegistration=1

        # Node configuration
        IceGrid.Node.CollocateRegistry=1
        IceGrid.Node.Name=node1
        IceGrid.Node.Endpoints=tcp
        IceGrid.Node.Data=db/node

        # Set the default locator so the node and admin
        # tools can find the registry.
        Ice.Default.Locator=IceGrid/Locator:tcp -h registryhost.xyz.com -p 12000


- IceGrid.Node.CollocateRegistry 设置指明了节点同时设置为registry.
- IceGrid.Node.Name属性指明节点名字.IceGrid.Node.Data指明节点存放服务信息的文件路径.

启动IceGrid节点(包含了registry)::     

    $ icegridnode --Ice.Config=config.icegrid

在客户端, 没有任何变化. 为了使得服务自动激活, 我们必须做以下两个更改:
- 更新服务的配置.
- 部署服务.

第一个更改: server除了Ice.Default.Locator不需求其它配置. 服务相关的配置将从server转移到服务的部署.跌二个更改: 为了按需激活服务. 我们需要告知具体服务的IceGrid.以下信息IceGrid需要知道:- 应用名字- node名字- server的id- 服务对应可执行文件的路径- 服务对应的adapter名字.- 服务使用的协议.
IceGrid需要的这些信息放在部署描述xml文件中. 例如::


demo.xml
<icegrid>  <application name="demo">    <node name="node1">      <server id="DemoServer" exe="/usr/bin/demoserver" activation="on-demand">        <adapter name="MyAdapter" endpoints="tcp"/>      </server>    </node>  </application></icegrid>
文件中的信息都是自描述的:- application name: 指明这是该application的部署信息. - node name: 指明node节点- sever id:- exe:- activation:- adapter name- endpoints:
有了部署描述文件, 我们可以部署该application, 需要告知iceGrid registry:
     $ icegridadmin --Ice.Config=config.icegrid -e 'application add demo.xml'
-e 选项告诉icegridadmin执行命令. add命令告知工具我们需要添加demo.xml信息到registry数据库中.命令同时告知registry和node对应的配置文件.icegreidadmin只需要读取该配置文件的Ice.Default.Locator属性.因为工具需要知道如何联系位置服务.
以上都是"按需激活"的必要步骤.



























