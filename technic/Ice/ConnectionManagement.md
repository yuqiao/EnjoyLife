# Ice连接管理

Ice运行时为客户端透明地创建和销毁连接. 客户端应用程序不需要去关心ice如何管理连接.
但是, 了解ice如何管理连接, 是有益的, 特别是对于提供多个endpoints的服务.

## 客户端连接

当客户端通过TCP/SSL连接服务器时, Ice需要为两端建立连接. 连接由客户端初始化,服务端接受.
客户端通常查询proxy获取Connection对象( 描述了proxy的底层连接)
Connnection对象提供close和cretePrxy等操作方法.

有两种方法,可以通过proxy获取Connection对象:
- ice_getConnection, 返回与这个iceProxy关联的Connection对象. 若没有对应的connection存在, ice运行时会先创建一个连接
并且为这个新连接返回一个Connection对象. 若ice运行时创建不了连接, 就会抛出CollocationOptimizationException异常.
- ice_getCachedConnection. 若有关联的连接, 则返回Connection对象, 否则返回null.

获取Connection简单例子::

        CommunicatorPtr communicator = ...;
        HelloPrx hello = HelloPrx::uncheckedCast(    communicator->stringToProxy("hello:tcp -h remote.host.com -p 10000"));
        ConnectionPtr conn = hello->ice_getConnection();

ice_getConnection建立连接到remote.host.com的端口10000,并且返回相关的Connection对象. 与之相反, 有另一个例子::

        CommunicatorPtr communicator = ...;
        HelloPrx hello = HelloPrx::uncheckedCast(    communicator->stringToProxy("hello:tcp -h remote.host.com -p 10000"));
        ConnectionPtr conn = hello->ice_getCachedConnection();

在这种情况下, 因为没有连接被事先建立, ice_getCachedConnection获取null.

正如你现象, connections并不便宜.连接会消耗一个文件描述符和一些内存来跟踪pending请求.
正因为connection比较昂贵, connections的复用,是Ice运行库的一个必须的部分.
理解客户端判需要创建一个新连接或是复用原有连接, 非常重要.

## Connection生命周期
Ice运行时维持了当前存在的连接池. 但通过proxy做远程调用时, 运行时会绑定这些连接到proxy.
运行会透明地根据需要创建连接. 

例如代码::

        CommunicatorPtr communicator = ...;
        HelloPrx h1 = HelloPrx::uncheckedCast(communicator->stringToProxy("hello:tcp -h remote.host.com -p 10000"));
        h1->sayHello(); // Connection creation and binding occurs here

但客户端通过proxy h1调用sayHello方法时, ice运行时会创建一个连接到remote.host.com的端口10000并绑定这个连接到这个proxy.
需要注意: 前面例子使用uncheckedCast 方法. 该方法并不会做远程调用,所以从来不会建立连接. 但若使用checkedCast,连接创建就会发生,
因为checked cast需要一个远程调用来确定目标object是否支持指定的接口.


connection的生命周期独立于proxy的生命周期. 例如::

        void doit(const CommunicatorPtr& communicator){    
            HelloPrx h1 = HelloPrx::uncheckedCast(communicator->stringToProxy("hello:tcp -h remote.host.com -p 10000"));    
            h1->sayHello(); // Connection creation and binding occurs here 
        }


一旦doit方法返回, C++运行时就用摧毁proxy h1.但是曾绑定到h1的连接还是继续存在. 因为他们完全独立. 
所以就用这样一个问题:如何,什么时候关闭连接和释放相关资源.


Ice运行时在以下几种情况会关闭销毁连接:
-  销毁communicator, 就会销毁communicator上的连接.
-  如果ACM打开, 若连接长时间处于idle状态, 就会被关闭.
-  可以显式调用Connection对象的close方法来关闭.
-  若connection有个timeout, 运行时会因为timeout超时, 关闭连接.
-  运行时遇到了不可恢复的错误例如socket错误或者收到的数据违反了ice协议, 它将关闭对应的连接.


proxy在它的生命周期中, 会绑定到不同的连接. 例如: proxy刚开始有个连接,保持了一段空闲时间后被acm关闭, 下一次proxy被使用时, 
运行时会透明地为这个proxy创建一个新的连接. 类似的, 因为旧连接被关闭(以上很多原因)或者connection缓存被设置失效, 一个新的连接会被创建


如果先永久地位proxy绑定一个特殊的连接, 可以通过Connection::createProxy创建一个固定proxy.


ice运行时会尽可能复用已存在的连接, 例如::

        CommunicatorPtr communicator = ...;
        HelloPrx h1 = HelloPrx::uncheckedCast(communicator->stringToProxy("hello:tcp -h remote.host.com -p 10000"));
        h1->sayHello();
        HelloPrx h2 = HelloPrx::uncheckedCast(communicator->stringToProxy("hello2:tcp -h remote.host.com -p 10000"));
        h2->sayHello();

在以上这种情况下, Ice运行时为两个不同的proxy h1和h2绑定同一个连接. 因为两个proxy都指向同一个端口的同一个对象.


## Endpoints Selection
绑定时, ice运行时检查proxy的端口, 从端口列表中, 产生出候选端口列表. 创建候选端口的默认算法:
- 删除无用和不兼容的端口.
- 搅乱端口列表, 将secure端口放到列表的最后, 这样建立起来优先列表.
- 检查确认是否已存在适合连接.如何有, 复用这些连接.
- 若没有合适的连接存在, 从候选列表遍历, 试图建立连接.


代理设置可以修改这个算法.


## 删除不可用和不合适端点
删除端点的第一步, 满足以下条件:
- 端点不可知.
- 端点不合适, 
- 端口是insecure. 但proxy配置成要求secure.

若找不到一个端口, 调用就会抛出一个异常:NoEndpointException.



