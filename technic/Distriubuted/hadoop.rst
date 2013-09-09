============================
Hadoop 权威指南
============================

:author: yuqiao20@gmail.com
:version: 0.0.1
:Date: 

3 Hadoop 分布式文件系统
===========================
更加复杂：
- 网络编程
- 容忍节点故障且不丢失任何数据。

3.1HDFS的设计
------------------
以流式数据访问模式来存储超大文件，运行在商用硬件集群上：
- 超大文件。GB，TB ..., 一次写入多次读取。每次分析都将涉及数据集的大部分数据甚至全部。
  商用硬件。

3.2 HDFS的概念
---------------

数据块
~~~~~~
块，默认为64MB，目的是为了最小化寻址开销。
块抽象好处：
- 一个文件大小可以大于网络中任意一个磁盘的容量。
- 简化存储子系统的设计。

namenode和datanode
~~~~~~~~~~~~~~~~~~~~
namenode(管理者）：管理文件系统的命名空间，维护着文件系统树及整棵树内所有的文件和目录。也记录镁光文件中各个块所在的数据节点信息。

datanode(工作者）：根据需要存储并检索数据块，并定期向namenode发送它们所存储的块列表。

client代表用户通过namenode和datanode交互来访问整个文件系统。

namenode的容错性很重要，hadoop有两种机制：
- 备份那些组成文件系统元数据持久状态的文件。
- 运行一个辅助namenode,定期通过编辑日志合并命名空间镜像。

3.3 命令行接口
-------------------
伪分布模式设置Hadoop。

分布配置：
- fs.default.name: hdfs://localhost/, 默认文件系统。通过该属性确定HDFS namenode的主机和端口： 将在localhost默认端口8020运行namenode。
  dfs.replication: 1, HDFS就不会按默认设置将文件系统块复本设置为3。在单独一个datanode上运行时，无法复制到3个datanode上。

基本文件系统操作
~~~~~~~~~~~~~~~~
- hadoop fs -help

1. 从本地复制到HDFS:
   hadoop fs -copyFromLocal input/docs/quangle.txt hdfs://localhost/user/tom/quangle.txt
   hadoop fs -copyFromLocal input/docs/quangle.txt quangle.txt

2. 复制回本地：
   hadoop fs -copyToLocal quangle.txt quangle.copy.txt

3. hadoop fs -mkdir books

4. hadoop fs -ls .

目录作为元数据保存在namenode中，而非datanode中。


3.4 Hadoop文件系统
---------------------
Hadoop有个抽象的文件系统概念，HDFS只是其中的一个实现。org.apache.hadoop.fs.FileSystem.

文件系统        URI方案         Java实现                    描述
Local           file            fs.LocalFileSystem
HDFS            hdfs            hdfs.DistributedFileSystem  
HFTP            Hftp            hdfs.hftpFileSystem
HSFTP           hsftp           hdfs.HsftpFileSystem
HAR             har             fs.HarFileSystem 
hfs
FTP
S3

接口
~~~~~~
Thrift: 使之跨语言

FUSE（FileSystem in Userspace)用户空间文件系统，通过使用Hadoop的Fuse-DFS功能模块，将Hadoop文件系统作为标准文件系统挂载。

WebDAV：扩展http, 并支持文件编辑和文件更新。


3.5 Java接口
------------
从Hadoop URL中读取数据
~~~~~~~~~~~~~~~~~~~~~~
使用java.net.URL对象打开数据流::

    InputStream in = null;
    try{
        in = new URL("hdfs://host/path").openStream();
        //process in
    }finally{
        IOUtils.closeStream(in);
    }

为了让java能够识别hdfs URL方案，有额外工作::

    public class URLCat{
        static {
            URL.setURLStreamHandlerFactory( new FsUrlStreamHandlerFactory() );
        }

        public static void main(String[] args) throws Exception {
            InputStream in = null;
            try{
                in = new URL(args[0]).openStream();
                IOUtils.copyBytes(in, System.out, 4096,false);
            }finally{
                IOUtils.closeStream(in);
            }
        }
    }

运行示例：
    hadoop URLCat hdfs://localhost/user/tom/quangle.txt


通过FileSystem API读取数据
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
有时无法在应用中设置URLStreamHandlerFactory实例, 就需要FileSystem Api打开文件。

在Hadoop文件系统中：
- 使用Hadoop Path对象来代表文件。

- FileSystem是通用api,所以第一步是检索需要使用的系统实例，这里是HDFS,获取实例有两种静态方法：
  - public static FileSystem get(Configuration conf) throws IOException;
  - public static FileSystem get(URI uri, Configuration conf) throws IOException;

- 调用open()函数来获取文件的输入流:


写入数据
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
public FSDataOutputStream create(Path f) throws IOException;

还有个重载方法Progressable, 拥有传递回调接口。

目录
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
public boolean mkdirs(Path f) throws IOException;

查询文件系统
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. 文件元数据： FileStatus

   FileStatus类封装了文件系统中文件和目录的元数据，包括文件长度，块大小，备份，修改时间，所有者以及权限信息。

   getFileStatus()用于获取文件或目录的FileStatus对象。

2. 列出文件listStatus()

3. 文件模式
   通配，globStatus()

4. PathFilter对象


删除数据
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
public boolean delete(Path f, boolean recursive) throws IOException;

3.6 数据流
-------------
文件读取剖析
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
1. client 通过FileSystem对象open()方法打开希望读取的文件：
   a. DistributeFileSystem 通过RPC调用 namenode, 确定文件起始块的位置：
        对于每一个块，namenode访问存有该块复本的datanode地址。
   b. DistributeFileSystem 返回一个FSDataInputStream对象给client.
        FSDataInputStream 类转而封装DFSInputStream对象，该对象管理datanode和namenode的I/O.

2. client 从流中读取数据read(). 存储datanode地址的DFSInputStream随即连接最近的datanode,通过对数据流反复read(),
   将数据从datanode传输到client. 到达块的末端时，DFSInputStream会关闭与该datanode的连接，然后寻找下一个块的最佳
   datanode. 对于client来说，它只需对其连续的流，对这些并不需要知晓。

3. 一旦客户端完成读取，对FSDataInputStream调用close()方法。

设计的重点：
1. namenode负责告知client每个块中最佳的datanode,并让client直接联系该datanode且检索数据。

2. namenode只负责响应块位置请求（信息存在内存中，非常高效），避免成为瓶颈。


文件写入剖析
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
一致模型
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

3.7 通过distcp并行复制
------------------------
保持HDFS集群的均衡
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

3.8 Hadoop存档
----------------
使用Hadoop存档工具
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
不足
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

4 Hadoop I/O
===============
4.1 数据完整性
----------------
4.2 压缩
----------------
4.3 序列化
----------------
4.4 基于文件的数据结构
--------------------------

5 MapReduce应用开发
=======================
5.1 配置API
----------------

5.2 配置开发环境
----------------

5.3 编写单元测试
----------------

5.4 本地运行测试数据
-------------------------

5.6 在集群中运行
-------------------------

5.7 作业调优
-------------------------

5.8 MapReduce的工作流
-------------------------

6 MapReduce工作机制
=======================
6.1 剖析MapReduce作业运行机制
-------------------------------

6.2 失败
-------------------------

6.3 作业的调度
-------------------------

6.4 shuffle和排序
-------------------------------

6.5 任务的执行
-------------------------------

7 MapReduce类型与格式
=======================
7.1 MapReduce类型
-------------------------------

7.2 输入格式
-------------------------------

7.3 输出格式
-------------------------------

8 MapReduce的特性
=======================
8.1 计数器
-------------------------------

8.2 排序
-------------------------------

8.3 连接
-------------------------------

8.4 边数据分布
-------------------------------

8.5 MapReduce库类
-------------------------------


9 构建Hadoop集群
=======================
9.1 集群规范
-------------------------------

9.2 集群的构建和安装
-------------------------------

9.3 SSH配置
-------------------------------

9.4 Hadoop配置
-------------------------------

9.5 安全性
-------------------------------

9.6 利用基准测试程序测试Hadoop集群
------------------------------------

9.7 云端的Hadoop
-------------------------------


10 管理Hadoop
=======================
10.1 HDFS
-------------------------------

10.2 监控
-------------------------------

10.3 维护
-------------------------------

11 Pig简介
=======================
11.1 安装和运行Pig
-------------------------------

11.2 示例
-------------------------------

11.3 与数据库比较
-------------------------------

11.4 Pig Latin
-------------------------------

11.5 用户自定义函数
-------------------------------

11.6 数据处理操作
-------------------------------

11.7 Pig实战
-------------------------------

12 Hive简介
=======================
12.1 安装Hive
-------------------------------

12.2 示例
-------------------------------

12.3 运行Hive
-------------------------------

12.4 和传统数据库比较
-------------------------------

12.5 HiveQL
-------------------------------

12.6 表
-------------------------------

12.7 查询数据
-------------------------------

12.8 用户定义函数
-------------------------------


13 HBase
=======================

13.1 HBase基础
-------------------------------

13.2 概念
-------------------------------

13.3 安装
-------------------------------

13.4 客户端
-------------------------------

13.5 示例
-------------------------------

13.6 HBase和RDBMS比较
-------------------------------

13.7 Praxis
-------------------------------

14 ZooKeeper
=======================

14.1 安装和运行Zookeeper
-------------------------------

14.2 示例
-------------------------------

14.3 ZooKeeper服务
-------------------------------

14.4 使用ZooKeeper来构建应用
-------------------------------

14.5 生产环境中的ZooKeeper
-------------------------------

15 开源工具Sqoop
=======================

16 实例分析
=======================


~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~~


