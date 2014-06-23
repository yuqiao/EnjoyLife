# Spring实战

# 1 spring之旅

复杂的应用通常需要诸如事务支持,安全, 分布式计算此类的服务, 但JavaBean未提供.
所以就有了EJB. 但EJB从来没有实现它最初的设想: 简化企业级应用开发.

出现新的编程技术: AOP和DI. 为POJO提供了类似EJB的声明式编程, 但未引入EJB的复杂性.


## 1.1 简化java开发
Spring是为了解决企业级应用开发的复杂性而创建的. 

Spring的使命: 简化Java开发.

为了降低java开发的复杂性, spring采用了以下4种策略:

- 基于POJO的轻量级和最小侵入式编程
- 通过依赖注入(DI)和面向接口实现松偶尔
- 基于切面和惯例进行声明式编程
- 通过切面和模板减少样板式代码

### 激发POJO的潜能

EJB2和早期的Structs等重量级的框架, 都存在以下问题: 强迫开发者编写大量冗余代码, 应用于框架绑定, 
并且通常难以编写测试代码.

Spring却极力避免因自身的API而弄乱你的应用代码. 最坏的场景仅仅会使用Spring注解.

### 依赖注入

通常, 每个对象负责管理与自己相互协作的对象(依赖的对象)的引用. 导致高度耦合和难以测试自己的代码.

依赖注入让相互协助的软件组织保持松耦合.


## 1.2 容纳你的Bean



## 1.3 俯瞰Spring风景线

## 1.4 Spring新功能


## 小结

Spring致力于简化企业级java开发, 促进代码设计松耦合. 成功的关键在于依赖注入和AOP.

 
# 2 装配Bean

在spring中, 对象无需负责查找和创建与其关联的对象. 相反, 容器负责把需要协作的对象引用赋予给各个对象.

创建应用对象之间协作关系的行为常称为**装配**(wiring), 这也是依赖注入的本质.



## 2.1 声明Bean

### 简单例子

在这个例子里, 定义一个接口: 
	
	package com.springinaction.springidol; 
    public interface Performer {         
        void perform() throws PerformanceException;	
    }

典型spring xml配置:

    <?xml version="1.0" encoding="UTF-8"?>
    <beans xmlns="http://www.springframework.org/schema/beans"
        xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
        xsi:schemaLocation="http://www.springframework.org/schema/beans
    http://www.springframework.org/schema/beans/spring-beans-3.0.xsd">
        <!-- Bean declarations go here -->
    </beans>

声明一个简单Bean:

    package com.springinaction.springidol;
    public class Juggler implements Performer {
        private int beanBags = 3;
        public Juggler() {
        }
        public Juggler(int beanBags) {
            this.beanBags = beanBags;
        }
        public void perform() throws PerformanceException {
            System.out.println("JUGGLING " + beanBags + " BEANBAGS");
        } 
    }

在spring配置文件spring-idol.xml中:

    <bean id="duke"
              class="com.springinaction.springidol.Juggler" />


获取Juggler实例:

    ApplicationContext ctx = new ClassPathXmlApplicationContext(
        "com/springinaction/springidol/spring-idol.xml");
    Performer performer = (Performer) ctx.getBean("duke");
    performer.perform();

执行结果:

    JUGGLING 3 BEANBAGS


另一种spring配置文件, 使用有参数的构造函数:

    <bean id="duke"
        class="com.springinaction.springidol.Juggler">
        <constructor-arg value="15" />
    </bean>
    
### 注入对象引用

声明一个能吟诗的juggler:

    package com.springinaction.springidol;
        public class PoeticJuggler extends Juggler {
        private Poem poem;
        public PoeticJuggler(Poem poem) {
            super();
            this.poem = poem;
        }
        public PoeticJuggler(int beanBags, Poem poem) {
            super(beanBags);
            this.poem = poem;
        }
        public void perform() throws PerformanceException {
            super.perform();
            System.out.println("While reciting...");
            poem.recite();
        } 
    }


## 2.2 注入Bean属性

## 2.3 使用表达式装配

# 3 最小化Spring XML配置

spring减少配置技巧:

- 自动装配(autowiring)
- 自动检测(autodiscovery).

## 3.1 自动装配Bean属性

### 4种类型的自动装配
- byName
- byType
- constructor
- autodetect

## 3.2 使用装配注解

## 3.3 自动检测Bean

## 3.4 使用Spring基于Java的配置

## 小结

# 4 面向切面的Spring

横切关注点: 分布于应用多处的功能, 从概念上是与应用的业务逻辑相分离.

AOP(面向切面编程)将横切关注点与业务逻辑相分离,可以实现横切关注点与他们所影响的对象之间的解耦.

## 4.1 什么是面向切面编程

通知(advice): 定义了什么是切面以及何时使用.spring 切面有5种类型的通知:

- Before
- After
- After-returning
- After-throwing
- Around

切点(pointcut): 定义了"何处". 通常使用明确的类和方法名称来指定这些切点.

连接点(join point): 在应用执行过程中能够插入切面的一个点.

切面(Aspect): 通知和切点的结合.

引入(Introduction): 允许想现有的类添加新方法或属性.

### Spring对AOP的支持
Spring提供了4种:

- 基于代理的经典AOP
- @AspectJ注解驱动的切面
- 纯POJO切面
- 注入式AspectJ切面





## 4.2 使用切点选择连接点

## 4.3 在XML中声明切面

## 4.4 注解切面

## 4.5 注入AspectJ切面

## 小结

# 5 征服数据库

## 5.1 Spring的数据访问哲学

## 5.2 配置数据源

## 5.3 在Spring中使用JDBC

## 5.4 在Spring中集成Hibermate

## 5.5 Spring与Java持久化

# 6 事务编程
## 6.1 理解事务

## 6.2 选择事务管理器

## 6.3 在Spring中的编码事务

## 6.4 声明式事务

# 7 使用Spring MVC构建Web应用程序

## 7.1 Spring MVC起步

## 7.2 编写基本的控制器

## 7.3 处理控制器的输入

## 7.4 处理表单

## 7.5 处理文件上传

# 8 使用Spring web Flow

## 8.1 安装Spring Web Flow

## 8.2 流程的组件

## 8.3 组合起来: 披萨流程

## 8.4 保护Web流程

# 9 保护Spring应用

## 9.1 Spring Security介绍

## 9.2 保护Web请求

## 9.3 保护视图级别的元素

## 9.4 认证用户

## 9.5 保护方法调用

# 10 使用远程服务

## 10.1 Spring远程调用概览

## 10.2 使用RMI

## 10.3 使用Hessian和Burlap发布远程服务

## 10.4 使用Spring的HttpInvoker

## 10.5 发布和使用Web服务

# 11 为Spring添加REST功能

## 11.1 了解REST
## 11.2 编写面向资源的控制器
## 11.3 表述资源
## 11.4 编写REST客户端
## 11.5 提交RESTful表单

# 12 Spring消息
## 12.1 JMS简介
## 12.2 在Spring中搭建消息代理
## 12.3 使用Spring的JMS模板
## 12.4 创建消息驱动的POJO
## 12.5 使用基于消息的RPC


# 13 使用JMX管理Spring Bean

## 13.1 将Spring Bean导出为MBean
## 13.2 远程MBean
## 13.3 处理通知

# 14 其它Spring技巧

## 14.1 外部化配置
## 14.2 装配JNDI对象
## 14.3 发送邮件
## 14.4 调度和后台任务


