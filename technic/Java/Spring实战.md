# Sprint实战

# 一 Spring的核心
Spring的两个核心特性：依赖注入(DI)和面向切面编程（AOP）。

## 1. Spring之旅

### 1.1 简化Java开发

Spring的最根本的使命：简化Java开发。

为降低Java开发的复杂性，Spring采取了4种关键策略：

- 基于POJO的轻量级和最小侵入性编程
- 通过依赖注入和面向接口实现松耦合
- 基于切面和惯例进行声明式编程
- 通过切面和模板减少样板式代码

POJO(Plain Old Java Object) ：简单老式Java对象。

非侵入式编程模型：Spring竭力避免因自身API而弄乱你的应用代码，不会强迫你实现Spring规范的接口或继承Spring规范的类。

传统的做法：每个对象负责管理与自己相互协作的对象（即它所依赖的对象）的引用，这将导致高度耦合和难以测试的代码。

耦合的两面性：

- 紧密耦合的代码难以测试，难以复用，难以理解，并且典型地表现出“打地鼠”的bug特性（修复了一个bug, 将会出现一个或者更多新bug)
- 一定程度的耦合又是必须的—— 完全没有耦合的代码什么也做不了。

依赖注入会将所依赖的关系自动交给目标对象，而不是让对象自己去获取依赖。

Spring通过应用上下文（Application Context)装载bean的定义并把他们组装起来。

DI能够让相互协作的软件组件保持松散耦合，而AOP允许你把遍布应用各处功能分类处理形成可重用的组件。



### 1.2 容纳你的Bean

基于Spring的应用中，你的应用对象生存于Spring容器中。Spring容器负责创建对象，装配他们，配置它们并管理他们的整个生命周期，从生存到死亡。

#### 使用应用上下文

#### bean的生命周期

1. 对Bean实例化
2. 填充属性
3. 调用BeanNameAware的setBean-name()方法
4. 调用BeanFactoryAware的setBeanFactory方法
5. ​

### 1.3 俯瞰Spring风景线

### 1.4 Spring的新功能

## 2. 装配Bean

2.1 Spring配置的可选方案

2.2 自动化装配Bean

2.3 通过java代码装配Bean

2.4 通过XML装配Bean

2.5 导入和混合配置

## 3. 高级装配

3.1 环境和profile

3.2 条件化的Bean

3.3 处理自动装配的歧义性

3.4 bean的作用域

3.5 运行时值注入

## 4. 面向切面的Spring

4.1 什么是面向切面编程

4.2 通过切点来选择连接点

4.3 使用注解创建切面

4.4 在XML声明切面

4.5 注入AspectJ切面

4.6 小结

# 二 Web中的Spring

## 5. 构建Spring WEb应用
## 6. 渲染Web视图
## 7. Spring MVC的高级技术
## 8. Spring Web Flow
## 9. 保护Web应用

# 三 后端中的Spring
## 10. 通过Spring和JDBC征服数据库
## 11. 使用对象-关系映射持久化数据
## 12. 使用NOSQL数据库
## 13. 缓存数据
## 14. 保护方法应用

# 四 Spring集成
## 15. 使用远程服务
## 16. 使用Spring MVC创建REST API
## 17. Spring消息
## 18. 使用WebSocket和Stomp
## 19. 使用Spring发送邮件
## 20. 使用JMX管理Spring Bean
## 21. 借助Sring Boot简化Spring开发


