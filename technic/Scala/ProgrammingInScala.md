# Scala编程

# 1 可扩展的语言

增加新的类型:

     scala像一个集市而不是大教堂, 它被设计为可以让使用他的人扩展和修改.

增加新的控制结构:
     
     基于actor的并发模型

## 为何使用scala
1 兼容的
2 简洁的
3 高级的
     
     程序员总是在与代码的复杂性搏斗.只有对工作的代码了如指掌,才能有的放矢的编程开发.
     复杂的代码成了很多软件项目崩溃的原因.
     不幸的是,重要的软件通常需求复杂.复杂性不可避免,那么必须加强管理.
     scala可以通过帮助提升接口的抽象级别来帮助管理复杂性.

4 静态类型的
     
     scala通过类型推断避免了冗余性
     通过模式匹配和一些新的编写和组织类型的办法获得了灵活性.
     静态类型系统可以保证消除某些运行时错误.


# 2 Scala入门初探

定义方法时, 如果仅仅包含一条语句, 可以连花括号可以不写.

倾向于函数式的编程风格::
     
        args.foreach( arg => println(arg ) )
        args.foreach( println )

scala的数组是通过索引值加圆括号进行访问( 不是方括号)

另一个准则: 方法只有一个参数,调用的时候可以省略点和括号::

        for( i <- 0 to 2 )
            print( greetingString(i))

用括号传递给变量一个或多个值变量时, Scala会把它转换为对apply方法的调用.

类似有update方法调用.

scala对任何事物都追求概念的简洁性, 从数组到表达式,包括带有方法的对象.

方法没有副作用是函数式风格编程的重要概念.

为何列表不支持append, 性能差

Scala程序员的平衡感:

     崇尚val, 不可变量和没有副作用的方法.
     首先想到他们.只有在特定需要和并加以权衡之后才选择var,可变对象和有副作用的方法.

从文件里读取文本行:

        for( line <- Source.fromFile("scalaProject.iml").getLines())
          println( line.length + " " + line)

# 4 类和对象

保持对象健壮性的重要方法之一: 保证对象的状态.

scala里方法参数的一个重要特征是他们都是val.

方法的推荐风格是尽量避免return语句, 尤其是多条return语句.

scala通常的风格:把操作符放在行尾和不是行头::

        x +
        y +
        z

scala不能定义静态成员,而代之定义单例对象.

# 5 基本类型和操作

值类型: Int,Long,Short,Byte,Float,Double,Char, Boolean
String

符号字面值:  '<标识符>, Symbol("cybmbal")

方法调用的空括号可以省略. 惯例是若方法带有副作用就加上括号, 如pringln(). 若没有副作用,就去掉括号.

scala的==与java有差别.

富包装器.

# 6 函数式对象

函数式对象: 不具有任何可改变状态的对象的类

不可变对象的缺点: 有时需要负责很大的对象表, 而可变对象的更新可以在原址上发生.

为朱构造器定义先决条件. 用require方法.

不建议在标识符结尾使用下划线.

scala的常量名, 习惯只是第一个字母必须大写.

如果无技巧性地使用, 操作符方法和隐式转换都会让客户代码变得难以阅读和理解.

在设计库的时候你应记在脑袋你的目标并不是仅仅让客户代码简洁, 而是让他变得更可读,更易懂.

7 内建控制结构
===============

由于while循环不产生值,因此他经常被纯函数式语言所舍弃.

for 过滤:

        val filesHere = (new java.io.File(".") ).listFiles
        for( file <- filesHere if file.getname.endsWith(".scala")
            println( file)

        for( 
            file <- filesHere
            if file.isFile;
            if file.getName.endsWith(".scala")
        ) println(file)

嵌套枚举:

        def fileLines(file: java.io.File) =
            scala.io.Source.fromFile(file).getLines.toList

        def grep(pattern: String)=
            for(
                file <- filesHere
                if file.getName.endsWith(".scala");     //必须有分号;
                line <- fileLines(file)
                if line.trim.matches(pattern)
            )println( file + ": "+ line.trim )

        grep (".*gcd.*")

可以使用花括号代替小括号包裹发生器和过滤器,优点:可以不写分号.

以上有缺点, line.trim多次计算. 可以使用流间变量绑定::
     
        def grep(pattern: String)=
            for {
                file <- filesHere
                if file.getName.endsWith(".scala")
                line <- fileLines(file)
                trimmed = line.trim
                if trimmed.matches(pattern)
            } println( file + ": "+ trimmed )

制造新集合. 使用yield::
          
        for {  子句 } yield { 循环体}

抛出异常的类型是Nothing.

catch使用模式匹配::

        try{
            val f = new FileReader("input.txt")
        }catch{
            case ex:FileNotFoundException => // 处理丢失的文件
            case ex: IOException => //处理io错误
        }

finally也产生值.

不再使用break和continue

变量范围::

    任何定义在花括号里的东西超出括号之后就脱离了范围. ( for子句有例外)

8 函数和闭包
=============
函数式编程风格的重要设计原则: 程序应该被解构成若干小的函数.

本地函数可以访问包含其函数的参数.

函数字面值::
     
        (x: Int) => x + 1

想让函数字面量包含多条语句,可以用花括号包住函数体,一行一语句 ::

        increase = (x: Int) => {
            println("We")
            println("are")
            println("here!)
            x + 1
        }

其他例子::

        val someNumbers = List( -11, -10, -5, 0, 5, 10)
        someNumbers.foreach(  (x:Int) => println( x ) )
        someNumbers.filter( (x:Int) => x > 0 )

函数字面量的短格式. ( 去除参数类型; 去除括号) ::

        someNumbers.filter( x => x > 0 )

占位符语法(只要每个参数在函数字面量内仅出现一次)::
     
        someNumbers.filter( _ > 0 )
        
        val f = (_: Int ) + (_ : Int )
        f(5, 10 )

还可以用单个下划线代替整个参数列表::

        someNumbers.foreach( println _ )
        def sum(a: Int, b: Int, c:Int ) = a + b + c
        val a = sum _
        a( 1, 2, 3 )

        val b = sum( 1, _:Int, 3 )

 写一个省略所以参数偏函数表达式, 如 println _ 或 sum _, 在需要函数的地方可进一步去掉占位符::

        someNumbers.foreach( println )

        //但不能这样写: val c = sum
        val d = sum _   //必须带占位符
     
9 控制抽象
==========
## 9.1 减少代码重复

所有的函数都可以被分成通用部分, 以及非通用部分. 通用部分是函数体, 而非通用部分必须由参数提供.

高阶函数: 带其他函数做参数的函数- 给你额外的机会去组织和简化代码.

例如:

        object FileMatcher {
            def fileHere = (new java.io.File(".")).listFiles
        def filesEnding(query:String) =
            for(file <- fileHere; if file.getName.endsWith(query))
            yield file
    
        def filesContain(query:String) =
            for(file <- fileHere; if file.getName.contains(query))
            yield file
    
        }

以上filesEnding和FilesContain结构很像.

        def filesMatching(query:String, matcher: (String,String) =>Boolean) =
            for(file <- fileHere; if matcher(file.getName, query))
            yield file

那么原来两个方法可以简化为:

        def fileEnding(query:String) = filesMatching(query,_.endsWith(_))
        def fileContain(query:String) = filesMatching(query, _.contains(_))

使用闭包可以更短.

        def filesMatching(matcher: String => Boolean) =
            for(file <- fileHere; if matcher(file.getName))
            yield file

        def fileEnding(query:String) = filesMatching(_.endsWith(query))
        def fileContain(query:String) = filesMatching(_.endsWith(query))

## 9.2 简化客户代码

例子: 判断是否有负数.
     
        def containsNeg( nums: List[Int]):Boolean = {
            var exists = false
            for(num<-nums)
            if (num<0)
                exists = true
            exists   
        }

更简洁的方式, 是使用高阶函数exists:

       def containsNeg(nums: List[Int]) = nums.exists( _ < 0)
       def containsOdd(nums: List[Int]) = nums.exists( _ % 2 == 1) 

## 9.3 柯里化( currying )

例子：

        def plainOldSum(x :Int, y: Int) = x + y
        plainOldSum(1,3)
        def curriedSum(x:Int)(y:Int)= x + y
        curriedSum(1)(3)
        val first = curriedSum(1)_
        first(3)

## 9.4 编写新的控制结构

双倍控制结构:

        def twice(op: Double =>Double, x:Double) = op(op(x))
        twice(_ + 1, 5)

代码模式: 打开一个资源, 对它进行操作, 然后释放资源: 

        def withPrintWriter(file: File)(op: PrintWriter => Unit){
            val writer = new PrintWriter(file)
            try{
            op(writer)
            }finally{
            writer.close()
            }
        }
        val file = new File("date.txt")
        withPrintWriter(file){
            writer => writer.println(new java.util.Date)
        }

scala的任何方法调用, 如果你确实只传入一个参数, 就能可选地使用花括号替代小括号包围参数.
目的: 是让客户程序员写出包围在花括号内的函数字面量,.

## 9.5 传名参数

# 10 组合与继承

##10.1 二维布局库
##10.3 定义无参数方法

scala鼓励使用不带参数且没有副作用的方法定义为无参数方法的风格, 即省略空括号.
无论何时当调用有副作用的方法, 应确保包含一对括号.

scala里的字段和方法属于相同的命名空间.这让字段可以重写无参数方法.

scala仅有两个命名空间:
- 值
- 类型



21 隐式转换和参数
================
隐式转换只是普通的方法, 但它一修饰符implicit开始.

例子:

        implicit def stringWrpper(s: String) = 
                new RandomAccessSeq[Char]  {
                def length = s.length
                def apply(i: Int) = s.charAt(i)
        }

优点:
- 免费获取所有其它RandomAccessSeq的方法
- 支持目标类型的转换

隐性定义是指编译器为了修正类型错误而允许插入到程序中的定义.
例如: 若 x + y不能通过类型检查, 编译器可能会把它改为convert(x) + y

标记规则: 只有标记为implicit的定义才是可用的.

作用域规则: 插入的隐性转换必须以单一标识符的形式处于作用域中,或与转换的源或目标类型关联在一起.

无歧义规则: 隐式转换唯有不存在其他可插入转换的前提下才能插入.

单一调用规则: 只会尝试一个隐式操作, 编译器不会把 x +y 重写成convert1(convert2(x)) + y.

显式操作先行规则: 若编写的代码类型检查无误, 则不会尝试任何隐式操作.

命名隐式转换.

隐式操作在哪里尝试:
- 转换为期望类型
- 指定(方法)调用者的转换
- 隐式参数

被所有Scala程序隐式引用的scala.Predef对象, 定义了把"较小的"数值类型转变为"较大的"类型的隐式转换.

## 转换为方法的接受者
例子:
     
        class Rational(n: Int, d: Int) {
            ...
            def + (that: Rational): Rational = ...
            def + (that: Int): Rational = ...
        }
     
执行：

    scala> 1 + oneHalf
    <console>:6: error: overloaded method value + with
    alternatives (Double)Double <and> ... cannot be applied

    scala> implicit def intToRational(x: Int) = new Rational(x, 1)
    intToRational: (x: Int)Rational

    scala> 1 + oneHalf
    res2: Rational = 3/2

## 模拟新的语法
例子:

        Map(1 ->"one", 2 ->"two", 3 ->"three")

"->"不是内建语法, ->只是定义在标准Scala中类ArrowAssoc的方法, 定义了Any到ArrowAssoc的隐式转化.
编写 1->"one"的时候, 编译器会插入1到ArrowAssoc的转换以便找到->方法.

        package scala
        object Predef {
            class ArrowAssoc[A](x: A) {
                def -> [B](y: B): Tuple2[A, B] = Tuple2(x, y)
            }
            implicit def any2ArrowAssoc[A](x: A): ArrowAssoc[A] =
            new ArrowAssoc(x)
            ...
        }

## 隐式参数

编译器有时会用someCall(a)(b)替换somCall(a) 或者用new someCall(a)(b)替换new someCall(a).
从而通过添加缺失参数列表以满足函数调用.



