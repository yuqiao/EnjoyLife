# java编程思想

# 12 通过异常处理错误

java的基本理念: 结构不佳的代码不能运行.

java的异常处理的目的在于通过使用少于目前数量的代码来简化大型,可靠的程序的生成,并通过这种方式使你更加自信:你的应用中没有未处理的错误.

## 12.1 概念

c以及其他语言错误处理模式的问题.

解决的办法: 用强制规定的形式来消除错误处理中随心所欲的因素.

另一个好处: 降低错误处理的复杂度.可以把"描述在正常执行过程中做什么事"和"出了问题怎么办"的代码分离.

## 12.2 基本异常

异常情形(exceptional condition)是指阻止当前方法或作用域继续执行的问题.

异常最重要的方面之一是: 如果发生问题, 它们将不允许程序沿着其正常的路径继续走下去.

例子:

    throw new NullPointerException("t = null");

## 12.3 捕获异常

try块:

    try{
        // code that might generate exceptions
    }catch(Type1 id1){
        // Handle exptions of Type1
    }catch(Type2 id2){
        // Handle exception of Type2 
    }

异常处理两种基本模型:
- 终止模型
- 恢复模型. 修正错误,然后重新尝试调用出问题的方法.

最终采用类似"终止模型"的代码. 恢复性的处理程序需要了解异常抛出的地点,势必要包含依赖于抛出位置的非通用性代码.增加了代码编写和维护的困难.

## 12.4 创建自定义异常

例子:

    class SimpleException extends Exception {}

    public class InheritingExceptions{
        public void f() throws SimpleException {
            System.out.println("Throw Exception from f()");
            throw new SimpleException();
        }
        public static main(String[] args){
            try{
                f();
            }catch(SimpleException e){
                System.out.println("Caugth it!");
            }
        }


## 12.12 其它可选方式

异常处理的原则之一: 只有在你知道如何处理时才捕获异常.

异常处理的目标之一: 把错误处理的代码和错误发生的地点分离开. 让两部分代码各专注各的.


