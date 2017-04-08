# 编写高质量代码
## 第1章　引论 1
### 建议1：理解Pythonic概念 1
Python之禅：

- 美胜丑，显胜隐，简胜杂，杂胜乱，平胜陡，疏胜密。
- 找到简单问题的一个方法，最好是唯一的方法（正确的解决之道）
- 难以解释的实现，源自不好的主意；如有非常棒的主意，它的实现肯定易于实现。



编写python代码：

-  可读性
-  代码风格
-  标准库； str.format 优于 ‘%s'
-  Pythonic的库或框架： Flask

### 建议2：编写Pythonic代码 5

- 避免劣化代码
- 深入认识Python: 全面掌握Python提供的特性；深入学习pythonic的代码（Flask, gevent, requests)
- 代码规范pep8:  pep8 —show-source —show-pep8  xxx/xx.py

Pep8, pychekcer(google python style), pylint, pyflakes



### 建议3：理解Python与C语言的不同之处 8

### 建议4：在代码中适当添加注释 10
### 建议5：通过适当添加空行使代码布局更为优雅、合理 12
### 建议6：编写函数的4个原则 15

1. 函数设计要尽量短小，嵌套层次不宜过深。
2. 函数声明应该做到合理，简单，易于使用
3. 函数设计应考虑向下兼容
4. 一个函数只做一件事，尽量保证函数语句粒度的一致性。



### 建议7：将常量集中到一个文件 18

常量使用的两种方式：

- 常量名所有字母大写

- 自定义类实现常量功能： 命名全部大写， 值一旦绑定便不可再修改

  ```python
  class _const:
      class ConstError(TypeErrory): pass
      class ConstCaseError(ConstError): pass
      
      def __setattr__(self, name, value):
          if self.__dict__.has_key(name):
              raise self.ConstError, "Can't change const.%s" % name
          if not name.isupper():
              raise self.ConstCaseError, \
              		'conxt name "%s" is not all uppercase' % name
  import sys
  sys.modules[__name__] = _const()
  ```

  ​

## 第2章　编程惯用法 20
### 建议8：利用assert语句来发现问题 20



### 建议9：数据交换值的时候不推荐使用中间变量 22
### 建议10：充分利用Lazy evaluation的特性 24

itertools

### 建议11：理解枚举替代实现的缺陷 25

- 使用类属性
- 使用collections.namedtuple

```Python
Seasons = namedtuple('Seasons', "Spring Summer Autumn Winter")._make(range(4))
print Seasons.Spring
```

- 使用第三方模块 flufl.enum

### 建议12：不推荐使用type来进行类型检查 27

若类型有对应工厂函数，使用工厂函数对类型转换；否则使用isinstance()来检测。

```python
isinstance(2, floart)     # False
isinstance("a", (str, unicode)) # True
isinstance((2,3), (str, list, tuple))  # True 支持多种类型列表
```



### 建议13：尽量转换为浮点类型后再做除法 29
### 建议14：警惕eval()的安全漏洞 31

eval语法： eval(expression[, globals[, locals]])

应尽量避免使用eval, 在需要使用eval的地方使用安全性更好的ast.literal_eval替代。

### 建议15：使用enumerate()获取序列迭代的索引和值 33
### 建议16：分清==与is的适用场景 35
### 建议17：考虑兼容性，尽可能使用Unicode 37
### 建议18：构建合理的包层次来管理module 42

## 第3章　基础语法 45
### 建议19：有节制地使用from...import语句 45

- 尽量优先使用import a形式
- 有节制使用from a import B形式
- 尽量避免使用from a import * 



### 建议20：优先使用absolute import来导入模块 48
### 建议21：i+=1不等于++i 50
### 建议22：使用with自动关闭资源 50
### 建议23：使用else子句简化循环（异常处理） 53
### 建议24：遵循异常处理的几点基本原则 55
### 建议25：避免finally中可能发生的陷阱 59
### 建议26：深入理解None，正确判断对象是否为空 60
### 建议27：连接字符串应优先使用join而不是+ 62
### 建议28：格式化字符串时尽量使用.format方式而不是% 64

read hear.

### 建议29：区别对待可变对象和不可变对象 68
### 建议30：[]、()和{}：一致的容器初始化形式 71
### 建议31：记住函数传参既不是传值也不是传引用 73
### 建议32：警惕默认参数潜在的问题 77
### 建议33：慎用变长参数 78
### 建议34：深入理解str()和repr()的区别 80
### 建议35：分清staticmethod和classmethod的适用场景 82

## 第4章　库 86
### 建议36：掌握字符串的基本用法 86
### 建议37：按需选择sort()或者sorted() 89
### 建议38：使用copy模块深拷贝对象 92
### 建议39：使用Counter进行计数统计 95
### 建议40：深入掌握ConfigParser 97
### 建议41：使用argparse处理命令行参数 99
### 建议42：使用pandas处理大型CSV文件 103
### 建议43：一般情况使用ElementTree解析XML 107
### 建议44：理解模块pickle优劣 111
### 建议45：序列化的另一个不错的选择——JSON 113
### 建议46：使用traceback获取栈信息 116
### 建议47：使用logging记录日志信息 119
### 建议48：使用threading模块编写多线程程序 122
### 建议49：使用Queue使多线程编程更安全 125

## 第5章　设计模式 129
### 建议50：利用模块实现单例模式 129
### 建议51：用mixin模式让程序更加灵活 132
### 建议52：用发布订阅模式实现松耦合 134
### 建议53：用状态模式美化代码 137

## 第6章　内部机制 141
### 建议54：理解built-in objects 141
### 建议55：__init__()不是构造方法 143
### 建议56：理解名字查找机制 147
### 建议57：为什么需要self参数 151
### 建议58：理解MRO与多继承 154
### 建议59：理解描述符机制 157
### 建议60：区别__getattr__()和__getattribute__()方法 160
### 建议61：使用更为安全的property 164
### 建议62：掌握metaclass 169
### 建议63：熟悉Python对象协议 176
### 建议64：利用操作符重载实现中缀语法 179
### 建议65：熟悉 Python 的迭代器协议 181
### 建议66：熟悉 Python 的生成器 185
### 建议67：基于生成器的协程及greenlet 188
### 建议68：理解GIL的局限性 192
### 建议69：对象的管理与垃圾回收 194

## 第7章　使用工具辅助项目开发 197
### 建议70：从PyPI安装包 197
### 建议71：使用pip和yolk安装、管理包 199
### 建议72：做paster创建包 202
### 建议73：理解单元测试概念 209
### 建议74：为包编写单元测试 212
### 建议75：利用测试驱动开发提高代码的可测性 216
### 建议76：使用Pylint检查代码风格 218
### 建议77：进行高效的代码审查 221
### 建议78：将包发布到PyPI 224

## 第8章　性能剖析与优化 227
### 建议79：了解代码优化的基本原则 227
### 建议80：借助性能优化工具 228
### 建议81：利用cProfile定位性能瓶颈 229
### 建议82：使用memory_profiler 和 objgraph 剖析内存使用 235
### 建议83：努力降低算法复杂度 237
### 建议84：掌握循环优化的基本技巧 238
### 建议85：使用生成器提高效率 240
### 建议86：使用不同的数据结构优化性能 243
### 建议87：充分利用set的优势 245
### 建议88：使用multiprocessing克服GIL的缺陷 248
### 建议89：使用线程池提高效率 254
### 建议90：使用C/C++模块扩展提高性能 257
### 建议91：使用 Cython 编写扩展模块 259
