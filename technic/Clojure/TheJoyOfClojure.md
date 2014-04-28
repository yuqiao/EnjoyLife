 # Clojure编程乐趣

# 基础

# 1. Clojure 哲学

## 1.1 clojure 之道
- 简单
- 专注
- 实用
- 清晰
- 一致

## 1.2 为何lisp
- 优美
- 极度灵活
- 代码即数据

## 1.3 函数式编程

## 1.4 clojure为何不是面向对象的


# 2. clojure的疾风式教程

## 2.1 标量

## 2.2 组合起来: 集合

## 2.3 付诸实现: 函数 

## 2.4 var

## 2.5 局部量,循环和block

## 2.6 防止发生:quote

例子:

    '(1 2 3)
    `(1 2 3)

反quote, "~":

    (let [x 2] `(1 ~x 3))

## 2.7 与java互操作

访问静态成员:

    (Math/sqrt 9)

创建java实例:

    (java.util.HashMap. {"foo" 42 "bar" 9})

访问java实例成员:
    
    (.x (java.awt.Point. 10 20))

使用set!设置java实例属性:

    (let [origin (java.awt.Point. 0 0)]
        (set! (.x origin) 15)
        (str origin))

宏 .. :

    user=> (.. (java.util.Date.) toString (endsWith "2013"))
    true

宏doto

定义类.

## 2.8 异常环境

抛出与捕获:

    (throw (Exception. "I done throwed"))

    (def throw-catch [f]
        [(try
            (f)
            (catch ArithmeticException e "No deviding by zero!")
            (catch Exception e (str "You are so bad " (.getMessage e)))
            (finally (println "returning ...")))])

## 2.9 命名空间

使用ns创建命名空间:

    (ns joy.ch)

使用*ns*代替当前的命名空间:

    (defn report-ns [] (str "The current namespace is " *ns*))

使用:require加载其他命名空间:

    (ns joy.require
        (:require clojure.set))

    (clojure.set/intersection #{1 2 3} #{3 4 5})  -> #{3}

用:use加载和创建映射:

    (ns joy.use 
        (:use [clojure.string :only [capitalize]]))

    (map capitalize ["kilgore" "trout"])

    (ns joy.use 
        (:use [clojure.string :exclude [capitalize]]))


使用:refer创建映射:

    (ns joy.yet-another
        (:refer joy.ch1 {hello hi}))

用:import加载java类

    (ns joy.java 
        (:import [java.util HashMap]
                 [java.util.concurrent.atomic AtomicLong]))
    (HashMap. {"happ?" true})
    (AtomicLong. 42)

# 3 小试牛刀

## 3.1 真值

### 什么是真

对于if而言, 处理false和nil, 所有值看上去都和true一样. 
意味着零长度字符串, 空list, 数字0等都被当成true.

### 不要创建布尔对象

### nil vs. false

## 3.2 小心翼翼nil双关

使用seq作为终止条件:

    (defn print-seq [s]
        (when (seq s)
            (prn (first s))
            (recur (rest s))))

## 3.3 解构

解构vector:

    (def guys-whle-name ["Guy" "Lewis" "Steels"])

    (let [ [f-name m-name l-name] guys-whole-name]
        (str f-name ", " m-name " " l-name))


    (let [ [ a b c & more ] (range 10)]
        (println "a b c are:" a b c )
        (println "more is:" more ))

vector解构:as :

    (let [  range-vec (vec (range 10))
            [ a b c & more :as all ] range-ec]
        (println "a b c are:" a b c )
        (println "more is:" more )
        (println "all is:" all))


解构map:

    (def guys-name-map {:f-name "Guy", :m-name "Lewis", :l-name "Steels"})

    (let [{f-name :f-name, m-name :m-name, l-name :l-name} guys-name-map]
        (str f-name ", " m-name " " l-name))
    
简洁的:

    (let [{:keys [f-name m-name l-name]} guys-name-map]
        (str f-name ", " m-name " " l-name))

可以用:or提供默认值.

# 3.4 用REPL做试验

查找文档:

    (find-doc "xor")

打开java文档:
    
    (def frame (java.awt.Frame.))
    (javadoc frame)

# 第二部分 数据类型

# 4 标量

## 4.1 理解精度

## 4.2 有理数

## 4.3 使用关键字的时机

关键字总是指向自身, 用途:
1. 作为键值
2. 作为枚举
3. 多重方法的分发值
4. 指令

### 限定关键字

## 4.4 符号解析

符号: 引用其他事物.

元数据

符号和命名哦老规矩.

## 4.5 正则表达式

### 小心可变匹配器(Matcher)

## 4.6 总结

有时候会遇到数字精度问题.

# 5 组合数据类型

## 5.1 持久化, 序列和复杂度

持久化: 拥有特定属性的不变内存集合.

一个数据抽象有100个函数操作, 好于10个数据结构各有10个函数操作.

sequential(顺序的)集合: 持有一系列值而不重新排序的集合.
seuence(序列): 一种顺序集合, 表示一系列可能存在也可能不存在的值.

三种逻辑分类:

- 顺序的
- set
- map

若两顺序集合有相同的值且顺序相同, =就会返回true.

## 5.2 vector

### 构建vector

用vector表示let, with-open, fn等的绑定是Clojure的一种惯用法, 一种模式.

1. 使用函数vec创建:

    (vec (range 10)) ; => (0 1 2 3 4 5 6 7 8 9)

2. into:

    (let [my-vector [:a :b :c]] 
        (into my-vector (range 10))) ; 要返回一个vector, into的第一个参数必须是vector.

3. 原生类型vector:

    (into (vector-of int) [Math/PI 2 1.3])  ;=> [3 2 1]
    (into (vector-of :char) [ 100 101 102]) ;=> [\d \e \f]

### 大vector

相对于list, vector在三个方面比较高效:

- 在集合右端添加或删除
- 通过数字索引访问或修改集合内的项.
- 反向遍历

(def a-to-j (vec (map char (range 65 75))))

三种查找方式:

    (nth a-to-j 4)
    (get a-to-j 4)
    (a-to-j 4)


使用assoc函数修改. 

    (assoc a-to-j 4 "no longer E")

### vector当做栈

使用conj和pop, 例子:

    (def my-stack [1 2 3])
    (peek my-stack)  ;=> 3
    (pop my-stack)   ;=> [1 2]
    (conj my-stack 4) ;=> [1 2 3 4]

### 使用vector而非reverse
vector可以在右边高效地增长.

(defn strict-map2 [f coll]
    (loop [coll coll, acc []]
        (if (empty? coll)
            acc
            (recur (next coll) (conj acc (f (first coll)))))))

### 子vector

虽然vector中的项无法高效移除(除了最后一项), 但子vector提供一种快速方式.
(subvec a-to-j 3 6)

### vector当做MapEntry

(vector? (first {:width 10, :height 20, :depth 15}))

(doseq [[dimension amount] {:width 10, :heght 20, :depth 15}]
    (println (str (name dimension) ":") amount "inches"))


## vector 不是什么

- vector不是稀疏的, 虽然可以替换, 却不能插入或者删除.
- vector不是队列.
- vector不是set


## 5.3 list: Clojure代码form的数据结构

(cons 1 '(2 3) ) ; => (1 2 3)
(conj '(2 3) 1) ;=> (1 2 3)

推荐使用conj.

- list 不能用索引.
- list 不是set.
- list 不是队列.

## 5.4 如何使用持久化队列
PersistentQueue

## 5.5 持久化set

## 5.6 思考map

## 5.7 知识汇总: 在序列里查找某项位置
(defn index [coll]
    (cond
        (map? coll) (seq coll)
        (set? coll) (map vector coll coll)
        :else (map vector (iterator inc 0) coll)) )

(defn pos [pred coll]
    (for [ [i v] (index coll) :when (pred v)] i))        

# 6 惰性与不变性


## 6.1 关于不变性

## 6.2 设计一个持久化玩具

## 6.3 惰性 

# 7 函数式编程

# 8 宏

# 9 组合数据与代码

# 10 Java.next

# 11 变化

# 12 性能 

# 13 Clojure 改变我们的思考方式


