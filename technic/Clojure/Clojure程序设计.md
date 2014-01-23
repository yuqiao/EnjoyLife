# Clojure 程序设计

我们正在被复杂性淹没,其中绝大部分是偶然复杂性 --并非源自问题本身,而是源自于我们为了解决问题而采用的方法.

函数式编程提供了别的选择. 通过强调纯函数-传入和返回的都是不可变值,副作用的存在成为了特例,而不是常态.

Clojure的设计目标:就是让函数式编程更加平易近人,且兼备商业软件开发者所需的实用性.

# 1 启航

# 2 探索Clojure

# 3 一切皆序列

在最底层, 程序面对的是字符串,列表,向量,映射表, 集合和树这样的数据结构.

在Clojure中, 所以这些序列都可以通过一个抽象概念来访问: 序列(seq, 发音:seek).


## 3.1　一切皆序列
三大核心能力:

- (first aseq)
- (rest aseq)
- (cons elem aseq)  # 构建一个序列.

序列函数rest, cons返回总是序列.

键值对也可算作序列的元素, 映射表也可作为序列.

映射表和集合是稳定的, 但这个顺序取决于实现细节. 若想要可靠的顺序, 可使用sorted-set, sorted-map:
    
    (sorted-set :the :quick :brown :fox)
    (sorted-map :c 3 :b 2 :a 1)

还有两函数:

- conj: 向容器加入一个或多个元素. (conj [1 2 3] :a)
- into: 把容器里的所有元素加入到另一容器中. (into [1 2 3] [:a :b :c])

clojure的序列库特别适合于那些庞大的序列. 绝大多数是惰性的.
clojure序列是不可变的.

## 3.2　使用序列库

### 3.2.1　创建序列
- (range start? end step?)
- (repeat n x), (repeat x)
- (iterate f x)

    (defn whole-numbers (iterate inc 1))

- (cycle coll)
- (interleave& colls) 接受多容器为参数, 并参数一个新容器, 新容器会从每个参数容器中交错地提取元素.
- (interpase seperator coll) 把输入序列coll用分隔符seperator隔开.
    
    (apply str (interpose \, ["apples", "bananas" "grapes"]))  "被封装成clojure.string/join

- 对应clojure的容器类型, 都有一个接受任意数量参数的函数, 创建该类型的容器.

    - (list_& elementes)
    - (vector_& elementes)
    - (hash-set_& elementes)
    - (hash-map_& key-1 val-1 ...)
    - (set coll)
    - (vec coll)

### 3.2.2　过滤序列

- (filter pred coll)

    (take 10 (filter even? (whole-numbers)))

- (take-while pred coll) 从序列中截取开头的一段, 其每个元素都被谓词判定为真.

    (take-while (complement #{\a\e\i\o\u}) "the-quick-brown-fox")

- (drop-while pred coll)
- (split-at index coll)
- (split-with pred coll)

take-, splite-和drop开头的函数,返回的都是惰性序列.

complememt 会反转另一个函数的行为.

### 3.2.3　序列谓词

- every?
- some
- not-every?
- not-any?

### 3.2.4　序列转换

- (map f coll)

    (map #(format "<p>%s</p>" %) ["the" "quick" "brown" "fox"] )

- (reduce f coll)

    (reduce + (range 1 11))

- (sort comp? coll)
- (sort-by a-fn comp? coll)
- 列表解析: (for [binding-form coll-expr filter-expr? ...] expr)

    (for [word ["the" "quick" "brown" "fox"]]
        (format "<p>%s</p>" word))

    (take 10 (for [n (whole-numbers) :when (even? n)] n))


## 3.3　惰性和无限序列

大多数clojure序列都是惰性的, 直到需要时,元素才会计算出来. 好处:

- 推迟那些实际上并不需要的昂贵计算.
- 可以出来超出内存允许范围的庞大数据集.
- 将I/O推迟至确实需要时才进行.

使用doall, dorun迫使序列求值:

    (def x (for [ x (range 1 3)] (do (println i) i )))

    (doall x)

    (dorun coll)


## 3.4　java亦可序化

对于序列的first/rest抽象, 可套用到任何"多于一个"的事物之上.在java世界中:

- 容器API
- 正则表达式
- 文件系统遍历
- XML处理
- 关系型数据库结果集

### 3.4.1　序化java容器

1. 数组:

    (first (.getBytes "hello"))
    (rest (.getBytes "hello"))
    (cons (int \h) (.getBytes "ello"))


2. 哈希表(Hashtabel)和映射(Map):

    (first (System/getProperties))
    (rest (System/getProperties))

**牢记:** 即使底层的java容器是可变的, 但经过包装后的序列却是不可变的.
所以你不可能通过用cons把属性添加到System/getProperties.

Clojure会把容器自动包装为序列, 但却不会自动拆包为它们的原始类型. 所以字符串反转:

    (apply str (reverse "hello"))


### 3.4.2　序化正则表达式

Clojure的内部正则表达式使用了java.util.regex库.

使用较为高级的re-seq.

    (re-seq #"\w+" "the quick brown fox")
    (map #(.toUpperCase %) (re-seq #"\w+" "the quick brown fox"))



### 3.4.3　序化文件系统

可以直接使用java.io.File::

    (import '(java.io File))
    (.listFiles (File. "."))

通过file-seq提供了一个深度优先的遍历方式:

    (count (file-seq (File. ".")))

检查最近半小时里,文件是否被改动:

    (defn minutes-to-millis [mins] (* mins 1000 60))
    (defn recently-modified? [file]
        (> (.lastModified file)
            (- (System/currentTimeMillis) (minutes-to-millis 30))))
    (filter recently-modified? (file-seq (File. ".")))

### 3.4.4　序化流

使用line-seq将java的Reader以行的方式进行序化:

    (use '[clojure.java.io :only (reader))
    (take 2 (line-seq (reader "src/expamles/utils.clj")))

由于reader代表非内存资源, 需要被明确关闭, 应该把Reader的创建包在with-open中.

    (with-open [rdr (reader "src/examples.clj")]
        (count (line-seq rdr)))

可以创建一些有趣的工具:

    (use '[clojure.java.io :only (reader))
    (defn non-blank? [line] (if (re-find #"\S" line) true false))
    (defn non-svn? [file] (not (.contains (.toString file) ".svn")))
    (defn clojure-source? [file] (.endsWith (.toString file) ".clj")))
    (defn clojure-loc [base-file]
        (reduce 
            +
            (for [file (file-seq base-file)
                  :when (and (clojure-source? file) (non-svn? file))]
             (with-open [rdr (reader file)]
                (count (filter non-blank? (line-seq rdr)))))))


### 3.4.5　序化xml

clojure.xml/parse函数能解析xml文件, 流或url, 并以clojure的映射表的形式,返回数据的树形结构:

    (use '[clojure.xml :only (parse)] )
    (parse (java.io.File. "data/composition.xml"))

使用xml-seq提取作曲家信息:

    (for [x (xml-seq 
          (parse (java.io.File. "data/composition.xml")))
          :when (= :composition (:tag x))]
     (:composor (:attrs x)))


## 3.5　调用特定于结构的函数

### 3.5.1　列表函数

:
    
    (peek '(1 2 3) )  -> 1
    (pop '(1 2 3)) -> (2 3)

peek等同于first, 但pop则与rest不同.若空序列, pop会抛出一个异常.

### 3.5.2　向量函数

peek从末尾开始取:

    (peek [1 2 3])   ; -> 3
    (pop [1 2 3])    ; -> [1 2]

get方法:

    (get [:a :b :c] 1) ->  :b
    (gee [:a :b :c] 5) -> nil

向量自身也是方法:

    ([:a :b :c] 1) -> :b
    ([:a :b :c] 5) -> 抛出异常

assoc在指定位置关联一个新值:

    (assoc [0 1 2 3 4] 2 :two) -> [0 1 :two 3 4]

subvec会返回子向量:

    (subvec [1 2 3 4 5] 3 ) -> [4 5]
    (subvec [1 2 3 4 5] 1 3 ) -> [2 3]

subvec的性能比take和drop组合快一些.


### 3.5.3　映射表函数

获取键序列:

    (keys map)
    (vals map)

get:

    (get map key value-if-not-found?)

映射表本身是一个函数:

    ({:subdance "spaniel", :darwin "beeagle"} :darwin) -> "beeagle"
    ({:subdance "spaniel", :darwin "beeagle"} :snoopy) -> nil

关键字同样也是函数:

    (:darwin {:subdance "spaniel", :darwin "beeagle"} ) -> "beeagle"
    (:snoopy {:subdance "spaniel", :darwin "beeagle"} ) -> nil

无法确认键对应的值为nil, 可以使用contains?来确定是否存在:

    (contains? map key)

### 3.5.4　集合函数

## 3.6　小结

clojure把所有类型的容易统一成了一个抽象 -- 序列.

clojure序列是使用函数式编程技术实现的: 不可变数据, 递归定义和惰性变现(lazy realization).

# 4 函数式编程

## 4.1 函数式编程理念

纯函数

持久性数据结构

惰性和递归

引用透明性: 在任何时刻,都可以用函数的结果来取代对其的调用.

FP的优势

6条规则:
- 避免使用直接递归
- 当产生的是标量(scalar values),或是体积小还数量固定的序列时,可用recur
- 当产生个头大,大小可变的序列时,让它成为惰性的,而不要用递归.
- 小心不要让一个惰性序列变现太多,超出你的需要.
- 熟悉序列库.
- 细分.把看似简单的问题也尽可能划分为更小的块.

规则5和6尤为重要.尽可能使用第3章技术.

## 4.2 怎么偷懒

递归定义两部分组成:

- 基础(basis), 明确地列举序列的部分成员.
- 归纳(induction), 提供一些规则, 规定了如何通过组合序列的成员,来产生更多的成员.

实现方式:

- 简单的递归.
- 尾递归
- 惰性序列

在clojure中,选择惰性化往往是正确的选择.

斐波那契数列实现.

糟糕的使用:

    (defn stack-consuming-fibo [n]
        (cond
            (= n 0) 0
            (= n 1) 1
            :else (+ (stack-consuming-fibo (- n 1)) 
                    (stack-consuming-fibo (- n 2)))))

stack-consuming-fibo会创深度与n正比的栈帧.

可以使用尾递归技术来解决栈空间的问题:

    (defn tail-fibo [n]
        (letfn [ (fib
                    [current next n]
                        (if (zero? n)
                            current 
                        (fib next (+ current next) (dec n))))]
        (fib 0N 1N n)))

对应大一点的n值,仍然失败. 因为问题来源于java虚拟机, 它缺乏TCO.


### 自递归

使用recur:

    (defn recur-fibo [n]
        (letfn [ (fib
                    [current next n]
                        (if (zero? n)
                            current 
                        (recur next (+ current next) (dec n))))]
        (fib 0N 1N n)))

### 惰性序列

使用lazy-seq宏创建.

## 4.3 懒上加懒

## 4.4 再议递归



# 5 状态

状态: 就是在某一个时间点上一个标识所代表的值.

值: 是一个不可变的持久数据结构.

Clojure引用模型将标识和值清晰地区分开来. Clojure提供了四种引用类型.

- 引用(REf), 负责协同地, 同步地更改共享状态.
- 原子(Atom), 负责非协同地, 同步地更改共享状态.
- 代理(Agent), 负责异步地更共享状态
- 变量(Var), 负责线程内的状态.

## 5.1 并发,并行和锁定

并发: 模拟了有许多事情同时发生.

并行: 拆分单独的小块, 而这些小块可同时执行, 以加速整个执行过程.

并发中, 会有不止一个线程在窥视着你的数据.

Clojure把模型分成两部分:
- 函数式编程, 没有可变状态.
- 引用模型.

## 5.2 引用与软事务内存

Clojure大部分对象都是不可变的. 若想要可变数据, 需要明确表示出来:

    (def current-track (ref "Mars, the Bringer of War"))

读取引用的内容, 调用deref或@:

    (deref current-track)   ; @current-track

使用ref-set来改变引用的指向, 必须包裹在dosync之中:

    (dosync (ref-set current-track "Venus, The Bringer of Peace"))

### 事务的属性

与数据库事务一样, STM事务具有一些重要的特性:

- 更新是原子的. 
- 更新是一致的. 可以为引用指定验证函数, 若函数中有失败, 整个事务将失败.
- 更新是隔离的. 无法看到来自于其他事务的局部完成结果.

数据库停供了一个额外的保证, 更新是持久的.

### alter

简单的聊天应用例子:

    (defrecord Message [sender text])
    (user.Message. "Aaron" "Hello")
    (def messages (ref ()))

添加一条消息:

    (defn add-message [msg]
        (dosync (alter messages conj msg)))

    (add-message (user.Message. "user 1" "hello"))
    -> (#user.Message{:sender "user 1", :text "hello")
    
### STM的工作原理: MVCC

MVCC(Multiversion Concurrency Control)多版本并发控制技术.

事务A启动时获取一个"起始点", 在事务A中访问任何一个引用, 实际上访问的是这个引用与起始点相关的一个高效复本.
在任何时间点, 若STM检测到其他事务设置或更改了某个引用, 而事务A正好也想设置或更改, 那么事务A被迫重来.
若dosync块中抛出来一个异常,那么事务A会终止,而非重试.

### commute

commute是一个特殊的alter变体, 允许更多并发. commutatiave(可交换的), 更新操作必须能以任何次序出现.

当天你尝试进行commute时, 另一个事务偷偷更改了引用, STM不会重启你的整个事务, 相反只是简单地再次运行你的commute函数.

### alter优先

### 为引用添加验证

创建引用时指定一个验证函数:

    (def validate-message-list (partial every? #(and (:sender %) (:text %))))
    (def messages (ref () :validator validate-message-list0))

    (add-message "not a valid message") -> Excpetion..

    (add-message (user.Message. "user 1" "hello"))
    -> (#user.Message{:sender "user 1", :text "hello")
    

## 5.3 使用原子

使用atom创建一个原子:

    (def current-track (atom "Venus, the Bringer of Peace"))

不需要dosync, 使用reset!:

    (reset! current-track "Creado")

或使用swap!:

    (def current-track (atom {:title "Credo" :composer "Byrd"}))
    (swap! current-track assoc :title "Sancet Deus") ->  {:title "Sancet Deus" :composer "Byrd"}


## 5.4 使用代理进行异步更新

: 
    (def counter (agent 0))
    (send count inc)

    (await & agents )
    (await-for timerout-millis & agents)

代理也可以携带一个验证函数:

    (def counter (agent 0 :validator numbers?))

使用agent-errors, 找具体的错误:

    (send counter (fn [_] "boo"))
    @counter -> java.lang.Exception ...

    (agent-errors counter)

清理错误:

    (clear-agent-errors agent)
    @counter -> 0

### 在事务中包含代理

有时候你会希望事务成功后, 能做一些副作用的事情. 若你在事务中向代理发送一个动作, 那么这个动作仅当事务成功时才会真正被发送.

send-off是send的一个变体.

# 6 协议和数据结构

# 7 宏

# 8 多重方法

# 9 极尽java之所能

# 10 搭建应用

