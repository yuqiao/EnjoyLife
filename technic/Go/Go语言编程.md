# Go语言编程

# 1. 初始go语言

## 1.6 问题追踪和调试

打印日志, 使用fmt,log.

gdb调试.


# 2. 顺序编程

## 2.1 变量

变量声明:

	var v1 int
	var v2 string
	var v3 [10]int
	var v4 []int
	var v5 struct {
		f int
	}
	var v6 *int
	var v7 map[string] int
	var v8 func(a int) int

	var (
		v1 int
		v2 string
	)

变量初始化:

	var v1 int = 10
	var v2  = 10
	v3 := 10

变量赋值:

	var v10 int
	v10 = 123

	i, j = j, i 

匿名变量: _

## 2.2 常量

字面常量:

	-12
	3.141592653
	3.2+12i
	true
	"foo"

常量定义:

	const Pi float64 = 3.14159
	const zero = 0.0
	const (
		size int64 = 1024
		eof = -1
	)
	const u, v float32 = 0, 3
	const a, b, c = 3, 4, "foo"

	const mask = 1<<3

常量的赋值是一个编译器行为, 所以右值不能出现任何需要运行期才能得出结果的表达式. 

预定义常量: true, false 和 iota. 

枚举:

	const {
		Sunday = itoa
		Monday
		Tuesday
		...
	}

## 2.3 类型

Go内置以下基础类型:

- bool类型: bool
- 整型: int8, byte, int16, int, uint, uintptr
- 浮点类型: float32, float64
- 复数类型: complex64, complex128
- 字符串: string
- 字符类型: rune
- 错误类型: error

复合类型:

- 指针
- 数组
- 切片
- 字典
- 通道
- 结构体
- 接口

### 字符串

len(s) 获取字符串长度. 
"Hello" + "123" // = "Hello123"
s[i]

字符串遍历. 

以字节数遍历:

	str := "Hello, 世界"
	n := len(str)
	for i:=0;i<n; i++ {
		ch := str[i]
		fmt.Println(i, ch)
	}

以Unicode字符遍历:

	for i, ch := range str {
		fmt.Println(i, ch)
	}	


# 3. 面向对象编程

## 3.1 类型系统

- 基础类型
- 复合类型, 数组, 结构体, 指针
- 可以指向任意对象的类型: Any
- 值语义和引用语义
- 面向对象类型
- 接口

### 为类型添加方法

例子:
	
	type Integer int

	func (a Integer)Less(b Integer) bool {
		retuan a < b	
	}

	func main() {
		var a Integer = 1
		if a.Less(2) {
			fmt.Println(a, "Less 2")
		}
	}

若需要修改对象, 需要用指针引用:

	func (a *Integer)Add() {
		*a += b
	}

### 结构体



## 3.2 初始化

## 3.3 匿名组合

## 3.4 可见性

## 3.5 接口

## 3.6 完整实例

## 3.7 小结


# 4. 并发编程

## 4.1 并发基础
主流的实现模型:

- 多进程
- 多线程
- 基于回调的非阻塞/异步IO
- 协程

线程+共享内存的方式

消息传递系统, 代表: Erlang

## 4.2 协程

优势在于"轻量级". 

缺点:
- 依赖库, 非语言原生. 
- 调用同步IO操作(网络通信, 本地文件读写), 都会阻塞其它的并发执行轻量级线程, 无法达到真正期望的目标. 

## 4.3 goroutine

简单, 例子:

	func Add(x, y int) {
		z := x + y
		fmt.Println(z)
	}

	func main() {
		go Add(1, 1)
	}

## 4.4 并发通信

并发编程的难度在于协调. 

并发通信的两种模型: 共享数据, 消息. 

## 4.5 channel




# 5. 网络编程

## 5.1 Socket编程

### Dial函数
- tcp: 

	conn, err := net.Dial("tcp", "192.168.0.10:2100")

- udp:

	conn, err := net.Dial("udp", "192.168.0.12:975")	

- icmp:

	conn, err := net.Dial("ip4:icmp", "www.baidu.com")	

使用conn的Write方法发送， 使用Read方法接收.

- read:
	
	func readFully(conn net.Conn) ([]byte,  error) {
		defer conn.Close()

		result := bytes.NewBuffer(nil)
		va buf [512]byte
		for {
			n, err := conn.Read(buf[0:])
			result.Write(buf[0:n])
			if err != nil {
				if err == io.EOF {
					break
				}
				return nil, err
			}

		}
		return result.Bytes(), nil
	}	

- write:

	_, err = conn.Write([]byte("HEAD / HTTP/1.0\r\n\r\n")

### 其它方法：

- 验证IP地址有效性： func net.ParseIP()
- 创建子网掩码： func IPv4Mask(a, b, c, d byte) IPMask
- 获取默认子网掩码： func (ip IP) DefaultMask() IPMask
- 根据域名查找IP:

	func ResolveIPAddr(net, addr string) (*IPAddr, error)
	func LookupHost(name string)(cname string, addrs []string, err error)

## 5.2 HTTP编程

### HTTPk客户端

- Get ：

	resp, err = http.Get("http://www.baidu.com")
	if err != nil {
		// process error...
		return
	}

	defer resp.Body.close()
	io.Copy(os.Stdout, resp.Body)

- Post:

	resp, err = http.Post("http://example.com/upload", "image/jpeg", &imageDtaBuf)
	if err != nil {
		// process error ...
		return
	}

	if resp.StatusCode != http.StatusOK {
		// process error ...
		return
	}
	// ...

- PostForm():	

	resp, err = http.PostForm("http://example.com/posts", url.Values{"title": {"articlae title"}, "content":{"article body"} })
	if err != nil {
		// process error ...
		return
	}

- Head():

	resp, err = http.Head("http://www.baidu.com")

- （*http.Client).Do()

	client := &http.Client {
		CheckRedirect: redirectPolicyFunc,
	}
	resp, err := client.Get("http://example.com")
	req, err := http.NewRequest("GET", "http://example.com", nil)
	req.Header.Add("User-Agent", "Our Custom User-Agent")
	req.Header.Add("If-None-Match", `W/"TheFileEtag"`)
	resp, err := client.Do(req)

- 自定义http.Transport	

- 灵活的http.RoundTripper接口

### HTTP服务端



## 5.3 RPC编程

## 5.4 JSON处理

## 5.5 网站开发

## 小结


# 6. 安全编程

# 7. 工程管理

# 8. 开发工具

# 9. 进阶话题

## 9.1 调试

go build -gcflags "-N -l" // 编译， 关闭内联优化
gdb demo
l main.main
l main.go:8
b main.main
b main.go:17
info breakpoints
info goroutines
goroutine 1 bt  // 查看指定序号的goroutine调用堆栈
info frame
info locals
whatis i
x/3xw &r     // 查看 r内存数据（
