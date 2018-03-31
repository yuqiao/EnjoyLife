# Git 权威指南

# 2. 爱上git的理由

## 2.1 每日工作备份

1. git add -u && git commit 
2. git push # 提交到公网的git服务器
3. 每次向内网服务器推送的时候，自动从内网服务器同步到外网Git镜像

鸡蛋分别装在了三个篮子里。

## 2.2 异地协同工作

公司，家里两地工作。

1. 在家里工作，将mirror版本库中数据同步到本地：git pull mirror master
2. 在家里电脑编写并提交。git push home
3. 到公司了。从home版本将家里的提交同步到公司电脑上： git pull home master

## 2.3 现场版本控制

现场版本控制： 在客户现场或在产品不是的现场进行产品源代码的修改， 并在修改过程中产生版本控制，以便修改后能将结果过程一并带走，并合并到项目对应的代码库中。

1. 创建现场版本库： git init
2. 添加文件并提交： git add -A && git commit -m "intialized"
3. 为初始提交建立一个里程碑: git tag v1
4. 开始在工作区中工作 -- 修改文件并提交: git commit -a
5. 对修改结果满意并想将工作成果保存带走， 可使用下面命令将从v1开始的历次提交逐一导出为补丁文件： git format-patch v1..HEAD

## 2.4 避免引入辅助目录

## 2.5 重写提交说明

* 修改最新提交的提交说明:  git commit --amend
* 修改某个历史的提交说明:  git rebase -i <commit-id>^

## 2.6 想吃后悔药

* git rm --cached winxp.img
* git commit --amend

## 2.7 更好用的提交列表

正确的版本控制系统的使用方法是： 一次提交只干一件事；或是完成一个新功能，或是修改了一个Bug.



# 第二篇 Git独奏
Git是分布式版本控制系统, 与集中式版本控制系统有巨大差异.
**"你应该了解真相,真相会使你自由".**

# 4. git初始化
## 4.1 创建版本库及第一次提交
一 需要设置Git的配置变量.

1. 告知git用户和邮件地址
2. 设置git别名,以便使用更简洁的命令.
3. 在git命令输出中开启颜色显示:
    $ git config --global conlor.ui true

二 创建git版本库.

1. 创建目录
2. 进入目录, 执行git init.

隐藏的.git目录就是Git版本库.

## 4.2 思考: 为何有.git目录

svn目录结构特点:

1. 不仅包含配置文件, 还包含当前工作区下每个文件的拷贝.使得svn子命令可以脱离版本库执行,而且客户端提交时,可以只提交改动的部分. 缺点: 加倍占用工作区的空间.
2. 搜索结果很乱.

git这种将版本库放在工作区根目录下的设计优点:

1. 所有操作(除了远程版本库操作)都可以在本地完成. 不像svn只有寥寥无几的命令能脱离网络.
2. 没有svn安全泄露问题.
3. 没有本地搜索结果很乱的问题. git grep 命令.

在git工作区的某个子目录下执行操作的时候,会在工作区目录上依次向上递归查找.git目录.找到的.git目录就是工作区对应的版本库.

使用strace命令跟踪git status命令时的磁盘访问:
    strace -e "trace=file" git status

.git/index 记录了工作区文件的状态(暂存区的状态)

- 显示版本库.git目录所在的位置:
    git rev-parse --git-dir
- 显示工作区目录:
    git rev-parse --show-toplevel
- 相对于工作区的相对目录:
    git rev-parse --show-prefix
- 显示从当前目录到工作区的深度:
    git rev-parse --show-cdup

git克隆可以降低因为版本库和工作区混杂在一起导致版本库被破坏的风险.

## 4.3 思考: git config 命令的各参数区别

- git config -e :  版本库 demo/.git/config文件
- git config -e --global: /home/rongqiao.yurq/.gitconfig
- git config -e --system: /etc/gitconfig

git配置文件采用INI文件格式.

## 4.4 思考: 是谁完成了提交
清空配置:

- git config --unset --global user.name
- git config --unset --global user.email

提交:

    git commit --allow-empty -m "who does commit?"

看日志:

    git log --pretty=fuller

恢复user.name和user.email. ...

对刚刚提交进行修补:

    git commit --amend --allow-empty --reset-author

--amend: 对刚刚的提交进行修补.

## 4.5 思考: 随意设置提交者姓名,是否不太安全

## 4.6 思考: 使用别名

git config --global alias.ci "commit -s"

## 4.7 备份本章成果

git clone demo demo-step-1

# 5. Git暂存区

git log --stat (--stat参数可以看到每次提交的文件变更统计)

暂存区是一个介于工作区和版本库的中间状态, 当执行提交时, 即将暂存区的内容提交到版本库中.

## 5.1 修改不能直接提交

状态精简格式输出:

    git status -s

git log --pretty=oneline

git diff --cached / git diff --staged

## 5.2 理解暂存区(stage)

.git/index实际上是一个包含文件索引的目录树.像是一个虚拟的工作区, 在这个虚拟的工作区的目录树中, 记录了文件名和文件的状态信息(时间戳和文件长度等). 文件的内容并没有存储在其中, 而是保存在Git对象库.git/objects目录中, 文件索引建立了文件和对象库中对象实体之间的对应.

- HEAD实际是指向master分支的一个"游标", 可以当做Master.
- git add时, 暂存区的目录树将被更新, 同时工作区修改(或新增)的文件内容会被写入对象库中的一个新对象中, 而该对象的ID被记录在暂存区的文件索引中.
- 执行git commit时,暂存区的目录树会写到版本库(对象库)中, master分支会做相应的更新, 即Master的最新指向的目录树就是提交时原暂存区的目录树.
- git reset HEAD时, 暂存区的目录树会被重写,会被master指向的目录树所替换, 但是工作区的目录树不受影响.
- git rm --cached <file>命令时, 会直接从暂存区删除文件,工作区则不改变.
- git checkout 或 git checkout -- <file> 命令时, 会用暂存区全部的文件或指定的文件替换工作区的文件. **操作很危险.** 会清除工作区中未添加到暂存区的内容.
- git checkout HEAD <file>, 会用HEAD指向的master分支中的全部或部分文件替换暂存区和工作区中的文件, **极具危险性**

## 5.3 git Diff 魔法
### 1. 工作区,暂存区和版本库的目录树浏览

查看HEAD指向的目录树:

    $ git ls-tree -l HEAD
    100644 blob fd3c069c1de4f4bc9b15940f490aeb48852f3c42      25    welcome.txt

- -l 参数可以显示文件的大小.
- 输出格式: 文件属性(rw-r--r--), git对象库中的一个blob对象, 该文件在对象库中对应的ID(40位的SHA1哈希格式的ID), 文件大小, 文件名

清除工作区当前的改动:
    
    $ git clean -fd   # 清除当前工作区没用加入版本库的文件和目录

用暂存区内容刷新工作区:

    $ git checkout .

对工作区修改:

    $ echo "Bye-Bye." >> welcome.txt
    $ mkdir -p a/b/c
    $ echo "Hello." >> a/b/c/hello.txt
    $ git add .
    $ echo "Bye-Bye." >> a/b/c/hello.txt
    $ git status -s

查看工作区文件大小:
    
    find . -path ./.git -prune -o -type f -print | xargs ls -l
    -rw-r--r--  1 Qiao  staff  15 12 12 17:07 ./a/b/c/hello.txt
    -rw-r--r--  1 Qiao  staff  34 12 12 17:05 ./welcome.txt

显示暂存区的目录树: 

    $ git ls-files -s
    100644 e965047ad7c57865823c7d992b1d046ea66edf78 0   a/b/c/hello.txt
    100644 51dbfd25a804c30e9d8dc441740452534de8264b 0   welcome.txt

上面第三个字段不是文件大小而是暂存区编号.

若需要对暂存区目录树使用git ls-tree命令, 需要先讲暂存区的目录树写入git对象库(git write-tree命令), 然后用git ls-tree.

    $ git write-tree
    375f8181f28bf2f69f95f429f8ddc78f07ed3210
    $ git ls-tree -l 375f
    040000 tree e56a4d15295d3754310f114c86d93645308110ad       -    a
    100644 blob 51dbfd25a804c30e9d8dc441740452534de8264b      34    welcome.txt

递归显示目录树内容:

    $ git ls-tree -l -r -t 375f
    040000 tree e56a4d15295d3754310f114c86d93645308110ad       -    a
    040000 tree 6d2ce67bdd55ae2c2ac72cbc879ed7c67ecc9786       -    a/b
    040000 tree 8c3c7fbcd903744b20fd7567a1fcefa99133b5bc       -    a/b/c
    100644 blob e965047ad7c57865823c7d992b1d046ea66edf78       6    a/b/c/hello.txt
    100644 blob 51dbfd25a804c30e9d8dc441740452534de8264b      34    welcome.txt



### 2. git diff 魔法
1. 工作区和暂存区的比较.  git diff
2. 暂存区和HEAD的比较. git diff --staged
3. 工作区和HEAD的比较. git diff HEAD

## 5.4 不要使用git commit -a

git ci -a 对本地所以变更的文件执行提交操作.不包括未被版本库跟踪的文件.

丢掉git暂存区带给用户的最大好处: 对提交内容进行控制的能力. ??

## 5.5 搁置问题,暂存状态

保存当前工作进度: git stash

git stash之后, 会看见工作区尚未提交的更改全都不见了.

# 6. Git对象

## 6.1 git对象库探秘
对象Id: SHA1哈希值.

$ git log -1 --pretty=raw

    commit 86035d7a3816e881047b84513ef840c16a1e92e9
    tree f58da9a820e3fd9d84ab2ca2f1b467ac265038f9
    parent b3d0474e6643072d1be575b1ec299fd5ede17be0
    author rongqiao.yurq <rongqiao.yurq@taobao.com> 1386833774 +0800
    committer rongqiao.yurq <rongqiao.yurq@taobao.com> 1386833774 +0800
        which version checked in?

有3个对象ID:
- commmit 86035d7a3816e881047b84513ef840c16a1e92e9: 这是本次提交的唯一标识
- tree f58da9a820e3fd9d84ab2ca2f1b467ac265038f9: 这是本次提交对应的目录树
- parent b3d0474e6643072d1be575b1ec299fd5ede17be0: 这是本次提交的父提交(上一次提交)

研究Git对象ID的一个重量级武器是: git cat-file命令. 可以查看三个Id 的类型:

    $ git cat-file -t 86035
    commit
    $ git cat-file -t f58da
    tree
    $ git cat-file -t b3d0
    commit

引用对象id不需要写全,只要从头开始的几位不要冲突即可.

查看对象内容:

    $ git cat-file -p 8603
    tree f58da9a820e3fd9d84ab2ca2f1b467ac265038f9
    parent b3d0474e6643072d1be575b1ec299fd5ede17be0
    author rongqiao.yurq <rongqiao.yurq@taobao.com> 1386833774 +0800
    committer rongqiao.yurq <rongqiao.yurq@taobao.com> 1386833774 +0800

    which version checked in?
    $ git cat-file -p f58d
    100644 blob fd3c069c1de4f4bc9b15940f490aeb48852f3c42    welcome.txt

blob 对象, 保存了文件welcome.txt的内容.

    $ git cat-file -t fd3c
    blob
    $ git cat-file -p fd3c
    Hello.
    Nice to meet you.

这些对象保存在哪里? 在Git库中的objects目录下, ID的前两位作为目录名, 后38位作为文件名.

    $ ls .git/objects/fd/
    3c069c1de4f4bc9b15940f490aeb48852f3c42
    $ ls .git/objects/86/
    035d7a3816e881047b84513ef840c16a1e92e9

通过提交对象之间的相互关联, 可以很容易识别出一条跟踪链.

    $ git log --pretty=raw --graph 86035
    * commit 86035d7a3816e881047b84513ef840c16a1e92e9
    | tree f58da9a820e3fd9d84ab2ca2f1b467ac265038f9
    | parent b3d0474e6643072d1be575b1ec299fd5ede17be0
    | author rongqiao.yurq <rongqiao.yurq@taobao.com> 1386833774 +0800
    | committer rongqiao.yurq <rongqiao.yurq@taobao.com> 1386833774 +0800
    |
    |     which version checked in?
    |
    * commit b3d0474e6643072d1be575b1ec299fd5ede17be0
    | tree 190d840dd3d8fa319bdec6b8112b0957be7ee769
    | parent 659c6776234a7e12aa897b237351b6693132c906
    | author rongqiao.yurq <rongqiao.yurq@taobao.com> 1386831097 +0800
    | committer rongqiao.yurq <rongqiao.yurq@taobao.com> 1386831097 +0800
    |
    |     who does commit?
    |
    * commit 659c6776234a7e12aa897b237351b6693132c906
      tree 190d840dd3d8fa319bdec6b8112b0957be7ee769
      author rongqiao.yurq <rongqiao.yurq@taobao.com> 1386829417 +0800
      committer rongqiao.yurq <rongqiao.yurq@taobao.com> 1386829417 +0800

          initialized.

git st -b -s, -b选项可以显示出当前工作分支的名称.

    $ git st -s -b
    ## master

执行以下三个命令:

    Qiao@yumatoMacBook-Pro-2:~/practice/playGit/demo$ git log -1 HEAD
    commit 86035d7a3816e881047b84513ef840c16a1e92e9
    Author: rongqiao.yurq <rongqiao.yurq@taobao.com>
    Date:   Thu Dec 12 15:36:14 2013 +0800

        which version checked in?
    Qiao@yumatoMacBook-Pro-2:~/practice/playGit/demo$ git log -1 master
    commit 86035d7a3816e881047b84513ef840c16a1e92e9
    Author: rongqiao.yurq <rongqiao.yurq@taobao.com>
    Date:   Thu Dec 12 15:36:14 2013 +0800

        which version checked in?
    Qiao@yumatoMacBook-Pro-2:~/practice/playGit/demo$ git log -1 refs/heads/master
    commit 86035d7a3816e881047b84513ef840c16a1e92e9
    Author: rongqiao.yurq <rongqiao.yurq@taobao.com>
    Date:   Thu Dec 12 15:36:14 2013 +0800

        which version checked in?

说明HEAD, master, refs/heads/master具有相同的指向.

# 7. Git重置

# 8. Git检出

# 9. 恢复进度

# 10. Git基本操作

# 11. 历史穿梭

# 12. 改变历史

# 13. Git克隆

# 14 Git库管理

# 15. Git协议与工作协同

# 16. 冲突解决

# 17. Git里程碑

# 18. Git分支

# 19. 远程版本库

# 20. 补丁文件交互

# 21. 经典Git协同模型

# 22. Topgit协同模型

# 23. 子模组协同模型

# 24 子树合并

# 25. Android式多版本库协同

# 26. Git和Svn协同模型

# 第五篇 搭建Git服务器

# 27. 使用HTTP协议

# 28. 使用Git协议

# 29. 使用SSH协议

# 30. Gitolite服务架设

# 31. Gitosis服务架设

# 32. Gerrit代码审核服务器

# 33. Git版本库代管

# 34. CVS版本库到Git迁移

# 35. 更多版本控制系统的迁移

# 第七篇 Git的其它应用

# 36. etckeeper

# 37. Gistore

# 38. 补丁中的二进制文件

# 39. 云存储

# 第八篇 Git杂谈

# 40. 跨平台操作Git

# 41. Git的其它特性
