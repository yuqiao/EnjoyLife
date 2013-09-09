============================
Git 学习
============================

:author: yuqiao20@gmail.com
:version: 0.0.1
:Date: 12-01-15 09:25:50 

.. contents::

1 日常使用
==============
1.1 每日工作备份
------------------
1. 每做完一个小节或者一个步骤：

   - git add -u  （ 若添加新文件用： git add -i )
   - git commit

2. 推送到服务器

   - git push

2 GitHub
============
2.1 创建repo
---------------
- Click https://github.com/repositories/new
- mkdir ~/life
- cd ~/life
- git init
- toutch README
- git add README
- git commit -m 'first commit'
- git remote add origin git@github.com:yuqiao/life.git
- git push origin master

4 Git初始化
============
4.1 创建版本库及第一次提交
----------------------------

1. 配置用户名和电子邮件地址::

    git config --global user.name 'yuqiao20'
    git config --global user.email yuqiao20@gmail.com

2. 设置Git别名.

   - 令别名为所有用户使用，可用以下命令::

        sudo git config --system alias.st status
        sudo git config --system alias.ci commit
        sudo git config --system alias.co checkout
        sudo git config --system alias.br branch

   - 只在本用户的全局配置中添加别名::

        git config --global alias.st status
        git config --global alias.ci commit
        git config --global alias.co checkout
        git config --global alias.br branch
        
3. 在Git命令输出中开启颜色显示.

    git config --global color.ui true

4. 初始化版本库：

   - mkdir demo
   - cd demo
   - git init
   - echo "Hello" > welcome.txt
   - git add welcome.txt
   - git ci -m 'initialized'

4.2 为何工作区根目录下有个.git目录
------------------------------------
分布式版本控制系统的一个共同特点： 版本库位于工作区的根目录下。
而传统的集中式版本控制系统的版本库和工作区是分开的。

Git这种将版本库房子工作区根目录下，使得所有版本控制操作都在本地即可完成。

可以在子目录做git操作。

显示版本库.git目录所在目录::
    
    git rev-parse --git-dir

相对于工作区根目录的相对路径::

    git rev-parse --show-prefix

4.3 git config命令的各参数区别
------------------------------------
Git的三个配置文件分别是：版本库级别的配置文件，全局配置文件(用户主目录下）和系统级配置文件（/etc目录下），查看编辑::

- git config -e
- git config -e --global
- git config -e --system

4.4 总结
------------
版本库创建三部曲：
- git init
- git add
- git commit


5 Git暂存区(stage)
===================

5.2 理解暂存(stage）
-----------------------



命令总结
-----------------
1. 查看git提交日志，（--stat 可以查看每次提交的文件变更统计）::

    git log --stat 

2. 查看状态

   - git status
   - git status -s (精简)

3. 查看变化

   - git diff, 显示工作区和stage相比的区别。
   - git diff HEAD, 工作区和HEAD相比。
   - git diff --cached ( git diff --staged ), stage和版本库的比较。













