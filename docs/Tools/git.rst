============================
Git 学习
============================

:author: yuqiao20@gmail.com
:version: 0.0.1
:Date: 12-01-15 09:25:50 

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









