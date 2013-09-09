# git学习笔记

## 0. 说明
也是刚使用git, 一边学习,一边写这个文档. 可能有些错误, 有问题请帮忙及时指正.

学习感受:

    git有几十个命令, 比svn复杂多了, 也强大多了. 我们可以先使用git命令的子集,尽量少使用git reset等危险命令.

## 1. 简单入门

可参考: http://rogerdudler.github.io/git-guide/index.zh.html

### 1.1 git设置

1. 配置用户名和电子邮件地址:

    git config --global user.name '乔云'
    git config --global user.email rongqiao.yurq@taobao.com

检查配置:

     git config --global --list

2. 设置Git别名, 对本用户的全局配置中添加别名:

        git config --global alias.st status
        git config --global alias.ci commit
        git config --global alias.co checkout
        git config --global alias.br branch
       
3. 在Git命令输出中开启颜色显示.

        git config --global color.ui true

### 1.2 git操作

0. 获取帮助:

    git help <command>

1. 初始化版本库:

        mkdir demo
        cd demo
        git init
        echo "Hello" > welcome.txt
        git add welcome.txt
        git ci -m 'initialized'

2. 基本操作:
   
        git clone -b <branch> <版本库地址>   # 克隆版本库到本地
        git pull origin <branch>             # 将本地代码与服务器同步
        git status                           # 参考当前的文件修改
        git add <filename>                   # 将文件加入到版本控制
        git commit -m "commit comments"      # 提交代码到本地版本库
        git push origin <branch>             # 将本地代码推送到远程版本库

3. 分支操作:

        git branch <branchname>              # 创建分支
        git branch -d <branchname>           # 删除分支
        git checkout <branch>                # 切换分支
        git merge --no-ff <branch>           # 合并分支到当前分支

4. 标签操作:
  
        git tag <tagname> -m "tag comments"  # 创建标签
        git tag -a <tagname> <提交号>        # 在某个提交号上创建标签
        git show <tagname>                   # 查看标签内容
        git -d <tagname>                     # 删除标签.  危险!!!
        git push origin <tagname>            # 将本地标签推送的远程版本库.
        git push origin --tags               # 推送所有标签

5. 查看日志:

        git log --graph                      # 查看日志
        git log --graph --oneline           
        git log -p                           # 显示版本历史，以及版本间的内容差异
        git log -1                           # 只显示最近一次提交
        git log -p -20                       # 显示最近的20个提交：以及版本间的内容差异
        git log --since="6 hours"            # 显示最近6小时的提交

6. 比较操作:
    
        git diff                             # 显示当前工作目录树和暂存区间的差别
        git diff --cached                    # 显示暂存区和版本库间的差别
        git diff HEAD                        # 显示工作目录树和版本库之间的差别
        git diff <start point>               # 显示工作目录树与版本库中某次提交版本之间的差别
        git diff <start point> <end point>   # 显示版本库中两个版本之间的差别

7. 其它:

        git remote -v                        # 显示对应项目的远程克隆地址


### 1.3 reset操作

reset 操作比较危险, 还在学习中.


# 2 最佳实践

## 2.1 分支模型

请参考: http://www.juvenxu.com/2010/11/28/a-successful-git-branching-model

## 2.2 使用git协作开发的流程
以wxcppsrv库为例 (以下步骤未完全验证, 会持续更改)

1. checkout出开发分支develop:

        git clone -b develop git@gitlab.alibaba-inc.com:rongqiao.yurq/wxcppsrv.git
        cd wxcppsrv

2. 若是开发新功能,请创建自己的feature分支:

        git checkout -b myfeature

3. 在myfeature分支下,编辑开发, 并添加提交:

        vi ...
        git add <path>  # 添加需要版本控制的文件
        git status      # 提交之前,需要检查一下提交的内容
        git commit -m "add commit comments" # 提交修改到本地库

4. 第3步骤, 在本地做了版本控制, 但未放到gitlab上. 若是长期开发, 建议开发提交的gitlab上, 让gitlab帮你保存.

        git push origin myfeature

5. 开发并自测完毕, 提交测试, 让qa帮忙测试新功能, 若出现bug, 可以重复以上第3和4步骤.

6. 基本测试完毕后, 合并到develop分支:

        git checkout develop   # 切换到develop分支
        git pull               # 与gitlab中心库同步.
        git merge --no-ff myfeature -m "..."  # 合并分支myfeature

7. 有可能会出现冲突, 需要处理完冲突.

        修改冲突的文件
        git add -u               # 标志冲突已经已经解决.
        git status      # 提交之前,需要检查一下提交的内容
        git commit -m "add commit comments" # 提交修改到本地库
        git push origin develop  # 将更改同步到gitlab中心库.

8. checkout出release分支, 建议release分支命名以release开始.

        git branch -b release-myfeature
        git push origin release-myfeature

9. 让测试人员帮忙做完整测试, 若测试有bug, 在release分支下做3,4步骤.

10. 测试通过后, 将release 分支合并到develop分支, 有可能需要处理冲突.

        git checkout develop
        git pull
        git merge --no-ff release-myfeature -m "..."
        git push origin develop  # 将更改同步到gitlab中心库.

11. 将release 分支合并到master分支(这里不应该有冲突).
    
        git checkout master
        git pull
        git merge --no-ff release-myfeature -m "..."
        git tag <tagname> -m "tag comments"       # 打标签
        git push origin master  # 将更改同步到gitlab中心库.


## 2.3 回滚更改