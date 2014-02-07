# mac系统

# 改机器名

网络名修改：
1.修改网络名可以在：“系统偏好设置”-》“共享”-》“电脑名”里修改
2.也可以在终端修改：sudo scutil --set ComputerName your_name
 
主机名修改：
这个主要你打开终端是显示的“xxx@host_name”中的host_name
在终端输入：sudo scutil --set HostName new_host_name


# 屏幕截屏

在Mac OS中，有非常方便的截屏快捷键： 

Command-Shift-3: 对整个屏幕截屏，图片会保存的一个文件中（默认保存的桌面上） 
Command-Ctrl-Shift-3: 对整个屏幕截屏，图片被保存到剪贴板（夹纸板）中。 
Command-Shift-4: 对选定区域进行截屏，并将图片保存到文件中（默认保存的桌面上）。在触发这个快捷键后，按空格(Space)键，可以对一整个窗口或菜单进行截屏。 
Command-Ctrl-Shift-4: 对选定区域进行截屏，图片被保存到剪贴板（夹纸板）中。 
这些快捷键都是系统的默认设置。我们可以对这些快捷键进行修改。进入“系统偏好设置” -> “鼠标与键盘” -> “快捷键”，在“屏幕快照”部分进行修改就可以了。

# Tips

1. spotlight注释功能定位文件
选中一个文件或文件夹, command+I打开简介, 在spotlight注释功能中加入自己特定的关键词.

2. 使用sips命令批量处理图片
- 将当前用户图片文件夹下的所有jpg图片宽带缩小为800px, 高度按比例缩放:
    
    sips -Z 800 ~/Pictures/*.JPG

- 顺时针旋转90

    sips -r 90 ~/Pictures/*.JPG

- 垂直反转

    sips -f vertical ~/Pictures/*.JPG

3. Safari浏览网页, command+I直接打开邮件

使用command+I,可以直接打开邮件并把当前网页附加到待发送的邮件中.

4. shift+command+delete自动清空废纸篓

使用command+delete删除文件, 或者使用shift+command+delete自动清空废纸篓.

5. 当使用系统软件, 使用esc键,系统会帮助你自动完成单词

6. 显示隐藏文件

在终端输入: ls -a. 在Finder中输入shift+command+.可以显示隐藏文件.

7. 利用触发角

8. 维护mac

9. 在Mission Control设置中把"是窗口按应用程序成组"关掉.

10. 截图

- shift + command + 3 全屏幕截屏.
- shift + command + 4 通过鼠标选取截屏.
- 默认格式是png. 可以修改截图文件类型: defaults write com.apple.screencapture type -string JPEG

11. 小工具推荐:

- smcFanContral.
- Go2Shell

12. safari标签:

- command + 链接点击, 在新标签页打开链接
-


