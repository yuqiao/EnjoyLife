# Linux Tips

## ssh问题

### 为何ssh不通?
一般.ssh文件夹要755
authorxx_keys要644
id_xsa(私钥）要600
id_xsa.pub(公钥）要644
还有个坑就是如果是用vim粘贴到authxx_keys里的密码串要主要职能是一样，如果有断行 也没用的
