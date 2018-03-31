## 1. 提测内容

### 1.1 abnormalins_report（实例异常报警服务）

1. 原来直接云鸽接口，更换为新的消息通知服务接口

   ​

2. 消息事件变更：

   - RDS_MS_CHANGE = r'rds_ms_change' -> 'rds_apsaradb_failover'

     短息文本： 尊敬的$!{mcUserName}：您的云数据库$!{ProductType}实例:$!{InstanceName}主库出现异常，高可用系统已触发主备切换，确保实例稳定运行。请检查程序连接是否正常，建议设置自动重连机制以避免切换影响。 

   ​

   - RDS_UNAVAILABLE = r'rds_fault_occurrence' -> 'rds_apsaradb_failurenotification'

     短信文本：
     尊敬的$!{mcUserName}：您的云数据库$!{ProductType}实例:$!{InstanceName}出现故障，阿里云团队正紧急抢修中，恢复时会第一时间通知。 

   ​

   - RDS_RECOVERED = r'rds_fault_termination'  -> 'rds_apsaradb_failurerecovery'

     短信文本：
     尊敬的$!{mcUserName}：您的云数据库$!{ProductType}实例:$!{InstanceName}已于$!{EventTime}恢复正常访问。感谢您对阿里云的支持。 

### 1.2 消息通知服务（adb_notify)

新服务。负责消息通知服务。

见设计文档： http://gitlab.alibaba-inc.com/rds/adb_notify

## 2. 具体提测内容

| 用例                                       | 校验点           | 执行人  | 是否通过 | 是否自动化 |
| ---------------------------------------- | ------------- | ---- | ---- | ----- |
| 发送数据库迁移消息                                | abc消息中心查找到    | 乔云   | 通过   | 否     |
| 发送数据库故障消息                                | abc消息中心查找到    | 乔云   | 通过   | 否     |
| 发送数据库恢复消息                                | abc消息中心查找到    | 乔云   | 通过   | 否     |
| 查找历史消息记录                                 | 接口返回正确的消息历史记录 | 乔云   | 通过   | 否     |
| 用例代码： http://gitlab.alibaba-inc.com/rds/adb_notify/blob/master/example/notify_cli.py |               |      |      |       |

## 3. 代码分支

1. dbaas: http://gitlab.alibaba-inc.com/rds/dbaas/tree/feature_qy_use_notify_srv (待merge)
2. adb_notify: http://gitlab.alibaba-inc.com/rds/adb_notify/tree/master

## 4. 设计文档，kelude记录

1. 设计文档： http://gitlab.alibaba-inc.com/rds/adb_notify/tree/master
2. kelude: https://aone.alibaba-inc.com/project/576940/issue/9850500
3. dml:

`insert ignore into bakowner_types values(114, now(), now(), 348493, 348493, 0, 0, 'apsaradb 消息通知服务', 'D', 0);`

`insert ignore into bakowner values(NULL, now(), now(), '100.69.195.50','','', 114, 348493, 348493, 0, 1, 'global', 5000, 1, '', 0, 'global') ;`

`insert ignore into access_authorization values (NULL, 'NT_DBAAS', 'b8cdd38di522y9ef16d516cc1z3774571cb80a13ee309a29f844ab5fadafeb861kb8ea1eff319a39f844ab5fadafeb86', 114, 'dbcenter access notify serv', 348493, 348493, now(), now());`



## 5. 环境部署

- docker化部署

- 依赖资源：

  - mysql db.
  - vip

- 目前notify service测试环境

  - ip: 100.69.195.50 
  - http服务: http://100.69.195.50:5000/api/v1.0/

  ​

