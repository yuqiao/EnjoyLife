set names utf8;
use mybatis;

create table `country` (
    `id` int not null auto_increment,
    `countryname` varchar(255) NULL,
    `countrycode` varchar(255) NULL,
    PRIMARY KEY (`id`)
);

insert into country (`countryname`, `countrycode`) values('中国', 'CN'), ('美国', 'US'), ('俄罗斯', 'RU'), ('英国', 'GB'), ('法国', 'GB');
