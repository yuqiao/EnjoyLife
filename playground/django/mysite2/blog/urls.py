#!/usr/bin/env python
# coding=utf-8
#
# Brief:
# Author: rongqiao.yurq (rongqiao.yurq@taobao.com)
#
# Updates:
# 0.1.0 --------------------
#   Create it.
#

from django.conf.urls import url
from . import views

urlpatterns = [
    url(r'^$', views.blog_title, name='blog_title'),
]

