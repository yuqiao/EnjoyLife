#!/usr/bin/env python
# coding=utf-8
"""
Created on 

@author: rongqiao.yurq
"""
from django.conf.urls import url
from . import views

urlpatterns = [
    url(r'^$', views.blog_title, name='blog_title'),
    url(r'(?P<article_id>\d)/$', views.blog_article, name='blog_detail'),
]
