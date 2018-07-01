from django.shortcuts import render
from .models import BlogArticle

# Create your views here.

def blog_title(request):
    blogs = BlogArticle.objects.all()
    return render(request, 'blog/titles.html', dict(blogs=blogs))
