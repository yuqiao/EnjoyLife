reStructuredText 学习
============================

:author: yuqiao20@gmail.com
:version: 0.0.1
:Date: 12-01-19 12:46:42

.. contents::


Structure
---------
It's more like "Relaxed Text" that uses certain consistent patterns. 
These patterns are interpreted by a HTML converter to produce "Very Structured Text" 
that can be used by a web browser.

The most basic pattern recognised is a **paragraph**.
That's a chunk of text that is seperated by blank lines (one is enough).
Paragraphs must have the same indentation. For example::

  This is a paragraph.  It's quite
  short.

     This paragraph will result in an indented block of
     text, typically used for quoting other text.

  This is another one.

Results in:

  This is a paragraph.  It's quite
  short.

     This paragraph will result in an indented block of
     text, typically used for quoting other text.

  This is another one.



Text Style
----------
Inside paragraphs and other bodies of text, you may additionally mark
text for *italics* with "``*italics*``" or **bold** with
"``**bold**``".  This is called "inline markup".

List
----





