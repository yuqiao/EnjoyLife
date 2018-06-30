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

from datetime import date
from calendary import Calendary

WD2FLAG = {'Monday':'㊀',
'Tuesday':'㊁',
'Wednesday':'㊂',
'Thursday':'㊃',
'Friday':'㊄',
'Saturday':'❻',
'Sunday':'❼' }


def print_month_note(month, year=None):
    today = date.today()
    year = year or today.year
    cal = Calendary(year)
    print '## Goal\n\n'
    print '## Action\n[[%s年%s月]]\n' % (year, month)
    print '## Summary\n\n'
    print '---'
    for wd, d in cal.month(month):
        print '%s. %s' % (d.day, WD2FLAG[wd])
    print ''
    print '#01.bullet/%s#' % year


def main():
    import sys
    print_month_note(int(sys.argv[1]))

if __name__ == "__main__":
    main()

