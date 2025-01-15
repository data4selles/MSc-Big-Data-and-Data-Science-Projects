#!/usr/bin/python3

import sys

'''
Mapper de citas
'''
next(sys.stdin)

#Por cada par, lo invierte
for linea in sys.stdin:
        citing, cited = linea.strip().split(",",1)
        print("%s\t%s" % (cited, citing))
