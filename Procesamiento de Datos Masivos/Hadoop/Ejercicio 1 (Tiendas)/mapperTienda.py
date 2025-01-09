#!/usr/bin/python3

import sys

'''
Mapper de Tiendas
'''

for linea in sys.stdin:
        nombre, tienda, gasto = linea.strip().split(";",2)
        print("%s;%s\t%s\t%s" % (nombre, tienda, gasto, 1))

