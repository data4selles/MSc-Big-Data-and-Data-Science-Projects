#!/usr/bin/python3

import sys
from collections import defaultdict

'''
Combiner de citas
'''

subproblema = None
lista = []

for claveValor in sys.stdin:
        cited, citing_string = claveValor.split("\t",1) #valores separados por comas
        citing_string = citing_string.strip()
        
        #El primer subproblema es el primer cited
        if subproblema == None:
                subproblema = cited
                
        #si el cited es el subproblema actual, agregamos el citing
        if subproblema == cited:
                lista.append(citing_string)
        else: #si ya acabo el subproblema, lo emitimos
                print("%s\t%s" % (subproblema,  ",".join(lista)))
                
                #Pasamos al siguiente subproblema
                subproblema = cited
                lista = []
                lista.append(citing_string)
            
#El anterior bucle no emite el ultimo subproblema
print("%s\t%s" % (subproblema,  ",".join(lista)), end= "")
