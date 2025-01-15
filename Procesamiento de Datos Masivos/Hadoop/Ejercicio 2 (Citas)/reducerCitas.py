#!/usr/bin/python3

import sys
from collections import defaultdict

'''
Reducer de Citas
'''

subproblema = None
lista = []

for claveValor in sys.stdin:
        cited, citing_string = claveValor.split("\t",1) #valores separados por comas
        citing_list = [elemento.strip() for elemento in citing_string.split(",")]
        #El primer subproblema es el primer cited
        if subproblema == None:
                subproblema = cited
                
        #si el cited es el subproblema actual, agregamos el citing
        if subproblema == cited:
                lista.extend(citing_list)
        else: #si ya acabo el subproblema, lo emitimos
                print("%s\t%s" % (subproblema, ",".join(map(str,(sorted([int(elemento) for elemento in lista]))))))
                
                #Pasamos al siguiente subproblema
                subproblema = cited
                lista = citing_list 
            
#El anterior bucle no emite el ultimo subproblema
print("%s\t%s" % (subproblema, ",".join(map(str,(sorted([int(elemento) for elemento in lista]))))))
       
