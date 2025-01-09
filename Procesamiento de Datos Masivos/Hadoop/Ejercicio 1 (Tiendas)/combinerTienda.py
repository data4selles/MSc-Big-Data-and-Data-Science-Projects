#!/usr/bin/python3

import sys

'''
Combiner de Tiendas
'''

subproblema = None 
v = 0 
g = 0 

for claveValor in sys.stdin:
        nombretienda, gasto, veces = claveValor.split("\t",2) 

        gasto = float(gasto)
        veces = int(veces)
      
        # subproblema combinación nombre-tienda  
        if subproblema == None:
                subproblema = nombretienda
          
        # sumamos los gastos y las veces
        if subproblema == nombretienda:
                g+=gasto
                v+=veces

        # Acabamos con un subproblema, emitimos nombre-tienda, gastos totales y nº de veces 
        else: 
                print("%s\t%s\t%s" % (subproblema, g,v))
              
                # Siguiente subproblema 
                subproblema = nombretienda
                g=gasto
                v=veces

# Último subproblema
print("%s\t%s\t%s" % (subproblema, g, v))




