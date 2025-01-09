#!/usr/bin/python3

import sys

'''
Reducer de Tiendas
'''

subproblema = None
g = 0 
v = 0 

for claveValor in sys.stdin:
        nombretienda, gasto, veces = claveValor.split("\t",2) 

        gasto = float(gasto)
        veces = int(veces)
      
        # Subproblema combinación de nombre-tienda 
        if subproblema == None:
                subproblema = nombretienda
          
        # Sumamos los gastos y las veces
        if subproblema == nombretienda:
                g+=gasto
                v+=veces
        
        # Acabamos con un subproblema , emitimos el nombre-tienda, y la media
        else: 
                print("%s;%s" % (subproblema, g/v))
              
                # Pasamos al siguiente subproblema 
                subproblema = nombretienda
                g=gasto
                v=veces

# El anterior bucle no emite el ultimo subproblema
print("%s;%s" % (subproblema, g/v))




