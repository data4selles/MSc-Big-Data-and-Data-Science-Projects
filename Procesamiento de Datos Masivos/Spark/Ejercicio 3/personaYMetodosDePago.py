#!/usr/bin/python3

import sys
from pyspark.sql import SparkSession

'''
Programa que cuenta el nº de veces
que ha gastado una persona > o < 1500 euros
sin tarjeta
'''

def procesarLinea(linea):
    palabras = linea.split(";")
    nombre = palabras[0]
    tipo_pago = palabras[1]
    monto = float(palabras[2])
    mayor = 1 if tipo_pago != "Tarjeta de crédito" and monto > 1500 else 0
    menor = 1 if tipo_pago != "Tarjeta de crédito" and monto <= 1500 else 0
    return (nombre, (mayor, menor))

# Inicialización
spark = SparkSession.builder.appName('personaGastosConTarjetaCredito').getOrCreate()
entrada = sys.argv[1]  
salida1 = sys.argv[2]  #Ruta salida Mayores
salida2 = sys.argv[3]  #Ruta Salida Menores

datosEntrada = spark.sparkContext.textFile(entrada)

#Procesamos las líneas
paresProcesados = datosEntrada.map(procesarLinea)

#Obtenemos para cada persona el total de mayores de 1500 y de menores
conteos = paresProcesados.reduceByKey(lambda a, b: (a[0] + b[0], a[1] + b[1]))

#Separamos en mayores y menores
conteoMayor = conteos.map(lambda x: (x[0], x[1][0]))
conteoMenor = conteos.map(lambda x: (x[0], x[1][1]))

#Formateamos y guardamos la salida
salidaMayor = conteoMayor.map(lambda x: f"{x[0]},{x[1]}")
salidaMenor = conteoMenor.map(lambda x: f"{x[0]},{x[1]}")
salidaMayor.saveAsTextFile(salida1)
salidaMenor.saveAsTextFile(salida2)


