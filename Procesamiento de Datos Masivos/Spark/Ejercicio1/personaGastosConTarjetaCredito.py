#!/usr/bin/python3

import sys 
from pyspark.sql import SparkSession

'''
Programa que cuenta el dinero 
que ha gastado una persona con tarjeta
'''

def solotarjeta(linea):
    palabras = linea.split(";")
    if palabras[1] == "Tarjeta de crédito":
        return [(palabras[0], float(palabras[2]))]
    else:
        return [(palabras[0], 0.0)]

def suma(valor1, valor2):
	return float(valor1)+float(valor2)

#Inicialización
spark = SparkSession.builder.appName('personaGastosConTarjetaCredito').getOrCreate()
entrada = sys.argv[1] 
salida = sys.argv[2]

datosEntrada = spark.sparkContext.textFile(entrada)

#Aplicamos las funciones y formateamos el resultado para que sea "Nombre;Dinero"
sumadinero = datosEntrada.flatMap(solotarjeta).reduceByKey(suma)
resultado_formateado = sumadinero.map(lambda x: f"{x[0]};{x[1]}")

resultado_formateado.saveAsTextFile(salida)

