#!/usr/bin/python3

import sys 
from pyspark.sql import SparkSession
import glob

'''
Programa que cuenta las visitas de cada categoría en youtube
y devuelve finalmente la que tiene menos visitas
'''

def CategoriaVisitas(linea):
    palabras = linea.split("\t")
    if len(palabras) > 5:  #Al menos 6 columnas
        return [(palabras[3], float(palabras[5]))] 
    else:
        return [] 

def suma(valor1, valor2):
	return int(valor1)+float(valor2)

def obtenerMinimo(rdd):
    return rdd.min(key=lambda x: x[1])

# Inicialización
spark = SparkSession.builder.appName('CategoriaDeVideosMenosVista').getOrCreate()
entrada_carpeta = sys.argv[1]  #Ruta carpeta
salida = sys.argv[2]  

#Tomamos todos archivos de la carpeta excepto el log.txt
entrada = entrada_carpeta + "/*.txt"  
datosEntrada = spark.sparkContext.textFile(entrada).filter(lambda path: "log.txt" not in path) 

#Aplicamos funciones
pares = datosEntrada.flatMap(CategoriaVisitas) 
suma_por_categoria = pares.reduceByKey(suma)  
categoria_min_visitas = obtenerMinimo(suma_por_categoria)  
resultado_formateado = f"{categoria_min_visitas[0]};{categoria_min_visitas[1]}" #Formateamos salida

spark.sparkContext.parallelize([resultado_formateado]).saveAsTextFile(salida) #Guardamos






