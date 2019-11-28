# -*- coding: utf-8 -*-
"""
Created on Mon Oct 14 14:29:00 2019

@author: Jose Fernando Montoya C (jomontoyac@unal.edu.co)
"""

import pandas as pd
import numpy as np
import os
import glob
import re
from pyproj import Proj, transform
import unidecode
from datetime import datetime

meses = {'Enero': 1
         ,'Febrero': 2
         ,'Marzo': 3
         ,'Abril': 4
         ,'Mayo': 5
         ,'Junio': 6
         ,'Julio': 7
         ,'Agosto': 8
         ,'Septiembre': 9
         ,'Octubre': 10
         ,'Noviembre': 11
         ,'Diciembre': 12}


## Parametros de entrada y salida para convertir de conrdenadas planas X,Y a cordenadas geodesicas WGS84
inProj = Proj("+proj=tmerc +lat_0=4.596200416666666 +lon_0=-74.07750791666666 +k=1 +x_0=1000000 +y_0=1000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
#outProj = Proj("+init=EPSG:4326")
outProj = Proj("+proj=longlat +ellps=WGS84 +datum=WGS84 +units=degrees")
files = glob.glob("*.csv")

## Patron para transformar la columna hora
patron = r'(?P<hora>[0-9]{1,2}):(?P<min>[0-9]{2})\D*(?P<seg>[0-9]*)\s*(?P<temp>[a,p])\.*\s*m\.*'

## Compilacion del patron ignorando los caracteres en mayuscula o minuscula
p = re.compile(patron, re.IGNORECASE)

data = pd.DataFrame()

## Lectura de cada dataframe
for file in files:
    df = pd.read_csv(file, sep=",", header = 0, encoding = 'utf-8')
    #Transformacion de la hora a esquema estandar de HH:MM:SS y en horario militar
    df['HourMin'] = df['HORA'].apply(lambda x: '00'+ ':'+re.search(p, x).group('min') + ':00'
                                            if int(re.search(p, x).group('hora'))==12 and re.search(p, x).group('temp').lower()=='a'
                                            else (
                                            "{:02d}".format(int(re.search(p, x).group('hora'))) 
                                            + ':'+re.search(p, x).group('min') + ':00'
                                            if re.search(p, x).group('temp').lower()=='a'
                                            else (
                                            re.search(p, x).group('hora')+':'+re.search(p, x).group('min') + ':00'
                                            if int(re.search(p, x).group('hora'))==12 and re.search(p, x).group('temp').lower()=='p'
                                            else
                                            str(int(re.search(p, x).group('hora'))+12)
                                            +':'+ re.search(p, x).group('min')  + ':00'
                                            )
                                            )
                                        )
    df['Hour'] = df['HourMin'].apply(lambda x: re.findall('([0-9]{2}):[0-9]{2}:[0-9]{2}', x)[0] + ':00:00')
    ## Transformacion de la fecha en formato YYYY-MM-DD
    df['Date'] = df['FECHA'].apply(lambda x: re.findall('[0-9]{4}-[0-9]{2}-[0-9]{2}', x)[0])
    ## Columna fecha en formato YYY-MM-DD HH:MM:SS y modificacion de tipo de dato
    df['DateHour'] = df['Date'] + ' ' + df['Hour']
    df['DateHourMin'] = df['Date'] + ' ' + df['HourMin']
    df['DateHour'] = df['DateHour'].astype('datetime64[ns]')
    df['DateHourMin'] = df['DateHourMin'].astype('datetime64[ns]')
    ## Tranformacion de coordenadas planas a geodesicas
    df['Location'] = df[['X','Y']].apply(lambda rows: transform(inProj,outProj,rows.X,rows.Y), axis=1)
    ## Extraccion de la longitud y latitud ya transformada de la localizacion
    df['Longitud'] = df['Location'].apply(lambda x: x[0])
    df['Longitud'] = df['Longitud'].astype('float32')
    df['Latitud'] = df['Location'].apply(lambda x: x[1])
    df['Latitud'] = df['Latitud'].astype('float32')
    ## Eliminacion de columnas que no son de utilidad para el analisis
    df.drop(columns = ['Location','Hour','HourMin','X','Y','DIA','MES','FECHA','RADICADO','DIRECCION_ENC','CBML','MES_NOMBRE','HORA'], inplace = True)
    ## Agregacion de cada archivo anual
    data = data.append(df)

 
## Carga de informacion de barrios y comunas consultados de la pagina oficial de la alcaldia de medellin
## (https://www.medellin.gov.co/MAPGISV5_WEB/mapa.jsp?aplicacion=0) por medio de las direcciones de los datos insumo
## para tener la mejor informacion acerca de los nombres de los barrios y las comunas dado que la informacion cargada
## en muchos casos, el nombre de los barrios o comunas estan diferentes en 1 o 2 caracteres asi pertenezcan al mismo
## barrio
barrios = pd.read_excel('InformacionBarriosFaltantes_v2.xlsx',sheet_name = 'Geocodificacion', header = 0)
barrios = barrios[['DIRECCION','comuna','barrio']].drop_duplicates()
barrios.dropna(inplace = True)

## Modificacion de nombres de barrios y comunas
dataT = pd.merge(data, barrios, how = 'left', left_on='DIRECCION', right_on= 'DIRECCION')
dataT['COMU'] = dataT['comuna'].combine_first(dataT['COMUNA'])
dataT['BARR'] = dataT['barrio'].combine_first(dataT['BARRIO'])

## Modificacion de barrios cuyos nombres son numericos. La codificacion de los mismos se obtuvo a traves
## del siguiente documento https://www.medellin.gov.co/irj/go/km/docs/wpccontent/Sites/Subportal%20del%20Ciudadano/Planeaci%C3%B3n%20Municipal/Secciones/Indicadores%20y%20Estad%C3%ADsticas/Documentos/Estad%C3%ADsticas%20Sisb%C3%A9n/Estadisticas%20Sisb%C3%A9n%202006/04_NucleosFamNivelEstratoBarrio.pdf
codbarrios = pd.read_excel('BarriosConCodigos.xlsx',sheet_name = 'codigos', header = 0)
codbarrios['CodBarrio'] = codbarrios['CodBarrio'].astype('str')
dd = pd.merge(dataT, codbarrios, how = 'left', left_on='BARR', right_on= 'CodBarrio')
dd['BARR1'] = dd['Nombre'].combine_first(dd['BARR'])

dd = dd[['OBJECTID','Date','DateHour', 'DateHourMin','PERIODO','DIA_NOMBRE','DIRECCION','CLASE',
               'GRAVEDAD','DISENO','COMU', 'BARR1', 'Longitud', 'Latitud']]

## Eliminacion de datos nulos o faltantes
dd.dropna(inplace=True)

## Eliminacion de datos donde la comuna fuese 0 o de nombre AU dado que no corresponde a un barrio o comuna de 
## medellin
dd = dd[(dd['COMU']!='0')&(dd['COMU']!='AU')&(dd['BARR1']!='0')]

## Modificacion de los nombres de los barrios, comunas, clases, disenios, nombre de dias, gravedad. Se eliminan
## las tildes y se pone la primera letra en mayuscula y las demas en minuscula.
## Lo anterior se realiza debido a que algunos datos de las columnas asi hicieran referencia al mismo dato, diferian
## porque contenian acentuaciones

dd['BARR11'] = dd['BARR1'].apply(lambda x: ' '.join([unidecode.unidecode(i) for i in str(x).split(' ')]).capitalize())
dd['COMU1'] = dd['COMU'].apply(lambda x: ' '.join([unidecode.unidecode(i) for i in str(x).split(' ')]).capitalize())
dd['DAYNAME'] = dd['DIA_NOMBRE'].apply(lambda x: ' '.join([unidecode.unidecode(i) for i in str(x).split(' ')]).capitalize())
dd['CLASS'] = dd['CLASE'].apply(lambda x: ' '.join([unidecode.unidecode(i) for i in str(x).split(' ')]).capitalize())
dd['DAMAGE'] = dd['GRAVEDAD'].apply(lambda x: ' '.join([unidecode.unidecode(i) for i in str(x).split(' ')]).capitalize())
dd['DESIGN'] = dd['DISENO'].apply(lambda x: ' '.join([unidecode.unidecode(i) for i in str(x).split(' ')]).capitalize())

## Modificacion de nombres de barrios que contenian la palabra No. digito dado que no tenian un formato estandar, lo que
## ocasionaba que asi fuese el mismo barrio, se seleccionara como diferente porque tenia un caracter de mas o menos
dd['BARR12'] = dd['BARR11'].apply(lambda x: str(re.findall('([\w*\s*]*)[Nn][Oo]\s*\.\s*[0-9]{1,2}', x)[0])+ 'No. ' + str(re.findall('[Nn][Oo]\s*\.\s*([0-9]{1,2})', x)[0]) if re.search('[Nn][Oo]\s*\.\s*([0-9]{1,2})', x) else x)

## Se modifica la palabra caida ocupante por caida de ocupante dado que hace referencia a la misma categoria
dd['CLASE1'] = dd['CLASS'].apply(lambda x: 'Caida de ocupante' if re.search('Caida\s*\w*\s*ocupante', x) else x)

## Se selecciona las columnas de los datos de interes
dd = dd[['OBJECTID','Date','DateHour', 'DateHourMin','PERIODO','DAYNAME','DIRECCION','CLASE1',
               'DAMAGE','DESIGN','COMU1', 'BARR12', 'Longitud', 'Latitud']]

## Se modifican los nombres de las columnas
dd.columns = ['OBJECTID','Date','DateHour', 'DateHourMin','PERIODO','DIA_NOMBRE','DIRECCION','CLASE',
               'GRAVEDAD','DISENO','COMUNA', 'BARRIO', 'Longitud', 'Latitud']
## Se agregan las columnas mes y hora para agrupar por nombre_mes o por hora (periodo)
dd['MES'] = dd['DateHour'].apply(lambda x: ''.join([k for k, v in meses.items() if int(v) == int(x.strftime("%m"))]))
dd['HORA'] = dd['DateHour'].apply(lambda x: int(re.findall('[0-9]{4}-[0-9]{2}-[0-9]{2} ([0-9]{2}):[0-9]{2}:[0-9]{2}', str(x))[0]))


## Se exportan los datos en un archivo csv denominado AccidentalidadDepurada y codificado en utf-8

dd.to_csv('AccidentalidadDepurada.csv', sep = ",", index = False, header=True, encoding= 'utf-8')

