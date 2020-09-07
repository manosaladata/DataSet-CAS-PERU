
#----DATA pdf CAS MEF 2019 #----
rm(list = ls())

setwd("c:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data")


library(tidyverse)
library(readr)
library(dplyr)

list.files() # vemos todos los archivos de nuestra carpeta
#list.dirs() # ver los diectorios

#Listamos los ficheros para leerlos y modificar
Ficheros <- list.files(pattern = "\\.rds")

#87 y 123
# Ejemplo para leer y modificar individualmente

CAS<-read_rds(Ficheros[30])
# Vemos la estructura de la data para eliminar y Cambiar los nombres
str(CAS)
#Unimos la data de los Ficheros

DATAB<-read_rds("DataMEF_270.rds")
rm(DATAB)

#length(Ficheros)

for (j in 1:30) {
    DATAB<-bind_rows(DATAB,readRDS(Ficheros[j]))
  }

#DATAB<-DATAB[-1,]
#Guardamos las data

saveRDS(DATAB, "DataMEF_300.rds")

unique(DATAB$V2)
table(unique(DATAB$V1))
table(DATAB$V8)


#Limpieza

DeleteName<-c("V6","V12","V15")
CAS<- CAS[ , !(names(CAS) %in% DeleteName)]

NewName<-c("Proceso","Posiciones","Grado_Académico","Conocimiento1","Conocimiento2",
           "Conocimiento3","Experiencia_General","Experiencia_esp1","Experiencia_esp2",
           "Habilidades","Función1","Función2","Función3","Función4","Función5","Función6",
           "Función7","Función8","Función9","Duración_Contrato","Contraprestación")

names(CAS)[1:21]<-NewName

NewName<-c("Proceso","Posiciones","Grado_Académico","Grado_Académico2","Conocimiento1","Conocimiento2",
           "Conocimiento3","Experiencia_General","Experiencia_esp1","Experiencia_esp2",
           "Habilidades","Función1","Función2","Función3","Función4","Función5","Función6",
           "Departamento","Duración_Contrato","Contraprestación")

names(CAS)[1:20]<-NewName

# Unimos las bases de datos por fila
#DataSUMA<-bind_rows(DataSUMA,CAS)

#Una vez analizada que casi todos cumplem ciertos requsitos podemos automatizar para juntar la data

#Creamos un dataframe con NAs para compilar

DATAB <- matrix(ncol = 35,byrow = F)
DATAB<-as.data.frame(DATAB)


