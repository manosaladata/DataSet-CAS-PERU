
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

CAS<-read_rds(Ficheros[95])
# Vemos la estructura de la data para eliminar y Cambiar los nombres
str(CAS)

#Unimos la data de los Ficheros
DATAB<-read_rds("DataMEF_350.rds")
rm(DATAB)

#length(Ficheros)

for (j in 1:95) {
    DATAB<-bind_rows(DATAB,readRDS(Ficheros[j]))
  }

#DATAB<-DATAB[-1,]
#Guardamos las data

saveRDS(DATAB, "DataMEF_445.rds")

# fix(DATAB) para editar CAS N° 711-2019
#----Limpiando la data #----

#V1 Proceso

unique(CasMEF2019$Proceso)
unique(DATAB$V1)

DATAB$V1<-DATAB$V1%>%
          str_remove_all("Proceso")%>%
          str_remove_all("Cas")%>%
          str_remove_all("N")%>%
          str_remove_all("Ef 43 02")%>%
          str_trim()


DATAB$V1<-DATAB$V1%>%
  str_replace("0085 2019","085 2019")%>%
  str_replace("0113 2019","113 2019")%>%
  str_replace_all("  "," ")


          
# 421  2019       
# 0113 2019
# 0085 2019

#V2 N° de posiciones

unique(DATAB$V2)

DATAB$V2<-DATAB$V2%>%
  str_replace_all("2  Posiciones","")%>%
  str_replace_all("1  Posiciones","")%>%
  str_replace_all("3  Posiciones","")%>%
  str_trim()

DATAB$V2<-DATAB$V2%>%
  str_replace_all("Una  01","01  Uno")%>%
  str_replace_all("01  Una","01  Uno")%>%
  str_replace_all("1  Una","01  Uno")%>%
  str_replace_all("Dos  02","02  Dos")%>%
  str_replace_all("Cinco  05","05  Cinco")
  
Numeros<-c("Uno","Dos","Tres","Cuatro","Cinco","Seis","Siete","Ocho","Nueve","Diez",
           "Ciento Treinta Y Ocho")

KeyWord_remove <- str_c(Numeros, collapse = "|")

DATAB$V2<-DATAB$V2%>%
  str_remove_all(KeyWord_remove)%>%
  str_trim()

sum(is.na(DATAB$V2))
colSums(is.na(DATAB))

#V3 Formación académica
unique(DATAB$V3)

Palabras<-c("Académica","Académica  Los","Académico Y O","Formación O","Forma Cion","Formación",
            "Académico De","Grado De","Grado  O","Grado   O","Grado O","Grado","Académico",
            "Ccacemica","Ccacómica","Acadé Ca","Loa","0 Loa","Lo    ","Ls  ","0 Los   ",
            "Le   ","Ol Ca","Acadé A","Los    ","2  Lo","Y O Nivel De Estudio","No   ","Los  ","O   ")
Palabras_remove<-str_c(Palabras,collapse = "|")

DATAB$V3<-DATAB$V3%>%
  str_replace_all(Palabras_remove,"")%>%
  str_trim()

DATAB$V3<-DATAB$V3%>%
  str_replace_all("Los  ","")%>%
  str_replace_all("O   ","")%>%
  str_trim()

#v4
unique(DATAB$V6)
#Guardamos la data limpia

saveRDS(DATAB,"DataLimpiaMEF.rds")


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


