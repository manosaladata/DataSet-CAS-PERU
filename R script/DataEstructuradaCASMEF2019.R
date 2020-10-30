
#----DATA pdf CAS MEF 2019 #----
rm(list = ls())

setwd("c:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data")


library(tidyverse)
library(NLP)
library(tm)

CasMEF2019<-read_rds("./MEF/DataLimpiaMEF.rds")

# fix(DATAB) para editar CAS N° 711-2019
#----Limpiando la data #----
# Remover
Remover<-c("^En |En$","^A |A$","^0 |0$|^O |O$","^De |De$","^Oo|Oo$","^Y|Y$","^Del |Del$",
           "^La |La$","^Conocimientos   ","^Programa De","^Nivel De Estudio","^Conocimientos O",
           "60 Horas$","Con El$","^Que |Que$","^Ls ","^Académico Y O","^Formación Académica  Grado",
           "^Formación   Grado De","^Grado","^Académica  Grado","^Ccacemica","^Acadé Ca","^Ccacómica","^Loa |Loa$","3 O 4 Años",
           "Ciencias$","Ciencia$","3 Ó 4 Años","^Básica En","^Básica  1 Ó 2 Años  En",
           "Ingenieria$","O Ingeniera$", "Superior","^30 4 Años  En","^Asias",
           "Uni Itari E A","^Se","^Cuenta Con El","Po$","Recursos$","80 Horas Acumuladas$",
           "O Base$","^Experiencia   ","60 Horas Acumuladas$","El Cual Deberá Tener Una Duración No$",
           "Nivel$","Con Énfasis$","^Académico V O Nivel De Estudio Política",
           "O O Y  2 Ae E$","El Cual Deberá Tener Una Duración No Menor De Doce  12$","^C ",
           "El Cual Deberá$","Necesario Sólo En Caso De No Acreditar El Título$","An$",
           "^E 0","O Afines","Y O Uso$","El Cual$","Ingeniería$","50 Horas$","O  O Po  O 7$",
           "40 Horas$","40 Horas Acumuladas","20 Horas$","Po   E$","^C Oo","Afines$",
           "El Cual Deberá$","Duración No Menor De Doce  12  Horas Lectivas","60$","O E$",
           "^No ","E   O  L Da$","La  O  L$","Mm$","El Cual Deberá Tener Una$","En El Sector$",
           "En El$","Al$","^Los |Los$","2$","Como$","Horas Acumuladas$","Me On O","Ee$","Y Obras$",
           "Una Duración No Menor De Doce  12  Horas Lectivas","60 Hora$","L Er$","O La Poy$",
           "O Ys Lar O$","Es$","^Lo |Lo$","O E Rs Ls$","Deberá Tener Una Duración No Menor De Doce  12  Horas",
           "Entre Otros$","U$","Q   Y Gue  Progral     5$","Ao L E$","O O E Z$",
           "80 Horas$","^Oa","Co Em$","^Experiencia General Y Específica ","A An$","Oy$","A E$",
           "^Os Een","^Experiencia General V Específica   ","O   E$","Con$","^P ","Alib$",
           "^Laboral ","En E$","L Pr$","^1 ","^2 ","^3 ","^4 ","^5 ","^6 ","^7 ","^8 ",
           "^9 ","^10  ","^11 ","^12 ","^13 ","^14 ","^15 ","Oe$","^Experiencia Laboral   ","Pp$",
           "Py$","^E A |E A$","^Os Len","^Len ","Le Pr Oa$","^Os 7","^Pp Y Esp","Y El$","Por El Ente$",
           "^Las |Las$","^Q ","Al$","Sus$","^Para |Para$","Asuntos$","Otras$","^S ","^Su |Su$","Por$","Con Una$",
           "Desde Una$","Del Cncf$","Que Le Sea$","Sub$","De La Ult$","Así$","Quien$","Desde$","Entre$","Sin$") #Inicio y Final 
Remover<-str_c(Remover,collapse = "|") #   Oo


#---- V1 #----
#Proceso

unique(CasMEF2019$V1)

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

names(CasMEF2019)[1]<-"Proceso"
          
# 421  2019       
# 0113 2019
# 0085 2019

#---- V2 #----
#N° de posiciones

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

names(CasMEF2019)[2]<-"Posiciones"

#---- V3 #----
#Formación académica
unique(CasMEF2019$V3)

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


sapply(CasMEF2019,class)

# Generamos dos va variables a aprtir de V3 (Duplicacmos)
CasMEF2019<-CasMEF2019%>%
  mutate(v3Grado=V3)

unique(CasMEF2019$v3Grado)
Concerva<-c("Título Profesional Universitario","Título Universitario Profesional","Título Universitario",
            "Titulo Universitario","Título Profesional Técnica","Título Profesional",
            "Título De Técnico Superior","Título Profesional Técnica","Bachiller O Título Profesional Universitario",
            "Bachiller O Título Profesional","Bachiller O Título Universitario Profesional",
            "Bachiller Universitario","Bachiller","Egresado Universitario","Egresado De Técnica Superior","Egresado Técnico Superior",
            "Egresado A  De Carrera Técnica Superior ","Egresado De Técnica","Egresado","Secundaria Completa")
Concerva<-str_c(Concerva,collapse = "|")

CasMEF2019$v3Grado<-CasMEF2019$v3Grado%>%
  str_extract(pattern =Concerva)%>% #Extae las cadenas especificadas
  str_trim()


sum(is.na(CasMEF2019$v3Grado))
unique(CasMEF2019$v3Grado)
#Uniformizar la Data de v3Grado

CasMEF2019$v3Grado<-CasMEF2019$v3Grado%>%
  str_replace("Título Profesional Técnica|Título De Técnico Superior","Título De Técnico")

CasMEF2019$v3Grado<-CasMEF2019$v3Grado%>%
  str_replace_all("Título Profesional Universitario|Título Universitario Profesional|Título Profesional|Titulo Universitario",
                  "Título Universitario")

CasMEF2019$v3Grado<-CasMEF2019$v3Grado%>%
  str_replace("Egresado De Técnica Superior|Egresado De Técnica|Egresado Técnico Superior|Egresado A  De Carrera Técnica Superior",
                  "Egres Técnico")

CasMEF2019$v3Grado<-CasMEF2019$v3Grado%>%
  str_replace("Egresado Universitario","Egresado")

CasMEF2019$v3Grado<-CasMEF2019$v3Grado%>%
  str_replace("Egresado","Egresado Universitario")%>%
  str_replace("Egres Técnico","Egresado Técnico")%>%
  str_replace("Bachiller Universitario","Bachiller")

unique(CasMEF2019$v3Grado)

DataFiltro<-filter(CasMEF2019,v3Grado=="Egresado")

#Creamos otra variable V3Profesión a partir de V3

CasMEF2019$v3Profesión<-CasMEF2019$V3%>%
  str_remove(Concerva)%>%
  str_trim()

unique(CasMEF2019$v3Profesión)
Remover<-c("^En |En$","^O |O$","^De |De$","Oo$","De La$","O Ingeniería$","Ingeniería$",
           "^3 O 4 Años  En","Ciencias$","Ciencia$","^3 Ó 4 Años","^Básica En","^Básica  1 Ó 2 Años  En",
           "Ingenieria$","O Ingeniera$", "Superior  3 O 4 Años","^Nivel De Estudio      En",
           "^30 4 Años  En","^Asias","Uni Itari E A","O Afines Por La$")  #Inicio y Final

Remover<-str_c(Remover,collapse = "|")

CasMEF2019$v3Profesión<-CasMEF2019$v3Profesión%>%
  str_remove_all(pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
  str_trim()%>%
  str_remove(pattern = "Y$")%>%
  str_remove(pattern = "Ingenieria$")%>%
  str_remove(pattern = "O$")%>%
  str_trim()



unique(CasMEF2019$v3Profesión)
#---- v4 #----
CasMEF2019$V4<-as.character(CasMEF2019$V4)
unique(CasMEF2019$V4)
Remover<-c("^En |En$","^A |A$","^O |O$","^Académico Y O","^De |De$","^Ls","^La ","^Lo ","^E 0","Ingeniería$","O Afines",
           "^Programa De","^Académica  Grado","^Académico V O Nivel De Estudio Política",
           "^Formación Académica  Grado","60 Horas$","Por La$",
           "El$", "60 Hora$","40 Horas$","20 Horas$","20$","^No ",
           "^Formación   Grado De","De La$","50 Horas$","^2 Loa Gestión Pública","^Los",
           "^Oa","^Loa","40 Horas Acumuladas$","Oo$","60$","60 Horas Acumuladas$",
           "Declarar Y$","^C A")  #Inicio y Final

Remover<-str_c(Remover,collapse = "|")
CasMEF2019$V4<-CasMEF2019$V4%>%
  str_remove_all(pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
  str_trim()

CasMEF2019$V4<-CasMEF2019$V4%>%
  str_remove(pattern = "^Nivel")%>%
  str_remove(pattern = "Ingeniería$")%>%
  str_remove(pattern = "Y$")%>%
  str_trim()%>%
  str_remove(pattern = "^De |Del$")%>%
  str_trim()%>%
  str_remove(pattern = "^Estudio")%>%
  str_remove(pattern = "20 Horas Acumuladas")%>%
  str_remove(pattern = "^O Grado Académico De")%>%
  str_remove(pattern = "^Ad Ss")%>% #
  str_remove(pattern = "^Grado De")%>%
  str_remove(pattern = "^Los")%>%
  str_remove(pattern = "^Genera Nta")%>%
  str_trim()%>%
  str_remove(pattern = "^0 Los")%>%
  str_remove(pattern = "^O |O$")%>%
  str_remove(pattern = "^Grado Académico")%>%
  str_remove(pattern = "^Y O")%>%
  str_remove(pattern = "^En")%>%
  str_remove(pattern = "1 O 2 Años")%>%
  str_trim()

#
unique(CasMEF2019$V4)

#---- V5 #----

CasMEF2019$V5<-as.character(CasMEF2019$V5)
unique(CasMEF2019$V5)

Remover<-c("^En |En$","^A |A$","^0|^O |O$","^De |De$","^Oo","^Conocimientos   ","60 Horas$",
           "Con El$","Que$","^Ls ","^Académico Y O","^Formación","^Nivel De Estudio",
           "^E 0","O Afines","Y O Uso$","El Cual$","Ingeniería$","^A ","50 Horas$",
           "40 Horas$","40 Horas Acumuladas", "20 Horas$","Bpm$","Po   E$","^C Oo",
           "El Cual Deberá$","Duración No Menor De Doce  12  Horas Lectivas$",
           "60 Horas Acumuladas$","^No ","^Académica  Grado","Nivel$","E   O  L Da$",
           "En El$","Al$","A Los$","2$","O E$","Como$","Horas Acumuladas$",
           "Una Duración No Menor De Doce  12  Horas Lectivas","60 Hora$","O Po O Y$",
           "O Ys Lar O$","Es$","^Lo ","O E Rs Ls$","Deberá Tener Una Duración No Menor De Doce  12  Horas",
           "Entre Otros$","60$","^C Oa","^C A","U$")  #Inicio y Final

Remover<-str_c(Remover,collapse = "|")
CasMEF2019$V5<-CasMEF2019$V5%>%
  str_remove_all(pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
  str_trim()

CasMEF2019$V5<-CasMEF2019$V5%>%
  str_remove(pattern = "Y$")%>% 
  str_remove(pattern = "^O|O$")%>% 
  str_remove(pattern = "^Nivel De Estudio")%>% 
  str_remove(pattern = "^Programas De")%>% 
  str_remove(pattern = "^Académica  Grado")%>% 
  str_remove(pattern = "Y O Gestión Por$")%>% 
  str_remove(pattern = "^De No Acreditar El")%>% 
  str_remove(pattern = "Y Egresado$")%>% 
  str_remove(pattern = "A Nivel$")%>% 
  str_remove(pattern = "Tener$")%>% 
  str_remove(pattern = "Sistema$")%>% 
  str_remove(pattern = "En El Sector$")%>%
  str_trim()%>%
  str_remove(pattern = "O A$")%>%
  str_remove(pattern = "Y O S10$")%>%
  str_remove(pattern = "O Ingeniería$")%>%
  str_remove(pattern = "O Ingenieria$")%>%
  str_remove(pattern = "En Relación$")%>%
  str_remove(pattern = "Modelo$")%>%
  str_remove(pattern = "O Ingenieria$")%>%
  str_trim() # 
  
#---- V6 #----
CasMEF2019$V6<-as.character(CasMEF2019$V6)

Remover<-c("^En |En$","^A |A$","^0|0$|^O |O$","^De |De$","^Oo|Oo$","^Y|Y$","^Del|Del$",
           "^Conocimientos   ","^Programa De","^Nivel De Estudio","^Conocimientos O",
           "60 Horas$","Con El$","Que$","^Ls ","^Académico Y O","^Formación","^Académica  Grado",
           "^Se","^Cuenta Con El","Po$","Recursos$","80 Horas Acumuladas$",
           "60 Horas Acumuladas$","El Cual Deberá Tener Una Duración No$","Nivel$",
           "O O Y  2 Ae E$","El Cual Deberá Tener Una Duración No Menor De Doce  12$",
           "El Cual Deberá$","Necesario Sólo En Caso De No Acreditar El Título$",
           "^E 0","O Afines","Y O Uso$","El Cual$","Ingeniería$","De La$","50 Horas$",
           "40 Horas$","40 Horas Acumuladas","20 Horas$","Po   E$","^C Oo","Afines$",
           "El Cual Deberá$","Duración No Menor De Doce  12  Horas Lectivas","60$","O E$",
           "^No ","E   O  L Da$","La  O  L$","Mm$","El Cual Deberá Tener Una$",
           "En El$","Al$","A Los$","2$","Como$","Horas Acumuladas$","Me On O","Ee$",
           "Una Duración No Menor De Doce  12  Horas Lectivas","60 Hora$","L Er$","O La Poy$",
           "O Ys Lar O$","Es$","^Lo |Lo$","O E Rs Ls$","Deberá Tener Una Duración No Menor De Doce  12  Horas",
           "Entre Otros$","^C Oa","^C A","U$","Q   Y Gue  Progral     5$","Ao L E$")  #Inicio y Final
Remover<-str_c(Remover,collapse = "|")

CasMEF2019$V6<-CasMEF2019$V6%>%
  str_remove_all(pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
  str_trim()

CasMEF2019$V6<-CasMEF2019$V6%>%
  str_remove(pattern = "Y   Y$")%>%
  str_remove(pattern = "^Nivel De Estudio|^O|O$|Y O Uso$|^Afines|En Relación$|Modelo$|E$|^E ")%>%
  str_trim()%>%
  str_remove(pattern = "Ca  Co  Pos$|Ce Lo$|12  Horas Lectivas$|20$|La Poy$")%>%
  str_remove(pattern = "^Académico Ylo Nivel De Estudio|Y  2 Ae|12  Horas$")%>%
  str_trim()


unique(CasMEF2019$V6)

#---- V7 #----
CasMEF2019$V7<-as.character(CasMEF2019$V7)

unique(CasMEF2019$V7)

Remover<-c("^En |En$","^A |A$","^0|0$|^O |O$","^De |De$","^Oo|Oo$","^Y|Y$","^Del|Del$",
           "^La |La$","^Conocimientos   ","^Programa De","^Nivel De Estudio","^Conocimientos O",
           "60 Horas$","Con El$","Que$","^Ls ","^Académico Y O","^Formación","^Académica  Grado",
           "^Se","^Cuenta Con El","Po$","Recursos$","80 Horas Acumuladas$","O Base$","^Experiencia   ",
           "60 Horas Acumuladas$","El Cual Deberá Tener Una Duración No$","Nivel$","Con Énfasis$",
           "O O Y  2 Ae E$","El Cual Deberá Tener Una Duración No Menor De Doce  12$","^C O",
           "El Cual Deberá$","Necesario Sólo En Caso De No Acreditar El Título$","An$",
           "^E 0","O Afines","Y O Uso$","El Cual$","Ingeniería$","50 Horas$","O  O Po  O 7$",
           "40 Horas$","40 Horas Acumuladas","20 Horas$","Po   E$","^C Oo","Afines$",
           "El Cual Deberá$","Duración No Menor De Doce  12  Horas Lectivas","60$","O E$",
           "^No ","E   O  L Da$","La  O  L$","Mm$","El Cual Deberá Tener Una$","En El Sector$",
           "En El$","Al$","A Los$","2$","Como$","Horas Acumuladas$","Me On O","Ee$","Y Obras$",
           "Una Duración No Menor De Doce  12  Horas Lectivas","60 Hora$","L Er$","O La Poy$",
           "O Ys Lar O$","Es$","^Lo |Lo$","O E Rs Ls$","Deberá Tener Una Duración No Menor De Doce  12  Horas",
           "Entre Otros$","^C Oa","^C A","U$","Q   Y Gue  Progral     5$","Ao L E$","O O E Z$",
           "80 Horas$","^Oa") #Inicio y Final
Remover<-str_c(Remover,collapse = "|")

CasMEF2019$V7<-CasMEF2019$V7%>%
  str_remove_all(pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
  str_trim()

CasMEF2019$V7<-CasMEF2019$V7%>%
  str_remove(pattern = "^De |De$|^0|0$|^O |O$|^L$|Co  L$|^O E Z$|^Po  O 7$|^A |A$|^is|^Y |Y$")%>%
  str_remove(pattern = "^Lor$|^Nivel De Estudio|Ls$|A 7$|Y O Uso$|^Afines|En Relación$|Modelo$|E$|^E ")%>%
  str_trim()%>%
  str_remove(pattern = "Co$|Ce Lo$|Un Jor$|A   Pro$|^Z$|^Académica  Grado|^Programas De")%>%
  str_trim()

unique(CasMEF2019$V7)
#---- V8 #----
CasMEF2019$V8<-as.character(CasMEF2019$V8)

unique(CasMEF2019$V8)

Remover<-c("^En |En$","^A |A$","^0|0$|^O |O$","^De |De$","^Oo|Oo$","^Y|Y$","^Del|Del$",
           "^La |La$","^Conocimientos   ","^Programa De","^Nivel De Estudio","^Conocimientos O",
           "60 Horas$","Con El$","Que$","^Ls ","^Académico Y O","^Formación","^Académica  Grado",
           "^Se","^Cuenta Con El","Po$","Recursos$","80 Horas Acumuladas$","O Base$","^Experiencia   ",
           "60 Horas Acumuladas$","El Cual Deberá Tener Una Duración No$","Nivel$","Con Énfasis$",
           "O O Y  2 Ae E$","El Cual Deberá Tener Una Duración No Menor De Doce  12$","^C O",
           "El Cual Deberá$","Necesario Sólo En Caso De No Acreditar El Título$","An$",
           "^E 0","O Afines","Y O Uso$","El Cual$","Ingeniería$","50 Horas$","O  O Po  O 7$",
           "40 Horas$","40 Horas Acumuladas","20 Horas$","Po   E$","^C Oo","Afines$",
           "El Cual Deberá$","Duración No Menor De Doce  12  Horas Lectivas","60$","O E$",
           "^No ","E   O  L Da$","La  O  L$","Mm$","El Cual Deberá Tener Una$","En El Sector$",
           "En El$","Al$","A Los$","2$","Como$","Horas Acumuladas$","Me On O","Ee$","Y Obras$",
           "Una Duración No Menor De Doce  12  Horas Lectivas","60 Hora$","L Er$","O La Poy$",
           "O Ys Lar O$","Es$","^Lo |Lo$","O E Rs Ls$","Deberá Tener Una Duración No Menor De Doce  12  Horas",
           "Entre Otros$","^C Oa","^C A","U$","Q   Y Gue  Progral     5$","Ao L E$","O O E Z$",
           "80 Horas$","^Oa","Co Em$","^Experiencia General Y Específica    ","A An$","Oy$","A E$",
           "^Os Een","^Experiencia General V Específica   ") #Inicio y Final 
Remover<-str_c(Remover,collapse = "|") # 

CasMEF2019$V8<-CasMEF2019$V8%>%
  str_remove_all(pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
  str_trim()

CasMEF2019$V8<-CasMEF2019$V8%>%
  str_remove(pattern = "^De |De$|^0|0$|^O |O$|^En |En$|Py$|^O E Z$|^Laboral |^A |A$|^is|^Y |Y$")%>%
  str_remove(pattern = "^7  |^Nivel De Estudio|^1  |A 7$|^E$|^Afines|En Relación$|Modelo$|E$|^E ")%>%
  str_trim()


unique(CasMEF2019$V8)

#---- V9 #----

CasMEF2019$V9<-as.character(CasMEF2019$V9)

unique(CasMEF2019$V9)

CasMEF2019$V9<-CasMEF2019$V9%>%
  str_remove_all(pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
  str_trim()
#
CasMEF2019$V9<-CasMEF2019$V9%>%
  str_remove(pattern = "^De |De$|^0|0$|^O |O$|^En |En$|^E$|O Sector$|^A |A$|Cn$|^Y |Y$|^is ")%>%
  str_remove(pattern = "^7 |^Nivel De Estudio|^1  |A 7$|^E$|^Afines|En Relación$|Modelo$|E$|^E ")%>%
  str_remove(pattern = "^Experiencia Laboral  |^Se Cuenta Con El|^Os 0er")%>%
  str_trim()

unique(CasMEF2019$V9)

#---- Bucle #----
sapply(CasMEF2019, class)
unique(CasMEF2019$V10)
#Aplicamos bucle para las demás columnas
CasMEF2019[,10]

for (j in 6:35) {
  CasMEF2019[,j]<-as.character(CasMEF2019[,j])
  CasMEF2019[,j]<-str_remove_all(CasMEF2019[,j],pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
    str_trim()
  
  CasMEF2019[,j]<-CasMEF2019[,j]%>%
    str_remove_all(pattern = "^De |De$|^0|0$|^O |O$|^En |En$|^E$|O Sector$|^A |A$|Cn$|^Y |Y$|^is |Por$|Tener$")%>%
    str_remove_all(pattern = "^Nivel De Estudio|^1  |A 7$|^E$|^Afines|En Relación$|Modelo$|E$|^E |Para$|Ingeniería$")%>%
    str_remove_all(pattern = "^Experiencia Laboral  |^Se Cuenta Con El|^Os 0er|^Fin De|^Oa|^Con |Con$|A Fin$")%>%
    str_remove_all(pattern = "^Ylo Nivel De Estudio|^Ca  Co  Pos$|2$|^La Poy$|Ce Lo$|^Grado |Ss$|^Lectivas$")%>%
    str_trim()%>%
    str_remove_all(pattern = "^Programas De|^Pp|^P |^El |El$|^Al |Al$|^Con |Con$|A Fin$|Así$|Incluyen$|^Académico|^Loa |Loa$|Ri$")%>%
    str_remove_all(pattern = "^Los |Los$|^Académica|^Formación|Declarar$|^Nivel|^Estudio|^Grado Académico De|^Ad Ss|Ingenieria|^Or$")%>%
    str_remove_all(pattern = "^Grado De|^Genera Nta|^Grado Académico|1 O 2 Años|^De |De$|^0|0$|^O |O$|^En |En$|^E$|^Y |Y$|^A  A|Z$")%>%
    str_remove_all(pattern = "^La Materia  En El|^A   Pro$|^Un Jor$|Co$|Ls$|^Lor$|^Po  O 7$|^Z$|^L$|Py$|^Cv$|^La|^Os|^Li$|^2 Ae$|^L |^s ")%>%
    str_trim()
}

sapply(CasMEF2019, class)

#Guardamos la data limpia

saveRDS(CasMEF2019,"MEF/DataLimpiaMEF.rds")


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




