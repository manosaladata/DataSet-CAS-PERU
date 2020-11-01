# LIMPIEZA DE DATOS Y ESTRUCTURACIÓN MINSA 2019

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data")

#Datas Limpiadas
#MEF, MINSA,

library(tidyverse)

DataPCM<-read_rds("PCM/PCM_DataB_221.rds")

sapply(DataPCM,class)
unique(DataPCM$V43)
#---- Remover #----
Remover<-c("^Bases Del Concurso Público Del",
           "^Ii ","^Il ","^Ll ","^L ","^Ill ","^Experiencia General   ","^Formacion Academica  ","48 Horas Acumuladas",
           "Mínimo 120 Horas","Mínimo 90 Horas Acumuuladas","24 Horas Acumuladas","Afines A","^Vi ",
           "E ","^Zo |Zo$","^Ón ","^Pos ","^Po |Po$","^Ó |Ó$","[[:upper:]]$","[[:digit:]]$","^Bo |Bo$","^Ba |Ba$",
           "^En |En$","^A |A$","^0 |0$|^O |O$","^De |De$","^Oo|Oo$","^Y|Y$","^Del |Del$","^Aca ","^Loez ",
           "^La |La$","^Conocimientos   ","^Programa De","^Nivel De Estudio","^Conocimientos O",
           "60 Horas$","Con El$","^Que |Que$","^Ls ","^Académico Y O","^Formación Académica  Grado",
           "^Formación   Grado De","^Grado","^Académica  Grado","^Ccacemica","^Acadé Ca","^Ccacómica","^Loa |Loa$","3 O 4 Años",
           "Ciencias$","Ciencia$","3 Ó 4 Años","^Básica En","^Básica  1 Ó 2 Años  En",
           "Ingenieria$","O Ingeniera$", "Superior","^30 4 Años  En","^Asias",
           "Uni Itari A","^Se","^Cuenta Con El","Recursos$","80 Horas Acumuladas$",
           "O Base$","^Experiencia   ","60 Horas Acumuladas$","El Cual Deberá Tener Una Duración No$",
           "Nivel$","Con Énfasis$","^Académico V O Nivel De Estudio Política",
           "O O Y  2 Ae E$","El Cual Deberá Tener Una Duración No Menor De Doce  12$","^C ",
           "El Cual Deberá$","Necesario Sólo En Caso De No Acreditar El Título$","An$",
           "O Afines","Y O Uso$","El Cual$","Ingeniería$","50 Horas$","O  O Po  O 7$",
           "40 Horas$","40 Horas Acumuladas","20 Horas$","Po   E$","^C Oo","Afines$",
           "El Cual Deberá$","Duración No Menor De Doce  12  Horas Lectivas","60$","O E$",
           "^No ","  O  L Da$","La  O  L$","Mm$","El Cual Deberá Tener Una$","En El Sector$",
           "En El$","Al$","^Los |Los$","2$","^Como |Como$","Horas Acumuladas$","Me On O","Ee$","Y Obras$",
           "Una Duración No Menor De Doce  12  Horas Lectivas","60 Hora$","L Er$","O La Poy$",
           "O Ys Lar O$","Es$","^Lo |Lo$","O Rs Ls$","Deberá Tener Una Duración No Menor De Doce  12  Horas",
           "Entre Otros$","U$","Q   Y Gue  Progral     5$","Ao L E$","O O Z$",
           "80 Horas$","^Oa","Co Em$","A An$","Oy$","A E$",
           "^Os Een","^Experiencia General V Específica   ","O   E$","Con$","^P |P$","Alib$",
           "^Laboral ","En E$","L Pr$","^1 ","^2 ","^3 ","^4 ","^5 ","^6 ","^7 ","^8 ",
           "^9 ","^10  ","^11 ","^12 ","^13 ","^14 ","^15 ","Oe$","^Experiencia Laboral   ","Pp$",
           "Py$","^Os Len","^Len ","Le Pr Oa$","^Os 7","^Pp Y Esp","Y El$","Por El Ente$",
           "^Las |Las$","^Q ","Al$","Sus$","^Para |Para$","Asuntos$","Otras$","^S ","^Su |Su$","Por$","Con Una$",
           "Desde Una$","Del Cncf$","Que Le Sea$","Sub$","De La Ult$","Así$","Quien$","Desde$","Entre$","Sin$") #Inicio y Final 
Remover<-str_c(Remover,collapse = "|") #


#---- Bucle #----
sapply(DataPCM, class)

unique(DataPCM$V35)
#Aplicamos bucle para las demás columnas
DataPCM[,10]

for (j in 1:48) {
  DataPCM[,j]<-as.character(DataPCM[,j])
  DataPCM[,j]<-str_remove_all(DataPCM[,j],pattern = Remover)%>% #poner All para que funcione, sino solo toma la 1ra
    str_trim()
  
  DataPCM[,j]<-DataPCM[,j]%>%
    str_remove_all(pattern = "^De |De$|^0 |0$|^O |O$|^En |En$|^E$|O Sector$|^A |A$|Cn$|^Y |Y$|^is |Por$|Tener$")%>%
    str_remove_all(pattern = "^Nivel De Estudio|^1  |A 7$|^E$|^Afines|En Relación$|Modelo$|E$|^E |Para$|Ingeniería$")%>%
    str_remove_all(pattern = "^Experiencia Laboral  |^Se Cuenta Con El|^Os 0er|^Fin De|^Oa|^Con |Con$|A Fin$|^Lo |^Or ")%>%
    str_remove_all(pattern = "^Ylo Nivel De Estudio|^Ca  Co  Pos$|2$|^La Poy$|Ce Lo$|^Grado |Ss$|^Lectivas$|^Os |^Lor ")%>%
    str_trim()%>%
    str_remove_all(pattern = "^Programas De|^Pp|^P |^El |El$|^Al |Al$|^Con |Con$|A Fin$|Así$|Incluyen$|^Académico|^Loa |Loa$|Ri$")%>%
    str_remove_all(pattern = "^Los |Los$|^Académica|^Formación|Declarar$|^Nivel|^Estudio|^Grado Académico De|^Ad Ss|Ingenieria|^Or$")%>%
    str_remove_all(pattern = "^Grado De|^Genera Nta|^Grado Académico|1 O 2 Años|^De |De$|^0|0$|^O |O$|^En |En$|^E$|^Y |Y$|^A  A|Z$")%>%
    str_remove_all(pattern = "^La Materia  En El|^A   Pro$|^Un Jor$|Co$|Ls$|^Lor$|^Po  O 7$|^Z$|^L$|Py$|^Cv$|^La|^Os|^Li$|^2 Ae$|^L |^s ")%>%
    str_remove_all(pattern = "^No |^Lo |^S |No$|^De |De$|^On De |^Les |^Ps A Pa On Y Los |^Da |Da$|^De |^Le |^Oa |^Pp Y Bo |^Eo |^Coq ")%>%
    str_remove_all(pattern = "^D Bl |^Nr Cs |^P Y 8 La |^Nivel De A O|^Cc |^[[:upper:]] |^[[:lower:]] ")%>%
    str_trim()%>%
    str_remove_all(pattern = "^Y O Nivel")%>%
    str_trim()
}

#Guardar la data limpieda

saveRDS(DataPCM,"PCM/DataPCMCleaned.rds")
