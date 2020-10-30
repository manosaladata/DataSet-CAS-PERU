#----Proyecto DataSET CAS PERÚ----
# Pagina estática 
rm(list = ls())

# activamos librerias para hacer web scraping

  
# Llamamos las librerias

library(tidyverse)
#library(readr)
#library(stringr)    # manejador de string (objeto, patron)
#library(dplyr)
#library(tidyr)
#library(purrr)
library(xml2)
library(rvest)       #scrapea paginas estáticas
#library(RSelenium)  #escrapea paginas dinámicas
#library(wdman)      #Navegación fantasma para rselenium
library(tm)
library(pdftools)
library(tesseract)
library(magick)
library(robotstxt)
# https://brandominus.com/blog/creatividad/todas-etiquetas-html5/ : pagina importante para ver etiquetas en HTML

#----CAS MEF-2019-2020#----

URL<- "https://www.mef.gob.pe/contenidos/cas/procesos_2019_concluidas.php"

#Preguntar si esta premitio bajar los datos de la  web

paths_allowed(paths = c(URL)) # si se puede

#guess_encoding(URL) # para ver la codificación

WebPag<-read_html(URL) #Leemos la URL(toda la página) y guardamos en un objeto

#Escrapeamos la etiqueta clase "esp"

CasMEF2019<-WebPag%>%
  html_nodes(css=".esp")%>%
  html_text()%>%
  str_replace_all("[^ [: alnum:]]" , "" )%>%  # Eliminar no alfanumérico y ("[[:punct:]]", " ") elimina sig. de puntuacion
  str_trim()%>% #Recorta espacios en blanco antes y después
  str_subset("[:alnum:]")%>%                  #Extrae solo alfanuméricos
  str_replace_all("Proceso Concluido","")%>%  #reeplaza proceso por nada
  str_replace_all("Cerrado","Concluido")%>%   #reeplaza cerrado por concluído
  str_trim()%>%
  matrix(ncol = 3,byrow=TRUE) %>%            #Convierte en una matriz
  as.data.frame()                        # convertimos en un dataframe


#Convertimos en data estructurada


#Asignamos nombres específicos

names(CasMEF2019)[1:3]<-c("Proceso","Convocatoria","Estado")
N<-count(CasMEF2019) #Cuenta cuantas obsevaciones tiene el DataFrame

# Añadir nuevas columnas con sus valores string

CasMEF2019<-cbind(CasMEF2019,Entidad=rep("Ministerio de economía y finanzas",N),Id_Entidad=rep("MEF",N))

#saveRDS(CasMEF2019,"CasMEF2019.rds")

#str_split("Proceso") # divide cuando ve proceso.

# Posibles nombres de la dataset

#Nombres<-c("Proceso","Convocatoria","Estado","Entidad")

#----Descarga los links de pdfs#----

# Escrapeamos la etiqueta clase "a" que contiene enlases de pdf
#Necesito sólo las convocatorias y resultado final

Cas2019pdf_link<-WebPag%>%
  html_nodes("a")%>%
  html_attr("href")%>%
  str_subset(c("ACTA_EVALUACION_CURRICULAR"), negate = T)%>% # No filtra ese patrón de pdf
  str_subset("FICHA_POSTULANTE_ANEXOS",negate = T)%>% # No filtra ese patron de pdfs
  str_subset("ACTA_PRUEBA_CONOCIMIENTOS",negate = T)%>% # No filtra ese patrón de pdf
  str_subset("POSTERGACION", negate = T)%>% #No filtra ese patrón de pdf
  str_subset("ERRATAS",negate = T)%>%
  str_subset("ACTA_PRUEBA_COMPETENCIAS",negate = T)%>%
  str_subset("CRONOGRAMA",negate = T)

N*2 # debería tener esa cantidad de pdfs

#Separamos pdf Convocatoria y pdf resultado final

#----PDF de las bases#----

PdfBases2019<-Cas2019pdf_link%>%
  str_subset("RESULTADO_FINAL",negate = T) # Filtro las bases: son 455 pdfs

NBases<-length(PdfBases2019) # Número de las bases igual a N

#---- Leer los pdfs  bases #----

UrlMadre<-"https://www.mef.gob.pe"

#Urlpdf<-"https://www.mef.gob.pe/contenidos/cas/doc/BASES_002_2019_ESPECIALISTA_EN_CERTIFICADOS_DE_INVERSION_PUBLICA_CIPRL.pdf"

# Vamos descargar y leer pdf 1

#tesseract_info() # verificamos qué idiomas lee en principio: sólo tenía el inglés
#tesseract_download("spa") #isntalamos datos de entrenamiento en español, sólo una vez.

#----- Parte individual#----

Pdf_Base_MEF2019<-pdf_ocr_text(paste0(UrlMadre,PdfBases2019[100]),pages = c(2:4),language = "spa")%>% # entre corchetes lee un determinado pdf
  str_split(pattern = "\n")%>%
  unlist()%>%
  str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por nada
  str_to_title(locale = "en")%>% #convierte en forma de título
  str_trim() # recorta espacios en blanco



#Pdf_Base_MEF2019[1] #leemos la página 1

#Creamos palabras claves para filtrar sus oraciones

Palabra_clave<-c("Proceso Cas","Pc N","Posiciones","Cantidad","Título","Bachiller","Egresado","Secundaria",
                 "Ingeniería","Conocimientos","Conocimiento","Cursos","Curso","Especialización","Razonamiento","Redacción",
                 "Atención","Experiencia","Funciones","Analizar","Revisar","Procesar","Emitir","Participar",
                 "Efectuar","Orientar","Asistir","Facilitar","Identificar","Brindar","Colaborar","Recoger","Formular","Elaborar","Realizar","Evaluar",
                 "Absolver","Ejecutar","Conducir","Coadyuvar","Proponer","Integrar","Hacer","Apoyar","Validar",
                 "Recopilar","Coordinar","Recibir","Organizar","Planificar","Atender","Programar","Asesorar",
                 "Distribuir","Desarrollar","Representar","Diseñar","Implementar","Consolidar","Gestionar",
                 "Departamento","Hasta","Mil")

KeyWord_match <- str_c(Palabra_clave, collapse = "|")


#Filtramos las oraciones o fraces que tengan los keywords

#rm(Pdf_Base_MEF2019)

Pdf_Base_MEF2019<-Pdf_Base_MEF2019%>%
  str_subset(pattern = KeyWord_match,negate = F)%>%
  str_subset("Prueba",negate = T)%>%
  str_subset("Dependencia",negate = T)%>%
  str_subset("Mínimo",negate = T)%>%
  str_subset("Mínima",negate = T)%>%
  str_subset("Cálculo",negate = T)%>%
  str_subset("Calculo",negate = T)

Pdf_Base_MEF2019<-Pdf_Base_MEF2019%>%
  matrix(ncol = length(Pdf_Base_MEF2019),byrow = T)%>%
  as.data.frame()

#----For #----

# j es el contador

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data") # Para guardar los archiivos

for (j in 392:392) {
  
  NombrePDF<-paste0("Pdf_Base_MEF2019_",j)
  
  NombrePDF<-pdf_ocr_text(paste0(UrlMadre,PdfBases2019[j]),pages = c(2:4),language = "spa")%>% # entre corchetes lee un determinado pdf
    str_split(pattern = "\n")%>%
    unlist()%>%
    str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por nada
    str_to_title(locale = "sp")%>% #convierte en forma de título en español
    str_trim()
  
  #Luego sigue
  
  Palabra_clave<-c("Proceso Cas","Pc N","Posiciones","Cantidad","Título","Bachiller","Egresado","Técnica","Secundaria",
                   "Ingeniería","Conocimientos","Conocimiento","Cursos","Curso","Especialización","Razonamiento","Redacción",
                   "Atención","Años","Experiencia","Funciones","Analizar","Revisar","Procesar","Emitir","Participar",
                   "Identificar","Brindar","Colaborar","Recoger","Formular","Elaborar","Realizar","Evaluar","Difundir","Impulsar",
                   "Efectuar","Orientar","Asistir","Facilitar","Absolver","Ejecutar","Conducir","Coadyuvar","Sistematizar",
                   "Asegurar","Documentar","Proponer","Integrar","Hacer","Apoyar","Validar","Recopilar","Coordinar","Recibir",
                   "Organizar","Determinar","Registrar","Generar","Planificar","Atender","Programar","Asesorar","Distribuir",
                   "Desarrollar","Representar","Diseñar","Implementar","Consolidar","Gestionar","Actualizar","Mantener",
                   "Proporcionar","Establecer","Monitorear","Supervisar","Verificar","Cumplir","Comunicar","Mantenimiento",
                   "Departamento","Hasta","Mil","Soles")
  
  KeyWord_match <- str_c(Palabra_clave, collapse = "|")
  
  #Luego sigue
  NombrePDF<-NombrePDF%>%
    str_subset(pattern = KeyWord_match,negate = F)%>%
    str_subset("Sustentadora",negate = T)%>%
    str_subset("Dependencia",negate = T)%>%
    str_subset("Mínimo",negate = T)%>%
  #  str_subset("Mínima",negate = T)%>%
   # str_subset("Cálculo",negate = T)%>%
   # str_subset("Calculo",negate = T)%>%
    str_subset("Realizarse",negate = T)%>%
    str_subset("Veracidad",negate = T)%>%
    str_subset("Anexo",negate = T)%>%
    str_subset("Fe",negate = T)%>%
    str_subset("Objeto",negate = T)%>%
    #str_subset("Usuario",negate = T)%>%
    str_subset("Manual",negate = T)%>%
    str_subset("Entrevista Personal",negate = T)%>%
    str_subset("Declarado", negate = T)%>%
    str_subset("Montos Y Afiliaciones", negate = T)%>%
    str_subset("Registrarse", negate = T)
  
  #Convierte en data estructurada
  NombrePDF<-NombrePDF%>%
    matrix(ncol = length(NombrePDF),byrow = T)%>%
    as.data.frame()
  
  saveRDS(NombrePDF,paste0(j,".rds"))
  #Para no confundir con un bot
  Sys.sleep(1)
  }

gc()# limpia de la memoria 
rm(NombrePDF)

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MEF")

list.dirs() # ver los diectorios
list.files() # vemos todos los archivos de nuestra carpeta


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


#----PDFs de los resultados#----

pdfResul2019<-Cas2019pdf_link%>%
  str_subset("RESULTADO_FINAL",negate = F) # Filta los resultados: son 455 pdfs

# para CV
#install.packages("pagedown") # Paginar la salida HTML de R Markdown con CSS para imprimir, ya no requiere de latex
#install.packages("scholar") #

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MEF")
DATAWEBMEF<-read_rds(file = "CasMEF2019.rds")
DATAMEFPDF<-read_rds(file = "DataMEF_445.rds")

