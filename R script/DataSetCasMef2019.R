#----Proyecto DataSET CAS PERÚ----
#
rm(list = ls())

# Instalamos librerias para hacer web scraping
install.packages("xml2")
install.packages("rvest")     #Navega en páginas web estáticas
install.packages("RSelenium") # Navega en páginas web dinámicas
install.packages("pdftools")  # Para extraer textos y otros de PDFs
install.packages("tesseract") #OCR reconocimiento óptico de caracteres: 
# El paquete anterior admite mas de 100 idiomas, permite extraer texto de pdf como imagen.
install.packages('tm')      #Tex mining
install.packages("magick") # pdf como imagen, mejora la imagen para ser leído.

# Llamamos las librerias

library(tidyverse)
library(readr)
library(xml2)
library(rvest)      #scrapea paginas estáticas
library(RSelenium)  #escrapea paginas dinámicas
library(wdman) # Navegación fantasma para rselenium
library(stringr)    # manejador de string (objeto, patron)
library(tm)
library(dplyr)
library(tidyr)
library(pdftools)
library(tesseract)
library(magick)

# https://brandominus.com/blog/creatividad/todas-etiquetas-html5/ : pagina importante para ver etiquetas en HTML

#----CAS MEF-2019-2020#----

URL<- "https://www.mef.gob.pe/contenidos/cas/procesos_2019_concluidas.php"

#guess_encoding(URL) # para ver l codificación

WebPag<-read_html(URL) #Leemos la URL(toda la página) y guardamos en un objeto

#Escrapeamos la etiqueta clase "esp"

CasMEF2019<-WebPag%>%
  html_nodes(css=".esp")%>%
  html_text()%>%
  str_replace_all("[^ [: alnum:]]" , "" )%>% # Eliminar no alfanumérico y ("[[:punct:]]", " ") elimina sig. de puntuacion
  str_trim()%>% #Recorta espacios en blanco antes y después
  str_subset("[:alnum:]")%>%                 #Extrae solo alfanuméricos
  str_replace_all("Proceso Concluido","")%>% #reeplaza proceso por nada
  str_replace_all("Cerrado","Concluido")%>%  #reeplaza cerrado por concluído
  str_trim()%>%
  matrix(ncol = 3,byrow=TRUE) %>%            #Convierte en una matriz
  as.data.frame()                           # convertimos en un dataframe


#Convertimos en data estructurada


#Asignamos nombres específicos

names(CasMEF2019)[1:3]<-c("Proceso","Convocatoria","Estado")
N<-count(CasMEF2019) #Cuenta cuantas obsevaciones tiene el DF

# Añadir nuevas columnas con sus valores string

CasMEF2019<-cbind(CasMEF2019,Entidad=rep("Ministerio de economía y finanzas",N),Id_Entidad=rep("MEF",N))

#str_split("Proceso") # divide cuando ve proceso.

# Posibles nombres de la dataset

Nombres<-c("Proceso","Convocatoria","Estado","Entidad")

#----Descarga los links de pdfs#----

# Escrapeamos la etiqueta clase a que contiene enlases de pdf
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

#Vamos leer los pdfs  bases 

UrlMadre<-"https://www.mef.gob.pe"

#Urlpdf<-"https://www.mef.gob.pe/contenidos/cas/doc/BASES_002_2019_ESPECIALISTA_EN_CERTIFICADOS_DE_INVERSION_PUBLICA_CIPRL.pdf"

# Vamso descargar y leer pdf 1

#tesseract_info() # verificamos qué idiomas lee en principio: sólo tenía el inglés
#tesseract_download("spa") #isntalamos datos de entrenamiento en español, sólo una vez.

#----- Parte individual#----

Pdf_Base_MEF2019<-pdf_ocr_text(paste0(UrlMadre,PdfBases2019[1]),pages = c(2:4),language = "spa")%>% # entre corchetes lee un determinado pdf
  str_split(pattern = "\n")%>%
  unlist()%>%
  str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por nada
  str_to_title(locale = "en")%>% #convierte en forma de título
  str_trim() # recorta espacios en blanco



#Pdf_Base_MEF2019[1] #leemos la página 1

#Creamos palabras claves para filtrar sus oraciones

Palabra_clave<-c("Proceso Cas","Pc N","Posiciones","Cantidad","Título","Bachiller","Egresado","Secundaria",
                 "Ingeniería","Conocimientos","Conocimiento","Cursos","Curso","Razonamiento","Redacción",
                 "Atención","Experiencia","Funciones","Analizar","Revisar","Procesar","Emitir","Participar",
                 "Identificar","Brindar","Colaborar","Recoger","Formular","Elaborar","Realizar","Evaluar",
                 "Absolver","Ejecutar","Conducir","Coadyuvar","Proponer","Integrar","Hacer","Apoyar",
                 "Recopilar","Coordinar","Recibir","Organizar","Planificar","Atender","Programar",
                 "Distribuir","Desarrollar","Representar","Diseñar","Implementar","Consolidar","Gestionar",
                 "Departamento","Duración","Hasta","Mil")

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

#---De manera manual poner los nombres correspndientes #----

str(Pdf_Base_MEF2019)
Borrar <- c("V8","V11","V20")
Pdf_Base_MEF2019 <- Pdf_Base_MEF2019[ , !(names(Pdf_Base_MEF2019) %in% Borrar)]

NewName<-c("Proceso","Posiciones","Grado_Académico","Conocimiento1","Conocimiento2",
           "Experiencia_General","Experiencia_esp1","Experiencia_esp2","Habilidades",
           "Función1","Función2","Función3","Función4","Función5","Departamento",
           "Duración_Contrato","Contraprestación")

names(Pdf_Base_MEF2019)[1:17]<-NewName

#----For #----

# j es el contador


for (j in 1:2) {
  
  NombrePDF<-paste0("Pdf_Base_MEF2019_",j)
  
  NombrePDF<-pdf_ocr_text(paste0(UrlMadre,PdfBases2019[j]),pages = c(2:4),language = "spa")%>% # entre corchetes lee un determinado pdf
    str_split(pattern = "\n")%>%
    unlist()%>%
    str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por nada
    str_to_title(locale = "en")%>% #convierte en forma de título en español
    str_trim()
  
  #Luego sigue
  
  Palabra_clave<-c("Proceso Cas","Pc N","Posiciones","Cantidad","Título","Bachiller","Egresado","Secundaria",
                   "Ingeniería","Conocimientos","Conocimiento","Cursos","Curso","Razonamiento","Redacción",
                   "Atención","Experiencia","Funciones","Analizar","Revisar","Procesar","Emitir","Participar",
                   "Identificar","Brindar","Colaborar","Recoger","Formular","Elaborar","Realizar","Evaluar",
                   "Absolver","Ejecutar","Conducir","Coadyuvar","Proponer","Integrar","Hacer","Apoyar",
                   "Recopilar","Coordinar","Recibir","Organizar","Planificar","Atender","Programar",
                   "Distribuir","Desarrollar","Representar","Diseñar","Implementar","Consolidar","Gestionar",
                   "Departamento","Duración","Hasta","Mil")
  
  KeyWord_match <- str_c(Palabra_clave, collapse = "|")
  
  #Luego sigue
  NombrePDF<-NombrePDF%>%
    str_subset(pattern = KeyWord_match,negate = F)%>%
    str_subset("Prueba",negate = T)%>%
    str_subset("Dependencia",negate = T)%>%
    str_subset("Mínimo",negate = T)%>%
    str_subset("Mínima",negate = T)%>%
    str_subset("Cálculo",negate = T)%>%
    str_subset("Calculo",negate = T)
  
  #Convierte en data estructurada
  NombrePDF<-NombrePDF%>%
    matrix(ncol = length(NombrePDF),byrow = T)%>%
    as.data.frame()
  
 
}


  
#----PDFs de los resultados#----

pdfResul2019<-Cas2019pdf_link%>%
  str_subset("RESULTADO_FINAL",negate = F) # Filta los resultados: son 455 pdfs

# para CV
#install.packages("pagedown") # Paginar la salida HTML de R Markdown con CSS para imprimir, ya no requiere de latex
#install.packages("scholar") #
