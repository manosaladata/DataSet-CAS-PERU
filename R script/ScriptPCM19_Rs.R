# Páginas dinámicas

cat("\014")     #Limpia la consola
rm(list = ls()) # limpia la memoria de trabajo

#Prevaiamente se instala los paquetes
# Llamamos las librerias

library(tidyverse) # Carga todos los paquetes que contiene
#library(readr)
#library(stringr)    # manejador de string (objeto, patron)
#library(dplyr)
#library(tidyr)
#library(purrr)
library(xml2)
library(rvest)
library(RSelenium)   # Navega en páginas web dinámicas
library(wdman) # Navegación fantasma para rselenium
library(robotstxt)
library(binman)
library(tm)
library(pdftools)
library(tesseract)
library(magick)


# Usar la función rsDriver para abrir un navegador,
#usaremos para navegar sobre cualquier página web y posterior captura de información.
# Una vez detectada la clase de la sección utilizamos la función findElement utilizando como argumento el input

#vignette("basics", package = "RSelenium") # ver introduccion

#----Parte Rselenium #----

URL<-"http://convocatorias.pcm.gob.pe/convocatoria.aspx"


#Preguntar si esta premitio bajar los datos de la  web

#paths_allowed(paths = c(URL)) # 
#get_robotstxt(URL) # otra forma de preguntar

#

#acceptAlert()
#Acepta el cuadro de diálogo de alerta que se muestra actualmente
#equivale a hacer clic el botón "Aceptar" en el cuadro de diálogo

#dismissAlert() #Descarta el cuadro de diálogo de alerta que se muestra actualmente en la página
#Para los cuadros de diálogo confirmar () y preguntar (),esto equivale a hacer clic en el botón "Cancelar"
#Para los cuadros de diálogo alert (), esto es equivalente hacer clic en el botón "Aceptar"

# Asignamos como encondig a UTF-8

options(encoding = "utf-8")

#Abrimos una sesion en la web

# Ejecutamos el servidor phantomjs -creamos un navegador fantasma

server<-phantomjs(port=5010L)
#Abrimos el navegador
browser <- remoteDriver(browserName = "phantomjs", port=5010L)
browser$open()

#Navegar la página web que guardamos
browser$navigate(URL)
#browser$acceptAlert()
browser$screenshot(display=TRUE)
#browser$dismissAlert()
BotonAlerta<-browser$findElement(using = 'css',
                                 value='.btn-danger')
BotonAlerta$clickElement()
browser$screenshot(display=TRUE) # si salío 

# Eligimos los años

NodoYears<-browser$findElement(using = 'xpath',
                          value='//*[@id="ctl00_cphBodyMain_reserva1_ddlano"]')
Year<-NodoYears$selectTag()

Year$value[5] # año 2019

# Introducimos el año que queremos

txtYear<- browser$findElement(using = 'css', "#ctl00_cphBodyMain_reserva1_ddlano")
txtYear$clickElement()
txtYear$sendKeysToElement(list(Year$value[5])) # le dije el año 2019
browser$screenshot(display = TRUE)

# Hacer clic en Buscar y ver cuántas hojas contiene el año 2019

Buscar<- browser$findElement(using = 'xpath',
                             value = "//input[@id='ctl00_cphBodyMain_reserva1_imgbuscar']")
Buscar$clickElement()
browser$screenshot(display = TRUE)

#Hacer clic en siguiente

Siguiente<-browser$findElement(using = "xpath",
                           value = "//input[@id='ctl00_cphBodyMain_reserva1_btnsiguiente']")
Siguiente$clickElement()
browser$screenshot(display = TRUE)

# Hacer clic en anterior

Anterior<-browser$findElement(using = "xpath",
                               value = "//input[@id='ctl00_cphBodyMain_reserva1_btnanterior']")
Anterior$clickElement()
browser$screenshot(display = TRUE)

# Obtener el numero de hojas del 2019

NHojas<browser$findElement(using = "xpath",
                          value = "//input[@id='ctl00_cphBodyMain_reserva1_txttotal']")
NHojas$getElementText()
#debe ser = 8

#----Parte Rvest individual #----
# Ahora podemos bajar información con rvest sobre la web actual

Pagina_actual<-browser$getPageSource()

# Extraemos sólo el texto de la hoja N° 01

Hoja1<-read_html(Pagina_actual[[1]])%>% # el elemento 1 de la lista es la url de la página actual
      html_node(css = ".table-condensed")%>%
      html_nodes(".texto")%>%
      html_text()%>%
  str_remove("Nº")%>%
  str_remove("Proceso")%>%
  str_remove("Requisitospara postular")%>% #Limpiamos un poco
  str_remove("Etapa 1")%>%
  str_remove("Etapa 2")%>%
  str_remove("Etapa 3")%>%
  str_remove("Etapa 4")%>%
  str_remove("Comunicado")%>%
  str_subset("[:alnum:]") # Extrea sólo los afanúmericos, sin los saltos

# 

rm(Hoja1_link)

# Extraemos los link de los pdf para leerlos (Hoja 1)

#PDF DE REQUISITOS
#http://reclutamiento.pcm.gob.pe/procesos/S2292019.pdf
#http://reclutamiento.pcm.gob.pe/procesos/S2282019.pdf
#http://reclutamiento.pcm.gob.pe/procesos/S2272019.pdf
#http://reclutamiento.pcm.gob.pe/procesos/S2262019.pdf

Hoja1_linkPdf<-read_html(Pagina_actual[[1]])%>%
            html_nodes(".texto")%>%
            html_nodes("input")%>%
            html_attr("onclick")%>%
  str_remove_all("abrir")%>%
  str_remove_all("return false")%>%
  str_remove_all(";")%>%
  str_remove_all("'")%>%
  str_remove_all("\\(")%>% # como son caracteres reservados, se debe anteponer los backlash
  str_remove_all("\\)")%>%
  str_trim()

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/PCM")

#----Bucle Rvest y Rselenium #----

#Hacemos un bucle para extraer información de las 8 hojas

for(j in 1:8){
  NameObjeto<-paste0("Hoja_",j)
  
  Pagina_actual<-browser$getPageSource() #obtener de la página actual
  
  #baja texto de cada hoja
  
  NameObjeto<-read_html(Pagina_actual[[1]])%>% # el elemento 1 de la lista es la url de la página actual
              html_node(css = ".table-condensed")%>%
              html_nodes(".texto")%>%
              html_text()%>%
    str_remove("Nº")%>%
    str_remove("Proceso")%>%
    str_remove("Requisitospara postular")%>% #Limpiamos un poco
    str_remove("Etapa 1")%>%
    str_remove("Etapa 2")%>%
    str_remove("Etapa 3")%>%
    str_remove("Etapa 4")%>%
    str_remove("Comunicado")%>%
    str_subset("[:alnum:]")%>%
    matrix(ncol = 2,byrow = TRUE)%>%
    as.data.frame()
  saveRDS(NameObjeto,paste0("HojaPCM",j,".rds")) #Guardame cada Hoja
  
  #Ahora baja los links de los pdfs
    
 NamePDFlink<-paste0("PdfLink",j) 
 
 NamePDFlink<-read_html(Pagina_actual[[1]])%>%
              html_nodes(".texto")%>%
              html_nodes("input")%>%
              html_attr("onclick")%>%
    str_remove_all("abrir")%>%
    str_remove_all("return false")%>%
    str_remove_all(";")%>%
    str_remove_all("'")%>%
    str_remove_all("\\(")%>% # como son caracteres reservados, se debe anteponer los backlash
    str_remove_all("\\)")%>%
    str_trim()
    
    saveRDS(NamePDFlink,paste0("PdfBasesPCM",j,".rds")) #Guarda los pdfs de cada hoja
  
  #hacer click en siguente
  
  Siguiente<-browser$findElement(using = "xpath",
                                 value = "//input[@id='ctl00_cphBodyMain_reserva1_btnsiguiente']")
  Siguiente$clickElement()
  #Descansar un segundo para que no nos confunda con un Bot
  Sys.sleep(1)
}

getwd() #vemos nuesstro directorio


list.files() # vemos todos los archivos de nuestra carpeta PCM
Ficheros <- list.files(pattern = "\\.rds")
Hojas<-read_rds(Ficheros[9]) #Observamos hoja por hoja hasta 8
view(Hojas)

#juntamos toda la data de la convocatoria en una sola

Data_PCM<-read_rds(Ficheros[1]) #primero leemos la hoja 1 y ahi lo compilamos hasta la 8

for (j in 2:8) {
  Data_PCM<-bind_rows(Data_PCM,readRDS(Ficheros[j]))
}
#Guardamos la data de la convocatoria

saveRDS(Data_PCM,"DataPCM_221.rds") # Tiene 221 convocatorias


#Juntamos todos los links de los pdfs para luego leerlos

LinkPdf_PCM<-as.data.frame(matrix(read_rds(Ficheros[9]),ncol = 1)) #primero leemos la hoja 1 como datafrane, y ahi lo compilamos hasta la 8

for (j in 10:16) {
  LinkPdf_PCM<-bind_rows(LinkPdf_PCM, as.data.frame(matrix(readRDS(Ficheros[j]),ncol = 1)))
}

#Guardamos las links de los pdfs de la PCM 

saveRDS(LinkPdf_PCM,"DataPdfPCM_221.rds") #Tiene 221 pdfs

#browser$getStatus() para ver el estado actual de servidor
# siempre cerrar la sesión

browser$close()
server$stop()


#----Leer Pdf #----

# Ahora leemos los pdfs
#No necesito usar Rvest ni Rselenium para leerlos, porque ya los descargué el link
#Cuando abrimos el script, venir defrente aqui para leerlos
getwd()
PDFlinks<-read_rds("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/PCM/DataPdfPCM_221.rds")
#Convertimos como lista para acceder a cada pdf
PDFlinks<-as.list(PDFlinks)

#Creamos las palabras claves para y extraerlo de las páginas especificadas

KeyWords<-c("Proceso Cas","Contratar Los Servicios","Contratación De","Contratar A",
            "Título","Titulado","Maestría","Doctorado","Bachiller","Egresado","Diplomado",
            "Técnica","Estudios Técnicos","Secundaria","Nivel De Estudios","Ingeniería",
            "Conocimientos","Conocimiento","Cursos","Curso","Especialización","Capacitación",
            "Manejo","Experiencia General","Experiencia Específica","Orientación A Resultados",
            "Razonamiento","Vocación De Servicio","Trabajo En Equipo","Trabajar En Equipo",
            "Redacción","Organización","Responsabilidad","Planificación","Cooperación","Empatía",
            "Síntesis","Dinamismo","Proactivo","Iniciativa","Analítico",
            "Capacidad De Trabajar Bajo Presión","Comunicación","Atención","Funciones",
            "Asegurar","Asistir","Actualizar","Analizar","Acompañar","Acompañar",
            "Administrar","Análisis","Absolver","Asesorar","Aplicar","Articular","Apoyar",
            "Atender","Brindar","Coadyuvar","Conducir","Cumplir","Comunicar","Contribuir",
            "Controlar","Consolidar","Colaborar","Coordinar","Diseñar","Dirigir","Distribuir",
            "Desarrollar","Depurar","Documentar","Determinar","Difundir","Efectuar","Ejecutar",
            "Evaluar","Elaboración","Emitir","Establecer","Elaborar","Escanear","Facilitar",
            "Fomentar","Formular","Generar","Gestionar","Hacer","Interactuar","Identificar",
            "Impulsar","Integrar","Liderar","Mantener","Mantenimiento","Monitorear","Organizar",
            "Orientar","Participar","Planificar","Programar","Proporcionar","Proponer","Procesar",
            "Proyectar","Recibir","Recolectar","Recopilar","Realizar","Revisión","Revisar","Representar",
            "Registrar","Recoger","Sistematizar","Supervisar","validar","Velar","Verificar","Implementar",
            "Otras Tareas","Otras Asignadas","Departamento","Duración Del contrato","Hasta","Mil","Soles")

KeyMatch<-str_c(KeyWords,collapse = "|")

#Hacer el reading
#readimage<-pdf_convert(paste0(UrlMadrePdf,PDFlinks$V1[10]),pages = c(1:2),language = "spa",dpi = 600)
#readimage<-tesseract::ocr(readimage)

UrlMadrePdf<-"http://reclutamiento.pcm.gob.pe/procesos/"

ReadPDF_PCM<-pdf_ocr_text(paste0(UrlMadrePdf,PDFlinks$V1[221]),pages = c(1:2),language = "spa")

PDFLIMPIA<-ReadPDF_PCM%>%
  str_split(pattern = "\n")%>%
  unlist()%>%
  str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio blanco
  str_replace_all("  "," ")%>%
  str_to_title(locale = "en")%>% #convierte en forma de título en español
  str_trim()%>%
  str_subset(pattern =KeyMatch,negate = F)%>%
  str_subset(pattern = "Vinculados",negate = T)%>%
  str_subset(pattern = "Convocatoria",negate = T)%>%
  str_subset(pattern = "Directiva N",negate = T)%>%
  str_subset(pattern = "Dependencia",negate = T)%>%
  str_subset(pattern = "Comité De Selección",negate = T)%>%
  str_subset(pattern = "Ámbito De Su Competencia",negate = T)%>%
  str_subset(pattern = "Al Menos",negate = T)%>%
  str_subset(pattern = "Cobit",negate = T)%>%
  str_subset(pattern = "Experiencia De Obra",negate = T)%>%
  str_subset(pattern = "Conmemorativos",negate = T)%>%
  str_subset(pattern = "Respecto De Los Procesos",negate = T)%>%
  str_subset(pattern = "Programado",negate = T)%>%
  str_subset(pattern = "Beneficio",negate = T)%>%
  str_subset(pattern = "Pdp",negate = T)


# N° total = 221
# avanzar a 50

# Antes de correr el for, correr PDFlinks,KeyMatch, Keywords y UrlMadrePdf

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/PCM")# Para guardar


for(j in 151:221){
  Namepdf<-paste0("PCM_",j) # Generamos un nombre para cada convocatoria 
  
  Namepdf<-pdf_ocr_text(paste0(UrlMadrePdf,PDFlinks$V1[j]),pages = c(1:2),language = "spa")%>%
    str_split(pattern = "\n")%>%
    unlist()%>%
    str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio blanco
    str_replace_all("  "," ")%>%
    str_to_title(locale = "en")%>% #convierte en forma de título en español
    str_trim()%>%
    str_subset(pattern =KeyMatch,negate = F)%>%
    str_subset(pattern = "Vinculados",negate = T)%>%
    str_subset(pattern = "Convocatoria",negate = T)%>%
    str_subset(pattern = "Directiva N",negate = T)%>%
    str_subset(pattern = "Dependencia",negate = T)%>%
    str_subset(pattern = "Comité De Selección",negate = T)%>%
    str_subset(pattern = "Ámbito De Su Competencia",negate = T)%>%
    str_subset(pattern = "Al Menos",negate = T)%>%
    str_subset(pattern = "Cobit",negate = T)%>%
    str_subset(pattern = "Experiencia De Obra",negate = T)%>%
    str_subset(pattern = "Conmemorativos",negate = T)%>%
    str_subset(pattern = "Respecto De Los Procesos",negate = T)%>%
    str_subset(pattern = "Programado",negate = T)%>%
    str_subset(pattern = "Beneficio",negate = T)%>%
    str_subset(pattern = "Pdp",negate = T)
  
  #Convierte en data estructurada
  Namepdf<-Namepdf%>%
    matrix(ncol = length(Namepdf),byrow = T)%>%
    as.data.frame()
  
  saveRDS(Namepdf,paste0("PCM_",j,".rds"))
  #Para no confundir con un bot
  Sys.sleep(1)
}

#Cargamos la primera observación y luego compilamos los demás
PCM_dataRaspado<-readRDS("DataPCM_221.rds")

list.files() # vemos todos los archivos de nuestra carpeta
#list.dirs() # ver los diectorios

#Listamos los ficheros para leerlos y modificar
Ficheros <- list.files(pattern = "^PCM_") # Extrae sólo los que tienen patron al inicio ese string

#Juntamos la info descargada
for (j in 1:121) {
  PCM_DataB<-bind_rows(PCM_DataB,readRDS(Ficheros[j]))
}
#Guardamos la data juntada
saveRDS(PCM_DataB,"PCM_DataB_221.rds")

unique(PCM_DataB$V2)
PCM_dataRaspado$V2
#readimage<-pdf_convert("D:/Trabajos/Cristian/Practica.pdf",dpi = 600)
#readimage<-tesseract::ocr(readimage,engine ="spa")
#readimageC<-readimage%>%
 # str_split(pattern = "\n")
#cat(readimage)
DATAPCM<-read_rds("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/PCM/PCM_DataB_221.rds")
