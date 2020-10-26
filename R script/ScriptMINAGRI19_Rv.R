#MINAGRI

#Web scraping usando rvest para varios links
library(tidyverse)
#library(purrr)
#library(stringr)
#library(dplyr)
#library(tidyr)
#library(readr)
#library(ggplot2)
library(xml2)
library(rvest)
library(pdftools)
library(tesseract)

urlbase<-""

map_df(1:2,function(i){
  cat(".")
  page<-read_html(sprintf(urlbase,i))
  
  data.frame(IssueID=html_text(html_nodes(page,".results-block__toc-issue-heading")),
             Heading=html_text(html_nodes(page,".results-block__description-heading")),
             Author=html_text(html_nodes(page,".results-block__author")),
             Doin=html_text(html_nodes(page,".results-block__doi")),
             stringsAsFactors = FALSE)
  
})->cochrane2004

#---- Parte 1 #----

URL1<-"https://www.minagri.gob.pe/portal/convocatorias-2019/procesos-cas-hasta-95"
# https://www.minagri.gob.pe/portal/convocatorias-2019/procesos-cas-hasta-95?start=5
# https://www.minagri.gob.pe/portal/convocatorias-2019/procesos-cas-hasta-95?start=10
# https://www.minagri.gob.pe/portal/convocatorias-2019/procesos-cas-hasta-95?start=15
# https://www.minagri.gob.pe/portal/convocatorias-2019/procesos-cas-hasta-95?start=20
# https://www.minagri.gob.pe/portal/convocatorias-2019/procesos-cas-hasta-95?start=120

Hoja1<-read_html(URL1)%>%
  html_node(css = "#content")%>%
  html_nodes(css = ".item")%>%
  html_text()%>%
  str_remove_all("\n")%>%
  str_remove_all("\t")%>%
  str_split("Adjuntos")%>%
  unlist()%>%
  str_subset("SERVICIOS",negate = F)%>%
  str_split("CONTRATACION|CONTRATACIÓN")%>%
  unlist()%>%
  matrix(ncol = 2,byrow = T)%>%
  as.data.frame()

saveRDS(Hoja1,"DataMinagriB1.rds")
# Procesos-cas-hasta-95
getwd()
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINAGRI")
5*24

for (j in 1:24) {
  HojaName<-paste0("Hoja_",j)
  
  HojaName<-read_html(paste0(URL1,"?start=",5*j))%>%
    html_node(css = "#content")%>%
    html_nodes(css = ".item")%>%
    html_text()%>%
    str_remove_all("\n")%>%
    str_remove_all("\t")%>%
    str_split("Adjuntos")%>%
    unlist()%>%
    str_subset("SERVICIOS",negate = F)%>%
    str_split("CONTRATACION|CONTRATACIÓN")%>%
    unlist()%>%
    matrix(ncol = 2,byrow = T)%>%
    as.data.frame()
  
  saveRDS(HojaName,paste0("Minagri",j,".rds"))
  #Descansar un segundo para que no nos confunda con un Bot
  Sys.sleep(1)
}

5*15
#Ahora Juntamos la data
list.files() # vemos todos los archivos de nuestra carpeta MINAGRI
Ficheros <- list.files(pattern = "^Minagri") # Extrae solo los que inician con minagri

Hojas<-read_rds(Ficheros[18]) #Observamos hoja por hoja hasta 24

DataMinagriB1<-read_rds("DataMinagriB1.rds") # Data Madre al que se le adjunta las demás
for (j in 1:24) {
  DataMinagriB1<-bind_rows(DataMinagriB1,readRDS(Ficheros[j]))
}
# Guardamos la primera parte

saveRDS(DataMinagriB1,"DataMinagriB1_121.rds")
#rm(DataBMinagri2,DataBMinagri22,DataBMinagri,Hoja1,HojaName,Hojas)

#----Parte 2 #----

URL2<-"https://www.minagri.gob.pe/portal/convocatorias-2019/proceso-cas-desde-96"

# https://www.minagri.gob.pe/portal/convocatorias-2019/proceso-cas-desde-96?start=5
# https://www.minagri.gob.pe/portal/convocatorias-2019/proceso-cas-desde-96?start=10
# https://www.minagri.gob.pe/portal/convocatorias-2019/proceso-cas-desde-96?start=15
# https://www.minagri.gob.pe/portal/convocatorias-2019/proceso-cas-desde-96?start=20
# https://www.minagri.gob.pe/portal/convocatorias-2019/proceso-cas-desde-96?start=200
5*40

Hoja1<-read_html(URL2)%>%
  html_node(css = "#content")%>%
  html_nodes(css = ".item")%>%
  html_text()%>%
  str_remove_all("\n")%>%
  str_remove_all("\t")%>%
  str_split("Adjuntos")%>%
  unlist()%>%
  str_subset("SERVICIOS",negate = F)%>%
  str_split("CONTRATACION|CONTRATACIÓN")%>%
  unlist()%>%
  matrix(ncol = 2,byrow = T)%>%
  as.data.frame()

saveRDS(Hoja1,"DataMinagriB2.rds")
# Procesos-cas-hasta-95

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINAGRI")


for (j in 1:40) {
  HojaName<-paste0("Hoja_",j)
  
  HojaName<-read_html(paste0(URL2,"?start=",5*j))%>%
    html_node(css = "#content")%>%
    html_nodes(css = ".item")%>%
    html_text()%>%
    str_remove_all("\n")%>%
    str_remove_all("\t")%>%
    str_split("Adjuntos")%>%
    unlist()%>%
    str_subset("CONTRATACION|CONTRATACIÓN",negate = F)%>%
    str_split("CONTRATACION|CONTRATACIÓN")%>%
    unlist()%>%
    matrix(ncol = 2,byrow = T)%>%
    as.data.frame()
  
  saveRDS(HojaName,paste0("Minagri",j,".rds"))
  #Descansar un segundo para que no nos confunda con un Bot
  Sys.sleep(1)
}

saveRDS(HojaName,paste0("Minagri",40,".rds"))
  
#Ahora Juntamos la data
list.files() # vemos todos los archivos de nuestra carpeta MINAGRI
Ficheros <- list.files(pattern = "^Minagri") # Extrae solo los que inician con minagri

Hojas<-read_rds(Ficheros[32]) #Observamos hoja por hoja hasta 40

DataMinagriB2<-read_rds("DataMinagriB2.rds") # Data Madre al que se le adjunta las demás

for (j in 1:40) {
  DataMinagriB2<-bind_rows(DataMinagriB2,readRDS(Ficheros[j]))
}

#fix(DataMinagriB2)# Para modificar algunas dato

DataMinagriB2<-na.omit(DataMinagriB2)
saveRDS(DataMinagriB2,"DataMinagriB2_200.rds")

#---- Juntamos las dos Datas #----
DataMinagriB<-rbind(DataMinagriB1,DataMinagriB2)
saveRDS(DataMinagriB,"DataMinagriB_321.rds")

DataMinagrB<-read_rds("DataMinagriB_321.rds")

#----Descarga link pdfs #----}
#----Pdfs links Parte 1 #----

PdfHoja1<-read_html(URL1)%>%
  html_node(css = "#system")%>%
  html_nodes(css = ".at_url")%>%
  html_attr("href")%>%
  #str_to_title(locale = "sp") #En español
  str_subset("Requisitos|REQUISITOS|Requistos",negate = F)%>%
  matrix(ncol = 1,byrow = T)%>%
  as.data.frame()

saveRDS(PdfHoja1,"LinkPdfMadre.rds")

for (j in 1:24) {
  HojaName<-paste0("PdfLinkHoja_",j)
  
  HojaName<-read_html(paste0(URL1,"?start=",5*j))%>%
    html_node(css = "#system")%>%
    html_nodes(css = ".at_url")%>%
    html_attr("href")%>%
    #str_to_title(locale = "sp") #En español
    str_subset("Requisitos|REQUISITOS|Requistos|Perfil|PERFIL",negate = F)%>%
    matrix(ncol = 1,byrow = T)%>%
    as.data.frame()
  
  saveRDS(HojaName,paste0("PdfLink",j,".rds"))
  #Descansar un segundo para que no nos confunda con un Bot
  Sys.sleep(1)
}

list.files() # vemos todos los archivos de nuestra carpeta MINAGRI
Ficheros <- list.files(pattern = "^PdfLink") # Extrae solo los que inician con PdfLink
PdfLink<-read_rds(Ficheros[18]) #Observamos hoja por hoja hasta 24

DataPdfLinks<-read_rds("LinkPdfMadre.rds") # Data Madre al que se le adjunta las demás

for (j in 1:24) {
  DataPdfLinks<-bind_rows(DataPdfLinks,readRDS(Ficheros[j]))
}

saveRDS(DataPdfLinks,"LinkPdfParte1_108.rds")

#---- Pdfs links Parte 2 #----

PdfHoja2<-read_html(URL2)%>%
  html_node(css = "#system")%>%
  html_nodes(css = ".at_url")%>%
  html_attr("href")%>%
  #str_to_title(locale = "sp") #En español
  str_subset("Requisitos|REQUISITOS|Requistos|Perfil|PERFIL",negate = F)%>%
  matrix(ncol = 1,byrow = T)%>%
  as.data.frame()

saveRDS(PdfHoja2,"LinkPdfMadre2.rds") #No importante


for (j in 1:40) {
  HojaName<-paste0("PdfLinkHoja_",j)
  
  HojaName<-read_html(paste0(URL2,"?start=",5*j))%>%
    html_node(css = "#system")%>%
    html_nodes(css = ".at_url")%>%
    html_attr("href")%>%
    #str_to_title(locale = "sp") #En español
    str_subset("Requisitos|REQUISITOS|Requistos|Perfil|PERFIL|puesto|PUESTO|Puesto",negate = F)%>%
    matrix(ncol = 1,byrow = T)%>%
    as.data.frame()
  
  saveRDS(HojaName,paste0("PdfLink",j,".rds"))
  #Descansar un segundo para que no nos confunda con un Bot
  Sys.sleep(1)
}

list.files() # vemos todos los archivos de nuestra carpeta MINAGRI
Ficheros <- list.files(pattern = "^PdfLink") # Extrae solo los que inician con PdfLink
PdfLink<-read_rds(Ficheros[2]) #Observamos hoja por hoja hasta 24

DataPdfLinks2<-read_rds("LinkPdfMadre2.rds") # Data Madre al que se le adjunta las demás

for (j in 1:40) {
  DataPdfLinks2<-bind_rows(DataPdfLinks2,readRDS(Ficheros[j]))
}
#Juntamos
DataPdfLinksB<-rbind(DataPdfLinks,DataPdfLinks2)

#Eliminamos las que contiene resultados
DataPdfLinksBB<-DataPdfLinksB$V1%>%
  str_subset("Resultados|Resultado|RESULTADOS|RESULTADO",negate = T)%>%
  as.data.frame()

#Guardamos
saveRDS(DataPdfLinksBB,"LinkPdfB_311.rds")


#---- Leer los Pdfs #----

KeyWords<-c("Proceso Cas","Cas N","Objeto","Contratar Los Servicios","Contratación De","Contratar A",
            "Experiencia Laboral","Experiencia General","Experiencia Profesional","Experiencia Especifica",
            "Experiencia Específica","Años","Año","Título","Titulado","Titulada","Licenciado","Licenciada",
            "Maestría","Doctorado","Bachiller","Egresado","Egresada","Diplomado","Técnica","Técnico",
            "Estudios Técnicos","Secundaria","Secundarios","Nivel De Estudios","Ingeniería","Ingeniero",
            "Ingenieria","Economía","Economia","Economista","Contador","Contabilidad","Abogado","Ing",
            "Periodismo","Superior","Carreras","Secretariado","Capacitación","Conocimientos","Conocimiento",
            "Computación","Cursos","Curso","Especialización","Planeamiento","Gestión","Cultura Organizacional",
            "Desarrollo Organizacional","Contrataciones","Sistema","Manejo","Office","Ofimática","Procesador",
            "Hojas de Cálculo","Presupuesto","Presupuestos","Arc Gis","Modernización","Perfiles De Puesto",
            "AutoCad","Formalización","Derecho Laboral","Derecho Administrativo","Seguridad Previsional",
            "Online","Recursos Hídricos","Hidráulicas","Inversión","Siaf","Inglés","Desing Thinking",
            "Cambio Climático","Atención","Análisis","Analítico","Adaptabilidad","Calidad Del trabajo",
            "Comunicación","Compromiso","Cooperación","Dinamismo","Empatía","Iniciativa","Liderazgo","Orientación",
            "Organización","Planificación","Proactividad","Proactivo","Pensamiento Estratégico","Resolución De Conflictos",
            "Responsabilidad","Razonamiento","Redacción","Trabajo En Equipo","Trabajar En Equipo","Vocación",
            "Trabajar Bajo Presión","Síntesis","Funciones","Asegurar","Asistir","Actualizar","Analizar","Acompañar",
            "Administrar","Absolver","Asesorar","Aplicar","Articular","Apoyar","Apoya","Apoyo","Atender","Asignar",
            "Brindar","Capacitar","Coadyuvar","Conducir","Cumplir","Comunicar","Contribuir","Controlar","Consolidar",
            "Colaborar","Coordinar","Coordinación","Desarrollar","Desarrollas","Depurar","Determinar","Declarar",
            "Diseñar","Dirigir","Distribuir","Difundir","Documentar","Efectuar","Ejecutar","Evaluar","Elaborar",
            "Elaboración","Emitir","Emisión","Establecer","Escanear","Facilitar","Fomentar","Formular","Fortalecer",
            "Generar","Gestionar","Hacer","Identificación","Interactuar","Identificar","Impulsar","Implementar","Insertar",
            "Integrar","Informar","Ingreso","Iniciar","Jefaturar","Liderar","Mantener","Manejar","Mantenimiento",
            "Mapear","Monitorear","Modelar","Modelación","Organizar","Orientar","Participar","Planificar","Priorizar",
            "Presentación","Programar","Proporcionar","Proponer","Procesar","Procesamiento","Promover","Proyectar",
            "Precalificar","Recibir","Recolectar","Recabar","Recopilar","Realizar","Revisión","Revisar","Representar",
            "Registrar","Registro","Recoger","Redactar","Reportar","Seguimiento","Sistematizar","Sistematización",
            "Supervisar","Supervisión","Solucionar","Tomar","Tramitar","validar","Velar","Verificar","Verificación",
            "Otras Tareas","Otras Asignadas","Otras Actividades","Otras Labores","Departamento",
            "Duración","Meses","Término","Hasta","Mil","Soles","Remuneración","000 00")

KeyMatch<-str_c(KeyWords,collapse = "|")

#Cargamos la base de datos que contiene el link de los PDFs

getwd()
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINAGRI")

PdfLinks<-read_rds("LinkPdfB_311.rds")
names(PdfLinks)<-"PDF" #Ponemos un nombre a la columna
sapply(PdfLinks, class)
#Convertimos a cadena de texto los elementos del dataframe
PdfLinks$PDF<-as.character(PdfLinks$PDF)
sapply(PdfLinks, class)

PdfLinks$PDF[116] #Entramos al link de cada pdf


#Leemos

ReadPDF_MINAGRI<-pdf_ocr_text(PdfLinks$PDF[4],language = "spa",dpi = 600) #No le indiqué el N° de páginas, leerá todo max=3

PdfM4<-ReadPDF_MINAGRI%>%
  str_split(pattern = "\n")%>%
  unlist()%>%
  str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio blanco
  str_replace_all("  "," ")%>%
  str_to_title(locale = "sp")%>% #converte en forma de título en español
  str_trim()%>%
  str_subset(pattern =KeyMatch,negate = F)%>%
  str_subset(pattern = "Corrupción Y La Impunidad",negate =T )%>%
  str_subset(pattern = "Decreto Supremo",negate = T)%>%
  str_subset(pattern = "Vinculados",negate = T)%>%
  str_subset(pattern = "Trámite Documentario",negate = T)%>%
  str_subset(pattern = "Convocatoria",negate = T)%>%
  str_subset(pattern = "Directiva N",negate = T)%>%
  str_subset(pattern = "Dependencia",negate = T)%>%
  str_subset(pattern = "Comité De Selección",negate = T)%>%
  str_subset(pattern = "Disposiciones",negate = T)%>%
  str_subset(pattern = "Normativa",negate = T)%>%
  #str_subset(pattern = "Cobit",negate = T)%>%
  str_subset(pattern = "Conmemorativos",negate = T)%>%
  str_subset(pattern = "Respecto De Los Procesos",negate = T)%>%
  str_subset(pattern = "Programado",negate = T)

#Convierte en data estructurada
PdfM4<-PdfM4%>%
  matrix(ncol = length(PdfM4),byrow = T)%>%
  as.data.frame()

DataMinagriB<-bind_rows(PdfM1,PdfM2,PdfM3,PdfM4)
rm(PDFlinks)
#....... Para covertir primero en imagen y luego leerlo

readimage<-pdf_convert(PdfLinks$PDF[111],dpi = 600)
readimage<-tesseract::ocr(readimage,engine ="spa")
readimageC<-readimage%>%
 str_split(pattern = "\n")

cat(readimage)
# No se leó la 111, 175, 192,209,210,216,217, 256 no utilizado  dice,

#---- Hacemos un for #----
for(j in 301:311){
  Namepdf<-paste0("MinagriPdf_",j) # Generamos un nombre para cada convocatoria 
  
  Namepdf<-pdf_ocr_text(PdfLinks$PDF[j],language = "spa")%>%
    str_split(pattern = "\n")%>%
    unlist()%>%
    str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio blanco
    str_replace_all("  "," ")%>%
    str_to_title(locale = "sp")%>% #converte en forma de título en español
    str_trim()%>%
    str_subset(pattern =KeyMatch,negate = F)%>%
    str_subset(pattern = "Corrupción Y La Impunidad",negate =T )%>%
    str_subset(pattern = "Decreto Supremo",negate = T)%>%
    str_subset(pattern = "Vinculados",negate = T)%>%
    str_subset(pattern = "Trámite Documentario",negate = T)%>%
    str_subset(pattern = "Convocatoria",negate = T)%>%
    str_subset(pattern = "Directiva N",negate = T)%>%
    str_subset(pattern = "Dependencia",negate = T)%>%
    str_subset(pattern = "Comité De Selección",negate = T)%>%
    str_subset(pattern = "Disposiciones",negate = T)%>%
    str_subset(pattern = "Normativa",negate = T)%>%
    #str_subset(pattern = "Cobit",negate = T)%>%
    str_subset(pattern = "Conmemorativos",negate = T)%>%
    str_subset(pattern = "Respecto De Los Procesos",negate = T)%>%
    str_subset(pattern = "Programado",negate = T)
  
  #Convierte en data estructurada
  Namepdf<-Namepdf%>%
    matrix(ncol = length(Namepdf),byrow = T)%>%
    as.data.frame()
  
  saveRDS(Namepdf,paste0("PDFMinagri_",j,".rds"))
  #Para no confundir con un bot
  Sys.sleep(1)
}

# No se leó la 111, 175, 192,209,210,216,217, 256 no utilizado  dice, 
list.files() # vemos todos los archivos de nuestra carpeta MINAGRI
Ficheros <- list.files(pattern = "^PDFMinagri") # Extrae solo los que inician con PdfLink
DPDF<-read_rds(Ficheros[1]) #Observamos hoja por hoja hasta 96

for (j in 1:300) {
  DataMinagriB<-bind_rows(DataMinagriB,readRDS(Ficheros[j]))
}

#Guardamos la data total

saveRDS(DataMinagriB,"DataPdfMNGB_304.rds")

DATAMINGRI<-read_rds("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINAGRI/DataPdfMNGB_304.rds")
