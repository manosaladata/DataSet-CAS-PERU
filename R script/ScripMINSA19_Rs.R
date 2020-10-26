
#Pagina intersante de datos macroeconómicos 
#https://www.cesla.com/base-datos-estadisticas-economia.php


# MINSITERIO DE SALUD

#Librerías
#getCRANmirrors() #para saber la lista de mirros de servidores para instalar pkgs
packageDescription("pdftools") #Para conocer el paquete un poco más
library(tidyverse) #instala además 8 paquetes adicionales
#library(readr)
#library(stringr)    # manejador de string (objeto, patron)
#library(dplyr)
#library(tidyr)
#library(purrr)
#library(ggplot2)
#library(forcats)
#library(tibble)

library(xml2)
library(rvest)
library(RSelenium)  #escrapea paginas dinámicas
library(wdman) # Navegación fantasma para rselenium
library(tm) # text mining
library(NLP)
library(pdftools)
library(tesseract)
library(magick)
library(robotstxt)
library(binman)

URL<-"https://www.minsa.gob.pe/transparencia/index.asp?op=514"
      

#Preguntar si esta premitido bajar los datos de la  web

paths_allowed(paths = c(URL)) # 
#get_robotstxt(URL) # otra forma de preguntar

#

#acceptAlert()
#Acepta el cuadro de diálogo de alerta que se muestra actualmente
#equivale a hacer clic el botón "Aceptar" en el cuadro de diálogo

#dismissAlert() #Descarta el cuadro de diálogo de alerta que se muestra actualmente en la página
#Para los cuadros de diálogo confirmar () y preguntar (),esto equivale a hacer clic en el botón "Cancelar"
#Para los cuadros de diálogo alert (), esto es equivalente hacer clic en el botón "Aceptar"

#----Parte Rselenium #----
# Asignamos como encondig a UTF-8

options(encoding = "utf-8")

#Abrimos una sesion en la web

# Ejecutamos el servidor phantomjs -creamos un navegador fantasma

server<-phantomjs(port=5012L)
#Abrimos el navegador
Browser <- remoteDriver(browserName = "phantomjs", port=5012L)
Browser$open()

#Navegar la página web que guardamos
Browser$navigate(URL)
#browser$acceptAlert()
Browser$screenshot(display=TRUE)

#No hay Botón de aleta
#BotonAlerta<-browser$findElement(using = 'css',
 #                                value='.btn-danger')
#BotonAlerta$clickElement()
#Browser$screenshot(display=TRUE) # si salío 

# Eligimos los años

NodoYears<-Browser$findElement(using = 'xpath',
                               value='//*[@id="ano"]')
Years<-NodoYears$selectTag()

Years$value[2] # año 2019

# Introducimos el año que queremos

txtYear<- Browser$findElement(using = 'css', "#ano")
txtYear$clickElement()
txtYear$sendKeysToElement(list(Years$value[2])) # le dije el año 2019
Browser$screenshot(display = TRUE)


# Eligimos los meses

NodoMonths<-Browser$findElement(using = 'xpath',
                                value='//*[@id="mes"]')
Meses<-NodoMonths$selectTag()
Meses$text[1] # Me da el mes que elijo

#ver previmente qué meses contiene CAS
#Febrero(1),marzo(1), abril(1),mayo(1),junio(1),julio(1),agosto(1),setiembre(1)
#octubre(1),noviembre(1) y diciembre(1)

#Nos ingeniamos para buscar sólo los meses que queremos, para el bucle
Mesclick<-c(2:12) # Creamos el numero que corresponde a los meses

Meses$text[Mesclick[11]] # Probamos la indexación bucle de 1:11
length(Mesclick) # Para saber cuántas veces indexar el mes en el bucle

#----Extracción de información #----

#Introducimos el mes para extraer información

txtMes<- Browser$findElement(using = 'css', "#mes")
txtMes$clickElement()
txtMes$sendKeysToElement(list(Meses$text[Mesclick[11]])) # le dije el mes que está indexada
Browser$screenshot(display = TRUE)

# Ahora podemos bajar información con rvest sobre la página web actual

Pagina_actual<-Browser$getPageSource()

# Ya logré extraer información de un iframe con ese link
#https://steakrecords.com/es/739102-scraping-table-within-iframe-using-r-rvest-library-r-iframe-web-scraping.html

library(magrittr) # para hacer el extract

Pagina<-read_html(Pagina_actual[[1]]) # en el elemento 1 de la lista está la url de la página actual

Hoja11<-Pagina%>%
  html_nodes("iframe")%>%
  extract(1)%>%
  html_attr("src")%>%
  read_html()%>%
  html_nodes(css = ".text_descripcion")%>%
  html_text()

#Creamos vectores para indexar

Pares = c()      # Se crea un vector vacío
Impares = c()

for (j in 1:156) { # Se van a procesar los números de 1 a n
    if(j%%2==0) Pares<-c(Pares,j)    # Si al dividir por 2 sale 0
    else Impares<-c(Impares,j)
}

data.frame(Proceso=html_text(html_nodes(read_html(html_attr(extract(html_nodes(Pagina,"iframe")[1]),"src")),css = ".text_titular")),
           Dependencia=html_text(html_nodes(read_html(html_attr(extract(html_nodes(Pagina,"iframe")[1]),"src")),css = ".text_descripcion")[Impares]),
           Puesto=html_text(html_nodes(read_html(html_attr(extract(html_nodes(Pagina,"iframe")[1]),"src")),css = ".text_descripcion")[Pares]),
           stringsAsFactors = FALSE)->DataHoja11

#Juntamos las bases de datos que hemos generado individualmente

DataMinsaB<-rbind(DataHoja1,DataHoja2,DataHoja3,DataHoja4,DataHoja5,DataHoja6,
                  DataHoja7,DataHoja8,DataHoja9,DataHoja10,DataHoja11)
#Elimiamos las datas compiladas
rm(DataHoja1,DataHoja2,DataHoja3,DataHoja4,DataHoja5,DataHoja6,
   DataHoja7,DataHoja8,DataHoja9,DataHoja10,DataHoja11)
#Guardamos la data total bruta
saveRDS(DataMinsaB,"C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINSA/DataMinsaB_394.rds")

#Eliminamos los demás listas
rm(Pares,Impares,Hoja1,Hoja2,Hoja3,Hoja4,Hoja5,Hoja6,Hoja7,Hoja8,Hoja9,Hoja10,Hoja11,j)

#----For para bajar links de Pdfs #----
#Bueno esto se ubiera añadido en el anterior dataframe

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINSA")
getwd()
for (j in 1:11) {
  txtMes<- Browser$findElement(using = 'css', "#mes")
  txtMes$clickElement()
  txtMes$sendKeysToElement(list(Meses$text[Mesclick[j]])) # le dije el mes que está indexada
  #Le indicamos que opere sobre la página actual
  Pagina_actual<-Browser$getPageSource()
  
  NamePdf<-read_html(Pagina_actual[[1]])%>% # en el elemento 1 de la lista está la url de la página actual
  html_nodes("iframe")%>%
    extract(1)%>%
    html_attr("src")%>%
    read_html()%>%
    html_nodes("a")%>%
    html_attr("href")%>%
    str_subset("\\ConvoCas",negate = F)
  
  NamePdf<-matrix(NamePdf,ncol = 1)%>%
    as.data.frame()
  
  saveRDS(NamePdf,paste0("PdfConvo",j,".rds"))
   }
#Juntamos la data de los linkpdf

list.files() # vemos todos los archivos de nuestra carpeta MINSA
Ficheros <- list.files(pattern = "^PdfConvo") # Extrae solo los que inician con PdfLink
PdfLink<-read_rds(Ficheros[1]) #Observamos hoja por hoja hasta 11

DataPdfLinksMINSA<-PdfLink
for (j in 2:11) {
  DataPdfLinksMINSA<-rbind(DataPdfLinksMINSA,readRDS(Ficheros[j]))
}

#Guardamos la data de los links de pdfs
saveRDS(DataPdfLinksMINSA,"DataPdfLink_318.rds")

#---- Leer los PDFs #----

UrlmadrePdf<-"https://www.minsa.gob.pe/convocatorias/"
             #https://www.minsa.gob.pe/convocatorias/raiz/2020/Octubre/ConvoCas00010-DGOS_20201016.pdf
             #https://www.minsa.gob.pe/convocatorias/raiz/2019/Febrero/ConvoCas001-DM_20190225.pdf
   

KeyWords<-c("Proceso Cas","Cas N","Contratar Los Servicios","Contratación De","Contratar A",
            "Experiencia Laboral","Experiencia General","Experiencia Profesional",
            "Experiencia Especifica","Experiencia Específica","Experiencia De","Experiencia Como",
            "Años","Título","Titulado","Titulada","Licenciado","Licenciada","Maestría","Doctorado",
            "Bachiller","Egresado","Egresada","Profesional","Diplomado","Técnica","Técnico",
            "Estudios Técnicos","Estudios Universitarios","Secundaria","Secundarios","Ingeniería",
            "Ingeniero","Ingeniera","Ingenieria","Economía","Economia","Economista","Contador",
            "Contabilidad","Abogado","Ing","Médico","Cirujano","Químico","Periodismo","Superior",
            "Carreras","Secretariado","Capacitación","Certificación","Constancia","Conocimientos",
            "Conocimiento","Computación","Cursos","Curso","Especialización","Planeamiento","Gestión",
            "Cultura Organizacional","Desarrollo Organizacional","Contrataciones","Sistema","Manejo",
            "Office","Ofimática","Procesador","Hojas de Cálculo","Presupuesto","Presupuestos",
            "Presupuestales","Arc Gis","Modernización","Perfiles De Puesto","AutoCad","Formalización",
            "Derecho Laboral","Derecho Administrativo","Seguridad Previsional","Online","Recursos Hídricos",
            "Hidráulicas","Inversión","Siaf","Inglés","Desing Thinking","Cambio Climático","Atención",
            "Análisis","Analítico","Adaptabilidad","Calidad Del trabajo","Capacidad","Comunicación",
            "Compromiso","Cooperación","Confidencialidad","Dinamismo","Disciplina","Empatía","Habilidades",
            "Iniciativa","Liderazgo","Orden Y Calidad","Orientación","Organización","Planificación",
            "Proactividad","Proactivo","Pensamiento Estratégico","Resolución De Conflictos",
            "Responsabilidad","Razonamiento","Redacción","Trabajo En Equipo","Trabajar En Equipo",
            "Vocación","Bajo Presión","Síntesis","Funciones","Aperturar","Asegurar","Asistir","Actualizar",
            "Analizar","Acompañar","Administrar","Absolver","Asesorar","Aplicar","Articular","Apoyar",
            "Apoya","Apoyo","Atender","Asignar","Brindar","Capacitar","Cautelar","Coadyuvar","Conducir",
            "Cumplir","Comunicar","Contribuir","Controlar","Consolidar","Colaborar","Coordinar","Coordinación",
            "Clasificar","Creación","Desarrollar","Desarrollas","Depurar","Determinar","Declarar","Diseñar",
            "Dirigir","Distribuir","Difundir","Digitalizar","Documentar","Editar","Efectuar","Ejecutar",
            "Evaluar","Evaluación","Elaborar","Elaboración","Emitir","Emisión","Establecer","Escanear",
            "Facilitar","Fomentar","Formular","Fortalecer","Fotocopiar","Generar","Gestionar","Hacer",
            "Identificación","Interactuar","Identificar","Impulsar","Implementar","Insertar","Integrar",
            "Informar","Ingreso","Iniciar","Jefaturar","Liderar","Mantener","Manejar","Mantenimiento",
            "Mapear","Monitorear","Modelar","Modelación","Organizar","Orientar","Participar","Planificar",
            "Preparar","Presentar","Prestar","Primeros Auxilios","Priorizar","Presentación","Producir",
            "Programar","Proporcionar","Proponer","Procesar","Procesamiento","Promover","Proyectar","Precalificar",
            "Recibir","Recolectar","Recabar","Recomenda","Remitir","Recopilar","Realizar","Revisión","Revisar",
            "Representar","Registrar","Registro","Recoger","Redactar","Reportar","Seguimiento","Sistematizar",
            "Sistematización","Solicitar","Solucionar","Supervisar","Supervisión","Tomar","Tramitar","validar",
            "Velar","Verificar","Verificación","Otras Tareas","Otras Asignadas","Otras Actividades","Otras Labores",
            "Otras Acciones","Otras Que","Departamento","Duración","Meses","Término","Hasta","Mil","Soles","000")

KeyMatch<-str_c(KeyWords,collapse = "|")

#Cargamos la base de datos que contiene el link de los PDFs

getwd()
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINSA/")

PdfLinks<-read_rds("DataPdfLink_318.rds")
names(PdfLinks)<-"PDF" #Ponemos un nombre a la columna
sapply(PdfLinks, class)
PdfLinks$PDF[1]
#Convertimos a cadena de texto los elementos del dataframe
PdfLinks$PDF<-as.character(PdfLinks$PDF)
sapply(PdfLinks, class)

PdfLinks$PDF[1] #Entramos al link de cada pdf

#Leemos

ReadPDF_MINSA<-pdf_ocr_text(paste0(UrlmadrePdf,PdfLinks$PDF[318]),pages = c(1:2),language = "spa",dpi = 600) #

PdfM1<-ReadPDF_MINSA%>%
  str_split(pattern = "\n")%>%
  unlist()%>%
  str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio blanco
  str_replace_all("  "," ")%>%
  str_to_title(locale = "sp")%>% #converte en forma de título en español
  str_trim()%>%
  str_subset(pattern =KeyMatch,negate = F)%>%
  str_subset(pattern = "Dependencia",negate =T )%>%
  str_subset(pattern = "Decreto Supremo",negate = T)%>%
  str_subset(pattern = "Oficina General",negate = T)%>%
  str_subset(pattern = "Trámite Documentario",negate = T)%>%
  str_subset(pattern = "De Procedimiento Administrativo",negate = T)%>%
  str_subset(pattern = "Directiva Administrativa",negate = T)%>%
  str_subset(pattern = "Pioceuimiento Acminisitaiivo",negate = T)%>%
  str_subset(pattern = "Comité De Selección",negate = T)%>%
  str_subset(pattern = "Decreto",negate = T)%>%
  str_subset(pattern = "Vacantes",negate = T)%>%
  str_subset(pattern = "Acantes",negate = T)%>%
  str_subset(pattern = "Ofertas",negate = T)%>%
  str_subset(pattern = "Documentación",negate = T)%>%
  str_subset(pattern = "Rotulado",negate = T)%>%
  str_subset(pattern = "Desastres Y Defensa",negate = T)%>%
  str_subset(pattern = "Ministerio",negate = T)

#str_subset(pattern = "Desastres Y Defensa",negate = T)


#Convierte en data estructurada
PdfM4<-PdfM4%>%
  matrix(ncol = length(PdfM4),byrow = T)%>%
  as.data.frame()

#DataMinagriB<-bind_rows(PdfM1,PdfM2,PdfM3,PdfM4)

#....... Para covertir primero en imagen y luego leerlo

readimage<-pdf_convert(PdfLinks$PDF[111],dpi = 600)
readimage<-tesseract::ocr(readimage,engine ="spa")
readimageC<-readimage%>%
  str_split(pattern = "\n")

cat(readimage)


#---- Hacemos un for #----
for(j in 233:318){
  Namepdf<-paste0("MinsaPdf_",j) # Generamos un nombre para cada convocatoria 
  
  Namepdf<-pdf_ocr_text(paste0(UrlmadrePdf,PdfLinks$PDF[j]),pages = c(1:2),language = "spa", dpi = 600)%>%
    str_split(pattern = "\n")%>%
    unlist()%>%
    str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio blanco
    str_replace_all("  "," ")%>%
    str_to_title(locale = "sp")%>% #converte en forma de título en español
    str_trim()%>%
    str_subset(pattern =KeyMatch,negate = F)%>%
    str_subset(pattern = "Dependencia",negate =T )%>%
    str_subset(pattern = "Decreto Supremo",negate = T)%>%
    str_subset(pattern = "Oficina General",negate = T)%>%
    str_subset(pattern = "Trámite Documentario",negate = T)%>%
    str_subset(pattern = "De Procedimiento Administrativo",negate = T)%>%
    str_subset(pattern = "Directiva Administrativa",negate = T)%>%
    str_subset(pattern = "Pioceuimiento Acminisitaiivo",negate = T)%>%
    str_subset(pattern = "Comité De Selección",negate = T)%>%
    str_subset(pattern = "Decreto",negate = T)%>%
    str_subset(pattern = "Vacantes",negate = T)%>%
    str_subset(pattern = "Acantes",negate = T)%>%
    str_subset(pattern = "Ofertas",negate = T)%>%
    str_subset(pattern = "Documentación",negate = T)%>%
    str_subset(pattern = "Rotulado",negate = T)%>%
    str_subset(pattern = "Desastres Y Defensa",negate = T)%>%
    str_subset(pattern = "Ministerio",negate = T)
  
  #Convierte en data estructurada
  Namepdf<-Namepdf%>%
    matrix(ncol = length(Namepdf),byrow = T)%>%
    as.data.frame()
  
  saveRDS(Namepdf,paste0("PDFMinsa_",j,".rds"))
  #Para no confundir con un bot
  Sys.sleep(1)
}

#cerrar la sesión
Browser$close()
server$stop()

#Juntar la infromación descargada
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINSA")
list.files() # vemos todos los archivos de nuestra carpeta MINSA
Ficheros <- list.files(pattern = "^PDFMinsa") # Extrae solo los que inician con PDFMinsa
PdfInfor<-read_rds(Ficheros[1]) #Observamos hoja por hoja hasta 318

DataPdfInfoMINSA<-PdfInfor
for (j in 2:318) {
  DataPdfInfoMINSA<-bind_rows(DataPdfInfoMINSA,readRDS(Ficheros[j]))
}

#Hay duplicados: contar
nrow(DataPdfInfoMINSA[duplicated(DataPdfInfoMINSA), ])

DataDupli<-DataPdfInfoMINSA[duplicated(DataPdfInfoMINSA), ]
#Otra forma de eliminar datos es con distinct de dplyr
#distinct(datos)

#Nos quedamos con datos independientes

DataPdfInfoMINSA<-DataPdfInfoMINSA[!duplicated(DataPdfInfoMINSA), ]

#Guardamos la data

saveRDS(DataPdfInfoMINSA,"DataPdfInfoMINSA.rds")

