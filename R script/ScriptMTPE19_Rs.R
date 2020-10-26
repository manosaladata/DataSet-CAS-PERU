
#MINISTERIO DEL TRABAJO Y PROMOCIÓN DEL EMPLEO
	

rm(list = ls()) # limpia la memoria de trabajo
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
library(robotstxt)
library(binman)
library(tm) # text mining
library(NLP)
library(pdftools)
library(tesseract)
library(magick)


URL<-"http://69.10.39.53/SISCAS/externo/portal/ConvocatoriasPortal.aspx" # Defrente al Iframe vamos

#Preguntar si esta premitio bajar los datos de la  web

#paths_allowed(paths = c(URL)) # 
get_robotstxt(URL) # otra forma de preguntar

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

server<-phantomjs(port=5011L)
#Abrimos el navegador
Browser <- remoteDriver(browserName = "phantomjs", port=5011L)
Browser$open()

#Navegar la página web que guardamos
Browser$navigate(URL)
Browser$screenshot(display=TRUE) #Muéstrame en foto de la página

# No hay boton de alerta, por lo tanto,
# Eligimos los años

NodoYears<-Browser$findElement(using = 'xpath',
                               value='//*[@id="ddlanio"]')
Year<-NodoYears$selectTag()
Year$value[7] # año 2019
#Years<-NodoYears$getElementText()


# Introducimos el año que queremos

txtYear<- Browser$findElement(using = 'css', "#ddlanio")
txtYear$clickElement()
txtYear$sendKeysToElement(list(Year$value[7])) # le dije el año 2019
Browser$screenshot(display = TRUE)

# Eligimos los meses

NodoMonths<-Browser$findElement(using = 'xpath',
                                value='//*[@id="ddlmes"]')
Meses<-NodoMonths$selectTag()
Meses$text[1] # Me da el mes que elijo

#Ver previamente en que meses hacer click y buscar información
#Meses: Febrero(2Hojas),Abril(4), Mayo(7), junio(3), Julio(10),Agosto(10),
#Setiembre(10),Octubre(10),Noviembre(9) y Diciembre(2)

#Nos ingeniamos para buscar solo lo que queremos, para el bucle
Mesclick<-c(2,4,5:12) # Creamos el numero que corresponde a los meses
Mesclick<-as.list(Mesclick) # convertimos a lista para indexar
Meses$text[Mesclick[[1]]] # Probamos la indexación
length(Mesclick) # Para saber cuántas veces indexar el mes

#Introducimos el mes 
txtMes<- Browser$findElement(using = 'css', "#ddlmes")
txtMes$clickElement()
txtMes$sendKeysToElement(list(Meses$text[Mesclick[[1]]])) # le dije el mes que está indexada
Browser$screenshot(display = TRUE)

# Hacer clic en Buscar y ver cuántas hojas tiene cada mes

Buscar<- Browser$findElement(using = 'xpath',
                             value = "//input[@id='btnbuscar']")
Buscar$clickElement()
Browser$screenshot(display = TRUE)

#Hacer clic en siguiente y anterior, es indistinto, pero es lógico, inicia con siguente

Siguiente<-Browser$findElement(using = "xpath",
                               value = "//*[@id='PaginadoControl1']")
Siguiente$clickElement()
Browser$screenshot(display = TRUE)

# Hacer clic en anterior

##Anterior<-browser$findElement(using = "xpath",
##                             value = "//input[@id='ctl00_cphBodyMain_reserva1_btnanterior']")
##Anterior$clickElement()
##browser$screenshot(display = TRUE)


#----Parte Rvest individual #----
# Ahora podemos bajar información con rvest sobre la web actual

Pagina_actual<-Browser$getPageSource()

# Extraemos sólo el texto de la hoja N° 01

Hoja1<-read_html(Pagina_actual[[1]])%>% # el elemento 1 de la lista esta la url de la página actual
  html_nodes(css = ".etiketa")%>%
  html_text()%>%
  str_remove("AÑO")%>%
  str_remove("MES")%>%
  str_remove_all("Bases")%>%
  str_remove_all("Anexos")%>%
  str_remove_all("Resultado Final")%>%
  str_remove_all("Resultado de Evaluación Curricular")%>%
  str_subset("[:alnum:]")%>%# Extrea sólo los afanúmericos, sin los saltos
  str_replace_all("\n","")%>%
  str_trim()

#Meses: Febrero(2Hojas),Abril(4), Mayo(7), junio(3), Julio(10),Agosto(10),
#Setiembre(10),Octubre(10),Noviembre(9) y Diciembre(2)

Hojas<-c(2,4,7,3,10,10,10,10,9,2)
Hojas<-as.list(Hojas) # Servirá para el bucle que extraiga información de las hojas de cada mes

# Extraemos los link de los pdf para leerlos (Hoja 1)
#CAS%20008-2019%20-%20SECRETARIA%20REGIONAL%20-%20CAJAMARCA.pdf

#PDF DE REQUISITOS

Hoja1_linkPdf<-read_html(Pagina_actual[[1]])%>%
  html_nodes(".etiketa")%>%
  html_nodes("input")%>%
  html_attr("value")%>%
  str_subset("[:alnum:]")%>%
  str_trim()

Hoja1_linkPdf[1]
# No podemos acceder a los pdfs desde R, ¿?
#De aquí para adelante ya no funciona


UrlMadrePdf<-"http://sdv.midis.gob.pe/sis_rrhh/externo/portal/convocatoriasportal.aspx/"

ReadPDF_MIDIS<-pdf_ocr_text(paste0(UrlMadrePdf,Hoja1_linkPdf[1]),pages = c(1:2),language = "spa")

#Pagina_actual<-Browser$getPageSource() #obtener de la página actual

# Nos quedamos aquí
# siempre cerrar la sesión

Browser$close()
server$stop()