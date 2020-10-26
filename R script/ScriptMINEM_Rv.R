
#Ministerio de energía y minas

cat("\014")     #Limpia la consola
rm(list = ls()) # limpia la memoria de trabajo

#Analizando la página de MINEM

#http://www.minem.gob.pe/lfdata/5311449/BASES-370-2019.pdf
#http://www.minem.gob.pe/lfdata/5331250/BASES-378-2019.pdf
#http://www.minem.gob.pe/lfdata/5308638/BASES-378-2019.pdf
#http://www.minem.gob.pe/lfdata/5297956/BASES-377-2019.pdf
#http://www.minem.gob.pe/_convocatoriaBases.php?idSector=10&idConvocatoria=953&ver=2
#http://www.minem.gob.pe/_convocatoriaBases.php?idSector=10&idConvocatoria=962&ver=2
#http://www.minem.gob.pe/_convocatoriaBases.php?idSector=10&idConvocatoria=953&ver=2
#http://www.minem.gob.pe/_convocatoriaBases.php?idSector=10&idConvocatoria=951&ver=2

#Cargamos la librerías
library(tidyverse)
library(xml2)
library(rvest)
library(robotstxt)
library(pdftools) #Lee PDF
library(tesseract) # Para PDF escaneada

# Creamos un objeto que contenga el link de la web
#---- Escrapear la Página #----

URL1<-"http://www.minem.gob.pe/_detalle.php?idSector=10&idTitular=7174&idMenu=sub294&idCateg=1230" #2019   I

#Preguntar si esta premitio bajar los datos de la  web
paths_allowed(paths = c(URL1)) # debe decir TRUE 
get_robotstxt(URL1)            # otra forma de preguntar
# Asignamos como encondig a UTF-8
#options(encoding = "utf-8")
guess_encoding(URL1) # para ver la codificación
#Bajamos información de la web

Pag1<-read_html(URL1,encoding = "utf-8")#%>% #Con la actualizacíon del rvest, necesita poner el encoding
 
Tabla1<-Pag1%>%
  html_node(css = ".cuerpo")%>%
  html_node("tbody")%>%
  html_text(trim = T)%>%
  str_split("\n")%>%
  unlist()%>%
  str_replace_all("[^ [:alnum:]]"," ")%>%
  str_subset("[:alnum:]",negate = F)%>%
  str_trim()%>%
  str_subset(" 2019", negate = F)%>%
  str_replace_all("2019","2019/")%>% #Para separar 
  str_split("/")%>%
  unlist()%>%
  str_to_title()%>%
  str_subset("Cancelación",negate = T)%>%
  str_subset("Otros",negate = T)%>%
  str_subset("Calendario",negate = T)%>%
  str_replace_all("   "," ")%>%
  matrix(ncol = 2,byrow = T)%>%
  as.data.frame()

#Extraemos los links de los pdfs
LinkPdf<-Pag1%>%
  html_node(css = ".cuerpo")%>%
  html_node("tbody")%>%
  html_nodes("a")%>%
  html_attr("href")%>%
  str_subset("convocatoriaBases",negate = F)

LinkPdf[c(1:37)]

#Adjuntamos a la otra data
Tabla1$V3<-LinkPdf[c(1:37)]
names(Tabla1)<-c("Proceso","Puesto","LinkAccesoPDF")
sapply(Tabla1, class)

#bajamos informacíon de la pagina 2

URL2<-"http://www.minem.gob.pe/_detalle.php?idSector=10&idTitular=7763&idMenu=sub294&idCateg=1347" #2019 II
Pag2<-read_html(URL2,encoding = "utf-8")#%>% #Con la actualizacíon del rvest, necesita poner el encoding

Tabla2<-Pag2%>%
  html_node(css = ".cuerpo")%>%
  html_node("tbody")%>%
  html_text(trim = T)%>%
  str_split("\n")%>%
  unlist()%>%
  str_subset("[:alnum:]",negate = F)%>%
  str_replace_all("[^ [:alnum:]]"," ")%>%
  str_trim()%>%
  str_subset("2019", negate = F)%>%
  str_replace_all("2019","2019/")%>% #Para separar 
  str_split("/")%>%
  unlist()%>%
  str_to_title()%>%
  str_subset("Erratas",negate = T)%>%
  str_subset("Cancelación",negate = T)%>%
  str_subset("Desierto",negate = T)%>%
  str_subset("Otros",negate = T)%>%
  #str_subset("Calendario",negate = T)%>%
  str_replace_all("   "," ")%>%
  matrix(ncol = 2,byrow = T)%>%
  as.data.frame()

#Extraemos los links de los pdfs
LinkPdf2<-Pag2%>%
  html_node(css = ".cuerpo")%>%
  html_node("tbody")%>%
  html_nodes("a")%>%
  html_attr("href")%>%
  str_subset("convocatoriaBases",negate = F)

LinkPdf2[c(1:440)]
#Adjuntamos a la otra data
Tabla2$V3<-LinkPdf2[c(1:440)]
names(Tabla2)<-c("Proceso","Puesto","LinkAccesoPDF")
sapply(Tabla2, class)

#Juntamos las dos tablas

DataMINEMB<-rbind(Tabla2,Tabla1)
sapply(DataMINEMB, class)

#Guardamos la informacíon
getwd()
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINEM")

saveRDS(DataMINEMB,"DataMINEMcLink.rds")

#---- Download los Pdfs #----
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINEM")

UrlMadre<-"http://www.minem.gob.pe/"
BasesMINEM<-read_rds("DataMINEMcLink.rds")
#Parte individual
BasesMINEM$LinkAccesoPDF[2]
paste0(UrlMadre,BasesMINEM$LinkAccesoPDF[1])
#Descargar pdfs con bucle, porque no se pudo leer en la misma página
NamePDF<-"PdfMINAM"
for (j in 1:477) {
  
  download.file(paste0(UrlMadre,BasesMINEM$LinkAccesoPDF[j]),paste0(NamePDF,j,".pdf"),mode = "wb")
  #Para no confundir con un bot
  Sys.sleep(1)
}

#---- Leer los pfs downloaded #----
getwd()
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINEM")
Ficheros<-list.files(pattern = "^MINAM")
Ficheros[29]
#Palabras claves

KeyWords<-c("Proceso Cas","Cas N","Convocatoria N","De Un Una","De Un A","Contratar Los Servicios",
            "Contratación De","Contratar","Experiencia Laboral","Experiencia General","Experiencia Profesional",
            "Experiencia Especifica","Experiencia Específica","Específica","Experiencia De","Experiencia Como",
            "Años","Titulo","Título","Titulado","Titulada","Licenciado","Licenciada","Maestría","Doctorado",
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
            "Hidráulicas","Inversión","Siaf","Seace","Inglés","Desing Thinking","Cambio Climático","Atención",
            "Análisis","Analítico","Adaptabilidad","Calidad Del trabajo","Capacidad","Comunicación",
            "Compromiso","Cooperación","Confidencialidad","Dinamismo","Disciplina","Empatía","Habilidades",
            "Iniciativa","Liderazgo","Orden Y Calidad","Orientación","Organización","Planificación",
            "Proactividad","Proactivo","Pensamiento Estratégico","Resolución De Conflictos",
            "Responsabilidad","Razonamiento","Redacción","Trabajo En Equipo","Trabajar En Equipo",
            "Vocación","Bajo Presión","Síntesis","Funciones","Adaptar","Aperturar","Asegurar","Asistir","Actualizar",
            "Analizar","Acompañar","Administrar","Absolver","Asesorar","Aplicar","Articular","Apoyar",
            "Apoya","Apoyo","Atender","Asignar","Brindar","Capacitar","Cautelar","Coadyuvar","Conducir",
            "Cumplir","Comunicar","Contribuir","Controlar","Consolidar","Colaborar","Coordinar","Coordinación",
            "Clasificar","Creación","Desarrollar","Desarrollas","Depurar","Determinar","Declarar","Definir","Diseñar",
            "Dirigir","Distribuir","Difundir","Digitalizar","Documentar","Editar","Efectuar","Ejecutar",
            "Evaluar","Evaluación","Elaborar","Elaboración","Emitir","Emisión","Establecer","Escanear",
            "Facilitar","Fomentar","Formular","Fortalecer","Fotocopiar","Generar","Gestionar","Hacer",
            "Identificación","Interactuar","Identificar","Impulsar","Implementar","Insertar","Integrar",
            "Informar","Ingreso","Ingresar","Iniciar","Jefaturar","Liderar","Mantener","Manejar","Mantenimiento",
            "Mapear","Medir","Monitorear","Modelar","Modelación","Organizar","Orientar","Participar","Planificar",
            "Preparar","Presentar","Prestar","Primeros Auxilios","Priorizar","Presentación","Producir",
            "Programar","Proporcionar","Proponer","Procesar","Procesamiento","Promover","Proyectar","Precalificar",
            "Publicar","Rendir","Recibir","Recolectar","Recabar","Recomenda","Remitir","Recopilar","Realizar",
            "Revisión","Revisar","Representar","Registrar","Registro","Recoger","Redactar","Reportar","Seguimiento",
            "Separar","Sistematizar","Sistematización","Solicitar","Solucionar","Supervisar","Supervisión","Tomar",
            "Tramitar","validar","Velar","Verificar","Verificación","Otras Tareas","Otras Asignadas","Otras Actividades",
            "Otras Labores","Otras Acciones","Otras Que","Departamento","Término","Termino","Hasta",
            "Mil","Soles","000")

KeyMatch<-str_c(KeyWords,collapse = "|")

ReadPDFMINEM<-pdf_ocr_text(Ficheros[477],pages = c(1:3),language = "spa")
#%>% # entre corchetes lee un determinado pdf
PDFMINEM<-ReadPDFMINEM%>%
  str_split(pattern = "\n")%>%
  unlist()%>%
  str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio
  str_to_title(locale = "en")%>% #convierte en forma de título
  str_trim()%>% # recorta espacios en blanco
str_subset(pattern = KeyMatch,negate = F)%>%
  str_subset(pattern = "Dependencia",negate =T )%>%
  str_subset(pattern = "Secretaría General",negate = T)%>%
  str_subset(pattern = "Encargada",negate = T)%>%
  str_subset(pattern = "Encargado",negate = T)%>%
  str_subset(pattern = "Contexto",negate = T)%>%
  str_subset(pattern = "Trámite Documentario",negate = T)%>%
  str_subset(pattern = "Contabilizará",negate = T)%>%
  str_subset(pattern = "Regionales Y Locales",negate = T)%>%
  str_subset(pattern = "Directiva N",negate = T)%>%
  str_subset(pattern = "Régimen",negate = T)%>%
  str_subset(pattern = "Acreditar Mediante",negate = T)%>%
  str_subset(pattern = "Programas De Formación",negate = T)%>%
  str_subset(pattern = "Contará Cualquier",negate = T)%>%
  str_subset(pattern = "Postulante",negate = T)%>%
  str_subset(pattern = "Comité De Selección",negate = T)%>%
  str_subset(pattern = "Decreto",negate = T)%>%
  str_subset(pattern = "Aquellos Casos",negate = T)%>%
  str_subset(pattern = "Guarde Relación",negate = T)%>%
  str_subset(pattern = "Formativas",negate = T)%>%
  str_subset(pattern = "Nomenclatura",negate = T)%>%
  str_subset(pattern = "Especificados",negate = T)%>%
  str_subset(pattern = "Documentación",negate = T)%>%
  str_subset(pattern = "Rotulado",negate = T)%>%
  str_subset(pattern = "Probatorio",negate = T)%>%
  str_subset(pattern = "Ministerio",negate = T)%>%
  str_subset(pattern = "Cualquier Modalidad",negate = T)%>%
  str_subset(pattern = "Mínimo De",negate = T)%>%
  str_subset(pattern = "Los Programas",negate = T)%>%
  str_subset(pattern = "Determinadas",negate = T)%>%
  str_subset(pattern = "No Menos De",negate = T)%>%
  str_subset(pattern = "Pueden Ser Desde",negate = T)%>%
  str_subset(pattern = "Caso Contrario",negate = T)%>%
  str_subset(pattern = "Fases",negate = T)%>%
  str_subset(pattern = "Entrevista Personal",negate = T)%>%
  str_subset(pattern = "Crear Usuario",negate = T)%>%
  str_subset(pattern = "Requisitos",negate = T)%>%
  str_subset(pattern = "Oficina De Recursos",negate = T)%>%
  str_subset(pattern = "Registro De Contrato",negate = T)%>%
  str_subset(pattern = "Registro Del Contrato",negate = T)%>%
  str_subset(pattern = "Marco De Las Funciones",negate = T)%>%
  str_subset(pattern = "Objetivos Y Metas",negate = T)

#---- Bucle para leer #----
for(j in 351:477){
  Namepdf<-paste0("MinemPdf_",j) # Generamos un nombre para cada convocatoria 
  
  Namepdf<-pdf_ocr_text(Ficheros[j],pages = c(1:3),language = "spa", dpi = 600)%>%
    str_split(pattern = "\n")%>%
    unlist()%>%
    str_replace_all("[^ [: alnum:]]"," ")%>% # reemplaza a todo distinto a alfanumerico por un espacio
    str_to_title(locale = "en")%>% #convierte en forma de título
    str_trim()%>% # recorta espacios en blanco
    str_subset(pattern = KeyMatch,negate = F)%>%
    str_subset(pattern = "Dependencia",negate =T )%>%
    str_subset(pattern = "Secretaría General",negate = T)%>%
    str_subset(pattern = "Encargada",negate = T)%>%
    str_subset(pattern = "Encargado",negate = T)%>%
    str_subset(pattern = "Contexto",negate = T)%>%
    str_subset(pattern = "Trámite Documentario",negate = T)%>%
    str_subset(pattern = "Contabilizará",negate = T)%>%
    str_subset(pattern = "Regionales Y Locales",negate = T)%>%
    str_subset(pattern = "Directiva N",negate = T)%>%
    str_subset(pattern = "Régimen",negate = T)%>%
    str_subset(pattern = "Acreditar Mediante",negate = T)%>%
    str_subset(pattern = "Programas De Formación",negate = T)%>%
    str_subset(pattern = "Contará Cualquier",negate = T)%>%
    str_subset(pattern = "Postulante",negate = T)%>%
    str_subset(pattern = "Comité De Selección",negate = T)%>%
    str_subset(pattern = "Decreto",negate = T)%>%
    str_subset(pattern = "Aquellos Casos",negate = T)%>%
    str_subset(pattern = "Guarde Relación",negate = T)%>%
    str_subset(pattern = "Formativas",negate = T)%>%
    str_subset(pattern = "Nomenclatura",negate = T)%>%
    str_subset(pattern = "Especificados",negate = T)%>%
    str_subset(pattern = "Documentación",negate = T)%>%
    str_subset(pattern = "Rotulado",negate = T)%>%
    str_subset(pattern = "Probatorio",negate = T)%>%
    str_subset(pattern = "Ministerio",negate = T)%>%
    str_subset(pattern = "Cualquier Modalidad",negate = T)%>%
    str_subset(pattern = "Mínimo De",negate = T)%>%
    str_subset(pattern = "Los Programas",negate = T)%>%
    str_subset(pattern = "Determinadas",negate = T)%>%
    str_subset(pattern = "No Menos De",negate = T)%>%
    str_subset(pattern = "Pueden Ser Desde",negate = T)%>%
    str_subset(pattern = "Caso Contrario",negate = T)%>%
    str_subset(pattern = "Fases",negate = T)%>%
    str_subset(pattern = "Entrevista Personal",negate = T)%>%
    str_subset(pattern = "Crear Usuario",negate = T)%>%
    str_subset(pattern = "Requisitos",negate = T)%>%
    str_subset(pattern = "Oficina De Recursos",negate = T)%>%
    str_subset(pattern = "Registro De Contrato",negate = T)%>%
    str_subset(pattern = "Registro Del Contrato",negate = T)%>%
    str_subset(pattern = "Marco De Las Funciones",negate = T)%>%
    str_subset(pattern = "Objetivos Y Metas",negate = T)
  
  #Convierte en data estructurada
  Namepdf<-Namepdf%>%
    matrix(ncol = length(Namepdf),byrow = T)%>%
    as.data.frame()
  
  saveRDS(Namepdf,paste0("PdfMinem_",j,".rds"))
  #Para no confundir con un bot
  Sys.sleep(1)
}
  
#----Juntar la data Extraida #----
setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data/MINEM")
Ficheros<-list.files(pattern = "^PdfMinem")
Ficheros[29]

DataMinemPdf<-read_rds(Ficheros[1])

for (j in 2:477) {
  DataMinemPdf<-bind_rows(DataMinemPdf,readRDS(Ficheros[j]))
}

#Guardamos la data total

saveRDS(DataMinemPdf,"DataMinemPdf_477.rds")
unique(DataMinemPdf$V1)
sapply(DataMinemPdf,class)

#http://sdv.midis.gob.pe/sis_rrhh/externo/portal/convocatoriasportal.aspx/Anexo%20N%C2%B0%2005%20-%20BASES%20CAS%20VF.pdf
  
  

