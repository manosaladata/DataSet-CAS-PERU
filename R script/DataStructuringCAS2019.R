 # Data structuring

setwd("C:/Users/Jose Luis/Documents/GitHub/DataSet-CAS-PERU/Data")
library(tidyverse)

DataMEF<-read_rds("MEF/DataLimpiaMEF.rds")

sapply(DataMEF, class)
unique(DataMEF$V34)
unique(DataMEF$V35)
sum(is.na(DataMEF$v35))
#
#DataMEF$v3Profesión[DataMEF$v3Profesión==""]<-"NA"

#Hacemos estructuras de control
for (j in 1:455) {
  
  Patron<-str_subset(DataMEF[j,34],"Mil", negate = F)
  
  if (DataMEF[j,35]=="NA") {
    DataMEF[j,35]<-str_extract(DataMEF[j,34], pattern = Patron)
  } else{DataMEF[j,35]<-DataMEF[j,35]}
}

#Otra forma

while(DataMEF[c(1:455),35]=="NA"){
  Patron<-str_subset(DataMEF[j,34],"Mil", negate = F)
  DataMEF[j,35]<-str_extract(DataMEF[j,34], pattern = Patron)
}
