#####################################################################
# Title: Read data
# Name: Francisco Manuel Rodriguez Huerta
# Date: June 20 2015
# Description: read datos
#####################################################################

ReadData <- function(archivo){
	library(xlsx)
	strarchivo <- strsplit(archivo,split="[.]")[[1]]
	if(strarchivo[length(strarchivo)]=="csv" || strarchivo[length(strarchivo)]=="CSV"){
		datos<-read.csv(file=archivo, header=TRUE, na.strings=c("NA",".","-"," "), as.is=TRUE, check.names = FALSE)
	}else if(strarchivo[length(strarchivo)]=="txt"){
		datos<-read.table(file=archivo, sep="\t", header=TRUE, na.strings=c("NA",".","-"," "), as.is=TRUE, check.names = FALSE)
	}
	else{
		
		datos<-read.xlsx(file=archivo,1, header=TRUE, check.names = FALSE)
		datos[datos=="-"] <- NA
		datos[datos=="."] <- NA
		datos[datos==" "] <- NA
	}
	return(datos)
}

