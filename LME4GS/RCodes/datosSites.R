#####################################################################
# Title: Read data
# Name: Francisco Manuel Rodriguez Huerta
# Date: June 20 2015
# Description: choose sites to analyze
#####################################################################

DatosSites <- function(datos,Site,sitiosAnalizar){
	idsites <- datos[,Site]
	datos2 <- datos[idsites==sitiosAnalizar[1],]
	if(length(sitiosAnalizar)>1){
		for(i in 2:length(sitiosAnalizar)){
			datos2 <- rbind(datos2,datos[idsites==sitiosAnalizar[i],])	
		}
	}
	return(datos2)
}

