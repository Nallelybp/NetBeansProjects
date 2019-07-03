#Utility routines
#NOTE: This routine will give the list only with monomorphic markers with missing values,
#if you have monomorphic markers without missing values you have to remove it manually

Impute <- function(X){
	#dimes <- dim(X)
	monomorphic=numeric()
	for(i in 1:ncol(X)){
   		cat('Imputing Marker ',i,'\n')
      	if(length(as.numeric(table(X[,i])))==1){
        		monomorphic=c(monomorphic,i)
      	}else{
        		tmp=table(X[,i])
        		x=as.numeric(names(tmp))
        		X[which(is.na(X[,i])),i]=sample(x=x,size=sum(is.na(X[,i])),replace=TRUE,prob=tmp/sum(tmp))
     		}
	}
	MonoOMaf <- 0

	if(length(monomorphic)==0){
		messageMono <- 'No monomorfic markers'
		MonoYes <- FALSE
	}else{
		MonoOMaf <- 1
		MonoYes <- TRUE
		if(!InfoMarkersFoder){
			if(!file.exists('Info_Markers')) dir.create('Info_Markers')
			
			path <- paste(path,'Info_Markers',sep='/')
			setwd(normalizePath(path))
		}
		write.table(colnames(X)[monomorphic],'Monomorfic_Markers.csv',sep=',',row.names=FALSE,col.names=FALSE)
		X <- X[,-monomorphic]
		#dimes <- rbind(dimes,dim(X))
		#write.table(dimes,'Dim_Markers.csv',sep=',',row.names=FALSE,col.names=FALSE)
		messageMono <- paste(length(monomorphic),'monomorfic markers were removed, and you can see them on file:\n     Monomorfic_Markers.csv',sep=' ')
	}

	phat <- colMeans(X)/2
	MAF <- ifelse(phat<0.5,phat,1-phat)
	MAFlow <- which(MAF<0.05)
	MAFBool <- length(MAFlow)>0

	if(MAFBool){
		MonoOMaf <- 1
		if(!MonoYes || !InfoMarkersFoder){
			if(!file.exists('Info_Markers')) dir.create('Info_Markers')
			path <- paste(path,'Info_Markers',sep='/')
			setwd(normalizePath(path))
		}
		messageMono <- paste(messageMono,paste(paste("\n\n",length(MAFlow),sep=""),'markers with MAF lower than 0.05 were removed, and you can see them on file:\n     MAF_Markers.csv',sep=' '))
  		write.table(colnames(X)[MAFlow],'MAF_Markers.csv',sep=',',row.names=FALSE,col.names=FALSE)
		X <- X[,-MAFlow]
		#dimes <- rbind(dimes,dim(X))
		#write.table(dimes,'Dim_Markers.csv',sep=',',row.names=FALSE,col.names=FALSE)
		pdf('MAF.pdf')
			hist(MAF,main='MAF Histogram')
		dev.off()
	}else{
		messageMono <- paste(messageMono,'\n\nNo markers with MAF lower than 0.05')
	}

	### Agregar MAF
	return(list(X=X,messageMono=messageMono,MonoOMaf=MonoOMaf))

}