CreaETA <- function(CVboolean=FALSE){
	savename <- paste(yvar,"with_",sep="_")
	ETAMarker <- NA
	ETAPedigree <- NA
	ETARandom <- NA
	ETAFixed <- NA
	ExitMark <- ""
	if(MarkerBox=="true"){
		if(BGLRMarker=="Ridge"){
			#Z <- scale(Markers[levels(as.factor(datos[,Gen])),],center=TRUE,scale=TRUE)
			#ETAMarker <- list(X=Z,model='BRR')
			ETAMarker <- list(X=Markers[levels(as.factor(datos[,Gen])),],model='BRR')
			ExitMark <- "varB"
		}else if(BGLRMarker=="GBLUP"){
			#Genomic Relationship matrix
			Z <- scale(Markers[levels(as.factor(datos[,Gen])),],center=TRUE,scale=TRUE)
			G=tcrossprod(Z)/ncol(Z)
			#Linear predictor
			#ETAMarker <- list(K=G,model='RKHS')
			if(CVboolean){
				eigenVals <- eigen(G)
				ETAMarker <- list(V=eigenVals$vectors,d=eigenVals$values, model='RKHS')
				rm(eigenVals)
				ExitMark <- "varB"
			}else{
				ETAMarker <- list(K=G,model='RKHS')
				ExitMark <- "varU"
			}		
		}else if(BGLRMarker=="LASSO"){
			ETAMarker <- list(X=Markers[levels(as.factor(datos[,Gen])),],model='BL')
			ExitMark <- "lambda"
		}else if(BGLRMarker=="BayesA"){
			ETAMarker <- list(X=Markers[levels(as.factor(datos[,Gen])),],model='BayesA')
			ExitMark <- "ScaleBayesA"
		}else{ # BGLRMarker=="BayesB"
			ETAMarker <- list(X=Markers[levels(as.factor(datos[,Gen])),],model='BayesB')
			ExitMark <- "parBayesB"
		}
		savename <- paste(savename,paste(paste("Marker",BGLRMarker,sep="-"),"_",sep=""),sep="")
	}
	if(PedigreeBox=="true"){
		ETAPedigree <- list(K=Pedigree[levels(as.factor(datos[,Gen])),levels(as.factor(datos[,Gen]))],model='RKHS')
		savename <- paste(savename,"Pedigree_",sep="")
	}  
	if(RandomBox == "true"){
		formula <- TotalRandoms[1]
		datos[,formula] <- as.factor(datos[,formula])
		formula = paste('model.matrix(~',formula)
		if(length(TotalRandoms)>1){
			for(j in 2:length(TotalRandoms)){
				datos[,TotalRandoms[j]] <- as.factor(datos[,TotalRandoms[j]])
				#fixedEffectstemp <- TotalFixeds[j]
				#formula = paste(formula,TotalRandoms[j],sep='+')
			}
			testFrame <- datos[,TotalRandoms]
			Random <- model.matrix(~ . -1 , data=testFrame, contrasts.arg = lapply(testFrame, contrasts, contrasts=FALSE))
		}else{
			formula = paste(formula,'-1,data=datos)') #[,-1]
			Random<-eval(parse(text=formula)) 
		}
		
		ETARandom <- list(X=Random,model='BRR')
		savename <- paste(savename,"Random_",sep="")
	}
	if(FixedBox=="true"){
		fixedEffects <- TotalFixeds[1]
		datos[,TotalFixeds[1]] <- as.factor(datos[,TotalFixeds[1]])
		if(length(TotalFixeds)>1){
			for(j in 2:length(TotalFixeds)){
				datos[,TotalFixeds[j]] <- as.factor(datos[,TotalFixeds[j]])
				#fixedEffectstemp <- TotalFixeds[j]
				#fixedEffects = paste(fixedEffects,fixedEffectstemp,sep='+')
			}
			testFrame <- datos[,TotalFixeds]
			Fixed <- model.matrix(~ . -1 , data=testFrame, contrasts.arg = lapply(testFrame, contrasts, contrasts=FALSE))
		}else{
			formula = paste('model.matrix(~',fixedEffects)
			formula = paste(formula,'-1,data=datos)') #[,-1]
			Fixed <- eval(parse(text=formula))
		}
		ETAFixed <- list(X=Fixed,model='FIXED')
		savename <- paste(savename,"Fixed_",sep="")
	}
	ETA <- list(ETAMarker,ETAPedigree,ETARandom,ETAFixed,savename,ExitMark)	  
	return(ETA)
}


GWAS <- function(){
	folderGWAS <- "GWAS"
	if(!file.exists(folderGWAS))  dir.create(folderGWAS)
	ETA0 <- CreaETA()
	savename <- ETA0[[5]]
	savename <- paste(folderGWAS,savename,sep="/")
	ExitMark  <- ETA0[[6]]
	noNA <- which(!is.na(ETA0))
	ETA <- list()
	for(i in 1:(length(noNA)-2)){
		ETA[[i]] <- ETA0[[noNA[i]]]
	}
	rm(ETA0)
	#####################
	### ajusta modelo ###
	#####################  
	BGLRmodel <- BGLR(y=y, ETA=ETA, nIter=nIter, burnIn=burnIn, thin=thin, saveAt=savename)
	bHat <- BGLRmodel$ETA[[1]]$b
	SD.bHat <- BGLRmodel$ETA[[1]]$SD.b
	gHat <- Markers[levels(as.factor(datos[,Gen])),]%*%bHat	
	tmpghat <- range(gHat)
	MarkerEffect <- as.data.frame(cbind(Effect=bHat,SD=SD.bHat))
	MarkerEffect <- cbind(Marker=row.names(MarkerEffect),MarkerEffect)
	write.table(MarkerEffect,paste(savename,"Marker Effects.csv",sep=""),sep=",",row.names=FALSE)
	### Estimated Marker Effects & posterior SDs
	if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'Marker Effect.pdf',sep=""),width = 10.24, height = 7.68)
	else if(formatPlot=="PNG (*.png)") png(paste(savename,'Marker Effect.png',sep=""),width = 2000, height = 1500, res=300)
	else win.metafile(paste(savename,'Marker Effect.wmf',sep=""),width = 10.24, height = 7.68)
		plot(bHat^2, ylab='Estimated Squared-Marker Effect',xlab="Marker",
			type='o',cex=.5,col=4,main='Marker Effects')
	dev.off()
	#### h2 ###
	if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'h2.pdf',sep=""),width = 10.24, height = 7.68)
	else if(formatPlot=="PNG (*.png)") png(paste(savename,'h2.png',sep=""),width = 2000, height = 1500, res=300)
	else win.metafile(paste(savename,'h2.wmf',sep=""),width = 10.24, height = 7.68)
		par(mfrow=c(1,1))
		h2 <- apply(Markers[levels(as.factor(datos[,Gen])),],2,var)/var(y)*bHat^2 
		h2 <- 100*h2/sum(h2)
		plot(h2,xlab='Marker',ylab=expression(h^2))
	dev.off()
	### Trace plots ###
	# Residual variance #
	varE=scan(paste(savename,'varE.dat',sep=''))
	### Variance E ###
	if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'residual variance.pdf',sep=""),width = 10.24, height = 7.68)
	else if(formatPlot=="PNG (*.png)") png(paste(savename,'residual variance.png',sep=""),width = 2000, height = 1500, res=300)
	else win.metafile(paste(savename,'residual variance.wmf',sep=""),width = 10.24, height = 7.68)
		plot(varE,type='o',col=2,cex=.5,ylab=expression(var[e]),xlab="Iteration by thining")
		abline(h=BGLRmodel$varE,col=4,lwd=2)
		abline(v=BGLRmodel$burnIn/BGLRmodel$thin,col=4)
	dev.off()
	### Estimated Marker Effects & posterior SDs ###
	if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'Posterior_var.pdf',sep=""),width = 10.24, height = 7.68)
	else if(formatPlot=="PNG (*.png)") png(paste(savename,'Posterior_var.png',sep=""),width = 2000, height = 1500, res=300)
	else win.metafile(paste(savename,'Posterior_var.wmf',sep=""),width = 10.24, height = 7.68)	
		par(mfrow=c(2,2))
		par(mar = c(5, 4.6, 4, 4) + 0.3)
		if(BGLRMarker=="LASSO"){
			varMark=scan(paste(savename,paste(paste('ETA_1',ExitMark,sep="_"),'dat',sep="."),sep=''))						
			plot(varE,type='l',ylab=expression(sigma[e]^2),xlab="Iteration by thining")
			truehist(varE,xlab=expression(sigma[e]^2),ylab="Density")
			plot(varMark,type='l',ylab=expression(lambda),xlab="Iteration by thining")
			truehist(varMark,xlab=expression(lambda),ylab="Density")
		}else if(BGLRMarker=="BayesB"){
			varMark=read.table(paste(savename,paste(paste('ETA_1',ExitMark,sep="_"),'dat',sep="."),sep=''),sep=" ",header=TRUE)	
			plot(varMark$probIn,type='l',ylab=expression(p),xlab="Iteration by thining")
			truehist(varMark$probIn,xlab=expression(p),ylab="Density")
			plot(varMark$scale,type='l',ylab=expression(scale),xlab="Iteration by thining")
			truehist(varMark$scale,xlab=expression(scale),ylab="Density")
		}else{
			varMark=scan(paste(savename,paste(paste('ETA_1',ExitMark,sep="_"),'dat',sep="."),sep=''))
			plot(varE,type='l',ylab=expression(sigma[e]^2),xlab="Iteration by thining")
			truehist(varE,xlab=expression(sigma[e]^2),ylab="Density")
			plot(varMark,type='l',ylab=expression(sigma[u]^2),xlab="Iteration by thining")
			truehist(varMark,xlab=expression(sigma[u]^2),ylab="Density")
		}
	dev.off()
	
	#summary(BGLRmodel) str(BGLRmodel)
	### Godness of fit and related statistics ### FALTA
	#BGLRmodel$fit
	#BGLRmodel$varE # compare to var(y)
	#100*(var(y)-BGLRmodel$varE)/var(y) # percentage reduced

	### Godness of fit and related statistics  #### FALTA
	BGLRmodelsummary <- data.frame(nIter=nIter, burnIn=burnIn, thining=thin, 
		Average_VarE=BGLRmodel$varE,Variance_Reduction=100*(var(y)-BGLRmodel$varE)/var(y),
		t(unlist(BGLRmodel$fit)))	
	write.table(round(t(BGLRmodelsummary),2),paste(savename,"Summary.csv",sep=""),sep=",",col.names=FALSE)

	file.remove(list.files("GWAS",pattern="*.dat"))
	setwd(paste(getwd(),"GWAS",sep="\\"))
	unlink("*.dat")
	setwd(dirname(getwd()))
}



Predict <- function(){
	folderPredict <- "Predict"
	if(!file.exists(folderPredict))  dir.create(folderPredict)
	ETA0 <- CreaETA()
	savename <- ETA0[[5]]
	savename <- paste(folderPredict,savename,sep="/")
	ExitMark  <- ETA0[[6]]
	noNA <- which(!is.na(ETA0))
	ETA <- list()
	for(i in 1:(length(noNA)-2)){
		ETA[[i]] <- ETA0[[noNA[i]]]
	}
	rm(ETA0)
	#####################
	### ajusta modelo ###
	#####################  
	BGLRmodel <- BGLR(y=y, ETA=ETA, nIter=nIter, burnIn=burnIn, thin=thin, saveAt=savename)	
	yHat <- BGLRmodel$yHat
	SD.yHat <- BGLRmodel$SD.yHat
	tmp <- range(y,na.rm=TRUE)
	tmphat <- range(yHat)
	BLUPs <- as.data.frame(cbind(Predicted_Value=yHat,SD=SD.yHat))
	BLUPs <- cbind(Genotype=names(yHat),BLUPs)
	# Residual variance
	varE=scan(paste(savename,'varE.dat',sep=''))
	if(MarkerBox=="true"){
		if(BGLRMarker == "GBLUP"){
			gHat <- BGLRmodel$ETA[[1]]$u
			SDgHat <- BGLRmodel$ETA[[1]]$SD.u
		}else{
			bHat <- BGLRmodel$ETA[[1]]$b
			SD.bHat <- BGLRmodel$ETA[[1]]$SD.b
			#gHat <- Markers[levels(as.factor(datos[,Gen])),]%*%bHat
			gHat <- BGLRmodel$yHat-BGLRmodel$mu
		}
		tmpghat <- range(gHat)
		BLUPs$GEBV <- gHat		
		### Just the genomic part
		if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'genomic prediction.pdf',sep=""),width = 10.24, height = 7.68)
		else if(formatPlot=="PNG (*.png)") png(paste(savename,'genomic prediction.png',sep=""),width = 2000, height = 1500, res=300)
		else win.metafile(paste(savename,'genomic prediction.wmf',sep=""),width = 10.24, height = 7.68)
			plot(gHat~y,xlab='Observed',ylab='Predicted Genomic Value',col=2,
				xlim=tmp,ylim=tmpghat)
			#abline(a=0,b=1,col=4,lwd=2)
		dev.off()
		### Trace plots		
		### Posterior VAR
		if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'Posterior_var.pdf',sep=""),width = 10.24, height = 7.68)
		else if(formatPlot=="PNG (*.png)") png(paste(savename,'Posterior_var.png',sep=""),width = 2000, height = 1500, res=300)
		else win.metafile(paste(savename,'Posterior_var.wmf',sep=""),width = 10.24, height = 7.68)	
			par(mfrow=c(2,2))
			par(mar = c(5, 4.6, 4, 4) + 0.3)
			if(BGLRMarker=="LASSO"){
				varMark=scan(paste(savename,paste(paste('ETA_1',ExitMark,sep="_"),'dat',sep="."),sep=''))						
				plot(varE,type='l',ylab=expression(sigma[e]^2),xlab="Iteration by thining")
				truehist(varE,xlab=expression(sigma[e]^2),ylab="Density")
				plot(varMark,type='l',ylab=expression(lambda),xlab="Iteration by thining")
				truehist(varMark,xlab=expression(lambda),ylab="Density")
			}else if(BGLRMarker=="BayesB"){
				varMark=read.table(paste(savename,paste(paste('ETA_1',ExitMark,sep="_"),'dat',sep="."),sep=''),sep=" ",header=TRUE)	
				plot(varMark$probIn,type='l',ylab=expression(p),xlab="Iteration by thining")
				truehist(varMark$probIn,xlab=expression(p),ylab="Density")
				plot(varMark$scale,type='l',ylab=expression(scale),xlab="Iteration by thining")
				truehist(varMark$scale,xlab=expression(scale),ylab="Density")
			}else{
				varMark=scan(paste(savename,paste(paste('ETA_1',ExitMark,sep="_"),'dat',sep="."),sep=''))
				plot(varE,type='l',ylab=expression(sigma[e]^2),xlab="Iteration by thining")
				truehist(varE,xlab=expression(sigma[e]^2),ylab="Density")
				plot(varMark,type='l',ylab=expression(sigma[u]^2),xlab="Iteration by thining")
				truehist(varMark,xlab=expression(sigma[u]^2),ylab="Density")
			}
		dev.off()
		### Posterior VAR
		if(BGLRMarker=="GBLUP"){
			if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'Heritability.pdf',sep=""),width = 10.24, height = 7.68)
			else if(formatPlot=="PNG (*.png)") png(paste(savename,'Heritability.png',sep=""),width = 2000, height = 1500, res=300)
			else win.metafile(paste(savename,'Heritability.wmf',sep=""),width = 10.24, height = 7.68)				
				Heri=varMark/(varMark+varE)
				truehist(Heri,xlab="Heritability",ylab="Density")	
			dev.off()
		}
		
		######### End Plots for markers #########
	}
	### Total prediction
	if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'Total prediction.pdf',sep=""),width = 10.24, height = 7.68)
	else if(formatPlot=="PNG (*.png)") png(paste(savename,'Total prediction.png',sep=""), width = 2000, height = 1500, res=300)
	else win.metafile(paste(savename,'Total prediction.wmf',sep=""),width = 10.24, height = 7.68)	
		plot(yHat~y,xlab='Observed',ylab='Predicted',col=2,
			xlim=tmp,ylim=tmphat);
	#abline(a=0,b=1,col=4,lwd=2)
	dev.off()

	### Residual Variance ###
	if(formatPlot=="PDF (*.pdf)") pdf(paste(savename,'residual variance.pdf',sep=""),width = 10.24, height = 7.68)
	else if(formatPlot=="PNG (*.png)") png(paste(savename,'residual variance.png',sep=""),width = 2000, height = 1500, res=300)
	else win.metafile(paste(savename,'residual variance.wmf',sep=""),width = 10.24, height = 7.68)
		plot(varE,type='o',col=2,cex=.5,ylab=expression(var[e]),xlab="Iteration by thining")
		abline(h=BGLRmodel$varE,col=4,lwd=2)
		abline(v=BGLRmodel$burnIn/BGLRmodel$thin,col=4)
	dev.off()
	write.table(BLUPs,paste(savename,"Predicted Values.csv",sep=""),sep=",",row.names=FALSE) 

	### Godness of fit and related statistics  #### FALTA
	if(BGLRMarker=="GBLUP"){
		BGLRmodelsummary <- data.frame(nIter=nIter, burnIn=burnIn, thin=thin, Correalation=cor(yHat,y), Average_VarE=BGLRmodel$varE,
			Average_Heritability=mean(Heri),Variance_Reduction=100*(var(y)-BGLRmodel$varE)/var(y),t(unlist(BGLRmodel$fit)))	
	}else{
		BGLRmodelsummary <- data.frame(nIter=nIter, burnIn=burnIn, thin=thin, Correalation=cor(yHat,y), Average_VarE=BGLRmodel$varE,
			Variance_Reduction=100*(var(y)-BGLRmodel$varE)/var(y),t(unlist(BGLRmodel$fit)))	
	}

	write.table(round(t(BGLRmodelsummary),2),paste(savename,"Summary.csv",sep=""),sep=",",col.names=FALSE)
	file.remove(list.files("Predict",pattern="*.dat"))
	setwd(paste(getwd(),"Predict",sep="\\"))
	unlink("*.dat")
	setwd(dirname(getwd()))
}


CV_Predict <- function(){
	folderPredict <- "CV_Predict"
	if(!file.exists(folderPredict))  dir.create(folderPredict)
	#savename <- paste(folderPredict,"test",sep="/")
	ETA0 <- CreaETA()
	savename <- ETA0[[5]]
	savename <- paste(folderPredict,savename,sep="/")
	ExitMark  <- ETA0[[6]]
	noNA <- which(!is.na(ETA0))
	ETA <- list()
	for(i in 1:(length(noNA)-2)){
		ETA[[i]] <- ETA0[[noNA[i]]]
	}
	rm(ETA0)

	if(typeCV=="Folds"){
		sizeFold <- floor(length(y)/nfolds)
		exced <- length(y) - sizeFold*nfolds
		theFolds <- rep(1:nfolds,sizeFold)
		if(exced>=1) theFolds <- c(theFolds,1:exced)
		theFolds <- sample(theFolds)
		TableCorr <-  matrix(NA,ncol=6,nrow=nfolds)
		for(i in 1:nfolds){
			yNA <- y
			tst <- theFolds==i
			yNA[tst] <- NA
			#####################
			### ajusta modelo ###
			#####################  
			BGLRmodel <- BGLR(y=yNA, ETA=ETA, nIter=nIter, burnIn=burnIn, thin=thin, saveAt=savename)
			TableCorr[i,1] <- i
			#TableCorr[i,2] <- cor(BGLRmodel$yHat,y)                # correlation all
			TableCorr[i,2] <- cor(BGLRmodel$yHat[tst],y[tst])      # correlation in the testing set
			TableCorr[i,3] <- mean((BGLRmodel$yHat[tst]-y[tst])^2) # MSE in the testing set
			TableCorr[i,4] <- cor(BGLRmodel$yHat[-tst],y[-tst])    # correlation in the training set
			TableCorr[i,5] <- mean((BGLRmodel$yHat[-tst]-y[-tst])^2) # MSE in the training set			
			TableCorr[i,6] <- BGLRmodel$varE  
			 
		}
		colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining","VarE")
		write.table(round(TableCorr,3),paste(savename,"CorrTable.csv",sep=""),sep=",",row.names=FALSE)
		theFolds <- as.matrix(theFolds,ncol=1)
		colnames(theFolds) <- "CrossVal"
		write.table(theFolds,paste(savename,"FoldSets.csv",sep=""),sep=",",row.names=FALSE)
		TotalCV <- nfolds
		Size_Testing=sum(tst)
	}else if(typeCV=="Load Sets"){
		theFolds <- CVSets
		valuesFolds <- unique(theFolds)
		nfolds <- length(valuesFolds)
		TableCorr <-  matrix(NA,ncol=6,nrow=nfolds)
		for(i in 1:nfolds){
			yNA <- y
			tst <- theFolds==valuesFolds[i]
			yNA[tst] <- NA
			#####################
			### ajusta modelo ###
			#####################  
			BGLRmodel <- BGLR(y=yNA, ETA=ETA, nIter=nIter, burnIn=burnIn, thin=thin, saveAt=savename)
			TableCorr[i,1] <- valuesFolds[i]
			#TableCorr[i,2] <- cor(BGLRmodel$yHat,y)                # correlation all
			TableCorr[i,2] <- cor(BGLRmodel$yHat[tst],y[tst])      # correlation in the testing set
			TableCorr[i,3] <- mean((BGLRmodel$yHat[tst]-y[tst])^2) # MSE in the testing set
			TableCorr[i,4] <- cor(BGLRmodel$yHat[-tst],y[-tst])    # correlation in the training set
			TableCorr[i,5] <- mean((BGLRmodel$yHat[-tst]-y[-tst])^2) # MSE in the training set			
			TableCorr[i,6] <- BGLRmodel$varE    
		}
		colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining","VarE")
		write.table(round(TableCorr,3),paste(savename,"CorrTable.csv",sep=""),sep=",",row.names=FALSE)
		TotalCV <- nfolds
		Size_Testing=sum(tst)
	}else{ ##"Training\Testing %"
		TestSizeNum <- floor(length(y)*TestSize/100)
		TableCorr <-  matrix(NA,ncol=6,nrow=numberCV)
		Randomizations <-  matrix(NA,ncol=numberCV,nrow=TestSizeNum)
		for(i in 1:numberCV){
			tst <- sample(1:length(y),TestSizeNum)
			yNA <- y
			yNA[tst] <- NA
			Randomizations[,i] <- sort(tst)
			#####################
			### ajusta modelo ###
			#####################  
			BGLRmodel <- BGLR(y=yNA, ETA=ETA, nIter=nIter, burnIn=burnIn, thin=thin, saveAt=savename)	
			TableCorr[i,1] <- i
			#TableCorr[i,2] <- cor(BGLRmodel$yHat,y)                # correlation all
			TableCorr[i,2] <- cor(BGLRmodel$yHat[tst],y[tst])      # correlation in the testing set
			TableCorr[i,3] <- mean((BGLRmodel$yHat[tst]-y[tst])^2) # MSE in the testing set
			TableCorr[i,4] <- cor(BGLRmodel$yHat[-tst],y[-tst])    # correlation in the training set
			TableCorr[i,5] <- mean((BGLRmodel$yHat[-tst]-y[-tst])^2) # MSE in the training set			
			TableCorr[i,6] <- BGLRmodel$varE  
		}
		colnames(TableCorr) <- c("CrossVal","CorrTesting","MSETesting","CorrTraining","MSETraining","VarE")
		write.table(round(TableCorr,3),paste(savename,"CorrTable.csv",sep=""),sep=",",row.names=FALSE)
		colnames(Randomizations) <- paste("CV",1:numberCV,sep="_")
		write.table(Randomizations,paste(savename,"RandomTestingSets.csv",sep=""),sep=",",row.names=FALSE)
		TotalCV <- numberCV
		Size_Testing=length(tst)
	}

	#pdf(paste(savename,"Correlations_CV.pdf",sep=""))
	#barplot(TableCorr[,3],names.arg=paste("CV",TableCorr[,1],sep="_"),ylab="Correlation",main="")
	#abline(h=mean(TableCorr[,3]),col="red",lwd=2)
	#dev.off()

	pdf(paste(savename,"Boxplot_Corr_CV.pdf",sep=""))
	boxplot(TableCorr[,c(2,4)],col=c("red","blue"),names=c("Testing Set","Training Set"),ylim=c(0,1))
	legend("bottomright", paste(c("Cor Testing:", "MSE Testing:", "Cor Training:", "MSE Training:"),
		round(colMeans(TableCorr[,2:5]),2),sep=" "),text.col =c("red","red","blue","blue"),fill=c("red","red","blue","blue"))
	#plot(TableCorr[,1],TableCorr[,2],ylab="Correlation",xlab="Cross Validation",main="",pch=19,type="o",ylim=c(0,1))
	#points(TableCorr[,1],TableCorr[,3],col="red",type="o",pch=19)
	#points(TableCorr[,1],TableCorr[,4],col="blue",type="o",pch=19)
	#leg.txt <- c("Corr all", "Corr testing", "Corr training")
	#legend("topright", leg.txt, col=c("black","red","blue"), pch = 19,lty = 1)
	dev.off()

	#pdf(paste(savename,"Scatterplot_CV.pdf",sep=""))
	#plot(BGLRmodel$yHat,y,ylab="Observed",xlab="Predicted",pch=19)
	#points(BGLRmodel$yHat[tst],y[tst],col="red",pch=19)
	#dev.off()

	### Godness of fit and related statistics  #### FALTA
	BGLRmodelsummary <- data.frame(nIter=nIter, burnIn=burnIn, thining=thin, TotalCV, Percentage_Testing=round(100*Size_Testing/length(y),2),
		Size_Testing=Size_Testing, Size_Training=length(y)-Size_Testing, t(colMeans(TableCorr[,2:6])) )	
	names(BGLRmodelsummary)[8:12] <- paste(names(BGLRmodelsummary)[8:12],"Average",sep="_")
	write.table(round(t(BGLRmodelsummary),2),paste(savename,"Summary.csv",sep=""),sep=",",col.names=FALSE)

	#file.remove(list.files("CV_Predict",pattern="*.dat"))
	setwd(paste(getwd(),"CV_Predict",sep="\\"))
	unlink("*.dat")
	setwd(dirname(getwd()))

}
