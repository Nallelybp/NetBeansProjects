Models_lme4gs<- function(savename,MarkerBox="true",PedigreeBox="true"){

    library("lme4GS")
    library("BGLR")
	timein <- proc.time()
	FolderModels <- "Models"

	# savename <- paste(colnames(yvar),"with_",sep="_")
	# savename <- paste(folderModels,savename,sep="/")
	
	# if(!file.exists(FolderModels))  dir.create(FolderModels)
	# savename <- paste(paste(paste(yvar,"with_model",sep="_"),sep="_"),"_",sep="")
    # savename <- paste(FolderModels,savename,sep="/")

    aleat = NULL
    mrk=NULL
    ped=NULL
    efale=NULL


    if(MarkerBox=="true"  & PedigreeBox=="true"){
        X<-Markers
		Z<-scale(X,center=TRUE,scale=TRUE)
        G<-tcrossprod(Z)/ncol(Z)
        A<-as.matrix(Pedigree)
        #rownames(G)<-colnames(G)<-rownames(A)
        y<-datos[,2]
        random<-list(mrk=list(K=G,id=gen),
                     ped=list(K=A,id=gen))

        out<-lmer_uvcov(y,fixed="1",random=random)
        summary(out)
        plot(y,predict(out))
        a = data.frame(VarCorr(out,comp="Variance"))
        write.table(a,paste(savename,"var_comp.csv",sep=""),sep=",",row.names=FALSE)
        y2 = predict(out)
        write.table(y2,paste(savename,"_",yvar,"_pred.csv",sep=""),sep=",",row.names=FALSE)
    }


 }