library("lme4GS")
library("BGLR")
datos = read.csv("C:/BGLR/Examples/Wheat_GrainYield.csv")
Markers = read.csv("C:/BGLR/Examples/Wheat_Markers.csv")
Pedigree=read.csv("C:/BGLR/Examples/Wheat_Pedigree.csv")
y<-datos[,2]
gen = as.character(datos[,1])
Pedigree=Pedigree[,-1]
rownames(Pedigree) = gen
X<-Markers[,-1]
Z<-scale(X,center=TRUE,scale=TRUE)
G<-as.matrix(tcrossprod(Z)/ncol(Z))
A<-as.matrix(Pedigree)
rownames(G)<-colnames(G)<-gen
rownames(A)<-colnames(A)<-gen
random<-list(mrk=list(K=G,id=rownames(G)),
             ped=list(K=A,id=rownames(A)))

out<-lmer_uvcov(y,fixed="1",random=random)

####Example of function lmer_uvcov in other function
MarkerBox="true"
PedigreeBox="true"

datos = read.csv("C:/BGLR/Examples/Wheat_GrainYield.csv")
Markers = read.csv("C:/BGLR/Examples/Wheat_Markers.csv")
Pedigree=read.csv("C:/BGLR/Examples/Wheat_Pedigree.csv")

Models_lme4gs<- function(MarkerBox="true", PedigreeBox="true"){
  if(MarkerBox=="true"  & PedigreeBox=="true"){
    y<-datos[,2]
    yvar = colnames(datos)[2]
    gen = as.character(datos[,1])
    Pedigree=Pedigree[,-1]
    rownames(Pedigree) = gen
    X<-Markers[,-1]
    Z<-scale(X,center=TRUE,scale=TRUE)
    G<-as.matrix(tcrossprod(Z)/ncol(Z))
    A<-as.matrix(Pedigree)
    rownames(G)<-colnames(G)<-gen
    rownames(A)<-colnames(A)<-gen
    random<-list(mrk=list(K=G,id=rownames(G)),
                 ped=list(K=A,id=rownames(A)))
    out<-lmer_uvcov(y,fixed="1",random=random)
    summary(out)
    pdf(file=paste(yvar,"_scatterplot.pdf"))
    plot(y,predict(out))
    dev.off()
    a = data.frame(VarCorr(out,comp="Variance"))
    write.table(a,paste(yvar,"_var_comp.csv",sep=""),sep=",",row.names=FALSE)
    y2 = predict(out)
    write.table(y2,paste(yvar,"_pred.csv",sep=""),sep=",",row.names=FALSE)
  }
  
}
Models_lme4gs(MarkerBox="true", PedigreeBox="true")

####Example of function lmer_uvcov in other function and cross validation
MarkerBox="true"
PedigreeBox="true"
CVbox = "true"
typeCV="Folds"
nfolds=10

Models_lme4gs2<- function(MarkerBox="true", PedigreeBox="true"){
  if(MarkerBox=="true"  & PedigreeBox=="true"){
    yvar = colnames(datos)[2]
    out<-lmer_uvcov(y,fixed="1",random=random)
    summary(out)
    pdf(file=paste(yvar,"_scatterplot.pdf"))
    plot(y,predict(out))
    dev.off()
    a = data.frame(VarCorr(out,comp="Variance"))
    write.table(a,paste(yvar,"_var_comp.csv",sep=""),sep=",",row.names=FALSE)
    y2 = predict(out)
    write.table(y2,paste(yvar,"_pred.csv",sep=""),sep=",",row.names=FALSE)
  }
  if(CVbox == "true")
  {
    if(typeCV=="Folds"){
      sizeFold <- floor(length(y)/nfolds)
      exced <- length(y) - sizeFold*nfolds
      theFolds <- rep(1:nfolds,sizeFold)
      if(exced>=1) theFolds <- c(theFolds,1:exced)
      theFolds <- sample(theFolds)
      ######Checar por que ncol=5
      TableCorr <-  matrix(NA,ncol=5,nrow=nfolds)
      
      for(i in 1:nfolds){
        tst <- theFolds==i
        trn <- theFolds!=i
        names(y) = gen
        yNA = y[tst]
        yNA2 = y[trn]
        random1 =random<-list(mrk=list(K=G,id=names(yNA)),
                              ped=list(K=A,id=names(yNA)))
        random2 =random<-list(mrk=list(K=G,id=names(yNA2)),
                              ped=list(K=A,id=names(yNA2)))
        out1<-lmer_uvcov(yNA,fixed="1",random=random1)
        out2<-lmer_uvcov(yNA2,fixed="1",random=random2)
        yy1 = predict(out1)
        yy2 = predict(out2)
        TableCorr[i,1] <- i
        TableCorr[i,2] <- cor(yy1,y[tst])      # correlation in the testing set
        TableCorr[i,3] <- mean((yy1-y[tst])^2) # MSE in the testing set
        TableCorr[i,4] <- cor(yy2,y[trn])    # correlation in the training set
        TableCorr[i,5] <- mean((yy2-y[trn])^2) # MSE in the training set
      }
      colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining")
      write.table(round(TableCorr,3),paste("CorrTable.csv",sep=""),sep=",",row.names=FALSE)
      theFolds <- as.matrix(theFolds,ncol=1)
      colnames(theFolds) <- "CrossVal"
      write.table(theFolds,paste("y_",yname,"_FoldSets.csv",sep=""),sep=",",row.names=FALSE)
      TotalCV <- nfolds
      Size_Testing=sum(tst)
      
    }else if(typeCV=="Load Sets"){
      theFolds <- CVSets
      valuesFolds <- unique(theFolds)
      nfolds <- length(valuesFolds)
      TableCorr <-  matrix(NA,ncol=5,nrow=nfolds)
      
      for(i in 1:nfolds){
        tst <- theFolds==valuesFolds[i]
        yNA = y[tst]
        out<-lmer_uvcov(yNA,fixed=fijo,random=aleat)
        TableCorr[i,1] <- valuesFolds[i]
        TableCorr[i,2] <- cor(predict(out)[tst],y[tst])      # correlation in the testing set
        TableCorr[i,3] <- mean((predict(out)[tst]-y[tst])^2) # MSE in the testing set
        TableCorr[i,4] <- cor(predict(out)[-tst],y[-tst])    # correlation in the training set
        TableCorr[i,5] <- mean((predict(out)[-tst]-y[-tst])^2) # MSE in the training set
      }
      colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining")
      write.table(round(TableCorr,3),paste("y_",yname,"_CorrTable.csv",sep=""),sep=",",row.names=FALSE)
      TotalCV <- nfolds
      Size_Testing=sum(tst)
      
    }else{ ##"Training\Testing %"
      
      TestSizeNum <- floor(length(y)*TestSize/100)
      TableCorr <-  matrix(NA,ncol=5,nrow=numberCV)
      Randomizations <-  matrix(NA,ncol=numberCV,nrow=TestSizeNum)
      for(i in 1:numberCV){
        tst <- sample(1:length(y),TestSizeNum)
        yNA = y[tst]
        Randomizations[,i] <- sort(tst)
        out<-lmer_uvcov(yNA,fixed=fijo,random=aleat)
        TableCorr[i,1] <- i
        TableCorr[i,2] <- cor(predict(out)[tst],y[tst])      # correlation in the testing set
        TableCorr[i,3] <- mean((predict(out)[tst]-y[tst])^2) # MSE in the testing set
        TableCorr[i,4] <- cor(predict(out)[-tst],y[-tst])    # correlation in the training set
        TableCorr[i,5] <- mean((predict(out)[-tst]-y[-tst])^2) # MSE in the training set			
      }
      colnames(TableCorr) <- c("CrossVal","CorrTesting","MSETesting","CorrTraining","MSETraining")
      write.table(round(TableCorr,3),paste("y_",yname,"_CorrTable.csv",sep=""),sep=",",row.names=FALSE)
      colnames(Randomizations) <- paste("CV",1:numberCV,sep="_")
      write.table(Randomizations,paste("y_", yname,"_RandomTestingSets.csv",sep=""),sep=",",row.names=FALSE)
      TotalCV <- numberCV
      Size_Testing=length(tst)
      
    }
    
    pdf(paste( yname, "_Boxplot_Corr_CV.pdf",sep=""))
    boxplot(TableCorr[,c(2,4)],col=c("red","blue"),names=c("Testing Set","Training Set"),ylim=c(0,1))
    legend("bottomright", paste(c("Cor Testing:", "MSE Testing:", "Cor Training:", "MSE Training:"),
                                round(colMeans(TableCorr[,2:5]),2),sep=" "),text.col =c("red","red","blue","blue"),fill=c("red","red","blue","blue"))
    dev.off()  
  }
  
}
Models_lme4gs2()
