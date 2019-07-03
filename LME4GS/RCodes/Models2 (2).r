Models_lme4gs <- function(yy=y,gen=gen,Pedigree=Pedigree,Markers=Markers,RandomBox = "false",FixedBox="false"){
  library("lme4GS")
  library("BGLR")

  aleat = NULL
  mrk=NULL
  ped=NULL
  efale=NULL
  random = NULL

  #En que parte del código se bede poner gen
  if(MarkerBox=="true"){
    X<-Markers
    Z<-scale(X,center=TRUE,scale=TRUE)
    G<-tcrossprod(Z)/ncol(Z)
    G<-as.matrix(G)
    rownames(G) <- colnames(G) <- gen 
    mrk=list(K=G,id=gen)
    random = c(random,mrk=list(mrk))
  }
  
  if(PedigreeBox=="true")
  {
    A<-as.matrix(Pedigree)
    rownames(A) <- colnames(A) <- gen 
    ped=list(K=A,id=gen)
    random = c(random,ped=list(ped))
  }
  
  if(RandomBox == "true")
  {
    formula <- TotalRandoms[1]
    datos[,formula] <- as.factor(datos[,formula])
    formula = paste('model.matrix(~',formula)
    if(length(TotalRandoms)>1){
      for(j in 2:length(TotalRandoms)){
        datos[,TotalRandoms[j]] <- as.factor(datos[,TotalRandoms[j]])
      }
      testFrame <- datos[,TotalRandoms]
      Aleat <- model.matrix(~ . -1 , data=testFrame, contrasts.arg = lapply(testFrame, contrasts, contrasts=FALSE))
    }else{
      formula = paste(formula,'-1') #[,-1]
      Aleat<-eval(parse(text=formula)) 
    }
    
    efale <- list(K=random,id=gen)
    random = c(random,list=efale)
  }
  
  fijo = "1"
  
  if(FixedBox=="true"){
    fixedEffects <- TotalFixeds[1]
    datos[,TotalFixeds[1]] <- as.factor(datos[,TotalFixeds[1]])
    if(length(TotalFixeds)>1){
      for(j in 2:length(TotalFixeds)){
        datos[,TotalFixeds[j]] <- as.factor(datos[,TotalFixeds[j]])
      }
      testFrame <- datos[,TotalFixeds]
      Fixed <- model.matrix(~ . -1 , data=testFrame, contrasts.arg = lapply(testFrame, contrasts, contrasts=FALSE))
    }else{
      formula = paste('~',fixedEffects)
      Fixed <- eval(parse(text=formula))
    }
    fijo = paste(fijo,Fixed)
  }
  names(yy) = gen
  out<-lmer_uvcov_beta(yy,fixed=fijo,random=random)
  summary(out)
  
  #efectos fijos
  
  
  #componentes de varianza
  a = data.frame(VarCorr(out,comp="Variance"))
  write.table(a, paste0(yname,"_var_comp.csv",sep=""),sep=",",row.names=FALSE)
  
  #prediccion
  y2 = predict(out)
  write.table(y2,paste0(yname,"_pred.csv",sep=""),sep=",",row.names=FALSE)
  pdf(file=paste0(yname,"_scatterplot.pdf"))
  plot(yy,predict(out))
  dev.off()
  
  #Conditional means of the random effects
  out2=ranef_uvcov(out)
  write.table(out2,paste0(yname,"_CMeans.csv", sep=""), sep=",", row.names = FALSE)
  
  if(CVbox == "true")
  {
    if(typeCV=="Folds")
    {
      sizeFold <- floor(length(yy)/nfolds)
      exced <- length(yy) - sizeFold*nfolds
      theFolds <- rep(1:nfolds,sizeFold)
      if(exced>=1) theFolds <- c(theFolds,1:exced)
      theFolds <- sample(theFolds)
      ######Checar por que ncol=5
      TableCorr <-  matrix(NA,ncol=5,nrow=nfolds)
      
      for(i in 1:nfolds)
      {
        tst <- theFolds==i
        trn <- theFolds!=i
        names(yy) = gen
        yNA = yy[tst]
        yNA2 = yy[trn]

        #Preparación del modelo
        

        random1 =list(mrk=list(K=G,id=names(yNA)),
                              ped=list(K=A,id=names(yNA)))
        random2 =list(mrk=list(K=G,id=names(yNA2)),
                              ped=list(K=A,id=names(yNA2)))
        out1<-lmer_uvcov_beta(yNA,fixed="1",random=random1)
        out2<-lmer_uvcov_beta(yNA2,fixed="1",random=random2)
        yy1 = predict(out1)
        yy2 = predict(out2)
        TableCorr[i,1] <- i
        TableCorr[i,2] <- cor(yy1,yNA)      # correlation in the testing set
        TableCorr[i,3] <- mean((yy1-yNA)^2) # MSE in the testing set
        TableCorr[i,4] <- cor(yy2,yNA2)    # correlation in the training set
        TableCorr[i,5] <- mean((yy2-yNA2)^2) # MSE in the training set
      }
      colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining")
      write.table(round(TableCorr,3),paste("CorrTable.csv",sep=""),sep=",",row.names=FALSE)
      theFolds <- as.matrix(theFolds,ncol=1)
      colnames(theFolds) <- "CrossVal"
      write.table(theFolds,paste("y_",yname,"_FoldSets.csv",sep=""),sep=",",row.names=FALSE)
      TotalCV <- nfolds
      Size_Testing=sum(tst)
      
    } else if(typeCV=="Load Sets"){
      theFolds <- CVSets
      valuesFolds <- unique(theFolds)
      nfolds <- length(valuesFolds)
      TableCorr <-  matrix(NA,ncol=5,nrow=nfolds)
      
      for(i in 1:nfolds){
        tst <- theFolds==valuesFolds[i]
        tst <- theFolds!=valuesFolds[i]
        yNA = yy[tst]
        yNA2 = yy[trn]
        random1 =list(mrk=list(K=G,id=names(yNA)),
                              ped=list(K=A,id=names(yNA)))
        random2 =list(mrk=list(K=G,id=names(yNA2)),
                              ped=list(K=A,id=names(yNA2)))
        out1<-lmer_uvcov_beta(yNA,fixed="1",random=random1)
        out2<-lmer_uvcov_beta(yNA2,fixed="1",random=random2)
        yy1 = predict(out1)
        yy2 = predict(out2)
        TableCorr[i,1] <- valuesFolds[i]
        TableCorr[i,2] <- cor(yy1,yNA)      # correlation in the testing set
        TableCorr[i,3] <- mean((yy1-yNA)^2) # MSE in the testing set
        TableCorr[i,4] <- cor(yy2,yNA2)    # correlation in the training set
        TableCorr[i,5] <- mean((yy2-yNA2)^2) # MSE in the training set
      }
      colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining")
      write.table(round(TableCorr,3),paste("y_",yname,"_CorrTable.csv",sep=""),sep=",",row.names=FALSE)
      TotalCV <- nfolds
      Size_Testing=sum(tst)
      
    }else{ ##"Training\Testing %"
      
      TestSizeNum <- floor(length(yy)*TestSize/100)
      TableCorr <-  matrix(NA,ncol=5,nrow=numberCV)
      Randomizations <-  matrix(NA,ncol=numberCV,nrow=TestSizeNum)
      for(i in 1:numberCV){
        tst <- sample(1:length(yy),TestSizeNum)
        yNA = yy[tst]
        yNA2 = yy[-tst]
        random1 =list(mrk=list(K=G,id=names(yNA)),
                              ped=list(K=A,id=names(yNA)))
        random2 =list(mrk=list(K=G,id=names(yNA2)),
                              ped=list(K=A,id=names(yNA2)))
        out1<-lmer_uvcov_beta(yNA,fixed="1",random=random1)
        out2<-lmer_uvcov_beta(yNA2,fixed="1",random=random2)
        yy1 = predict(out1)
        yy2 = predict(out2)

        Randomizations[,i] <- sort(tst)

        TableCorr[i,1] <- i
        TableCorr[i,2] <- cor(yy1,yNA)      # correlation in the testing set
        TableCorr[i,3] <- mean((yy1-yNA)^2) # MSE in the testing set
        TableCorr[i,4] <- cor(yy2,yNA2)    # correlation in the training set
        TableCorr[i,5] <- mean((yy2-yNA2)^2) # MSE in the training set			
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