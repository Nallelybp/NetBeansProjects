

predict_lme4gs <- function(datos, Markers, Pedigree, 
                          TotalRandoms, TotalFixeds, 
                          Gen, y, yvar,
                          MarkerBox, PedigreeBox, 
                          RandomBox, FixedBox){

  library("lme4")
  library("BGLR")
  folderPredict = "Predict/"
  if(!file.exists(folderPredict))  dir.create(folderPredict)
  random = NULL
  fixedVec = NULL
  coln = NULL
  gen <- as.character(datos[, Gen])
  savename = paste
  if(MarkerBox=="true"){
    X<-Markers
    Z<-scale(X,center=TRUE,scale=TRUE)
    G<-tcrossprod(Z)/ncol(Z)
    G<-as.matrix(G)
    rownames(G) <- colnames(G) <- gen 
    mrk=list(K=G,id=gen)
    random = c(random,list(mrk))
    coln = c(coln, "mrk")
  }
  
  if(PedigreeBox=="true")
  {
    A<-as.matrix(Pedigree)
    rownames(A) <- colnames(A) <- gen 
    ped=list(K=A,id=gen)
    random = c(random, list(ped))
    coln = c(coln, "ped")
  }
  
  if(RandomBox == "true")
  {
    
    for(var in TotalRandoms) {
      print(var)
      rvar = list(K = NULL, id = datos[, var])
      random = c(random, list(rvar))
      coln = c(coln, paste("c",var, sep=""))
    }

  }
  
  names(random) <- coln
  #cat(coln)
  
  coln = NULL
  if(FixedBox == "true") {
    for(var in TotalFixeds) {
      fvar = datos[, var]
      fixedVec = c(fixedVec, list(fvar))
      coln = c(coln, paste("c", var, sep=""))
    }
  }
  names(fixedVec) <- coln
  
  #return (list(y = y, fixed = fixed, random = random))
  
  out<-lmer_uvcov_beta(y = y,fixed=fixedVec,random=random)
  summary(out)
  

  
  #componentes de varianza
  a = data.frame(VarCorr(out,comp="Variance"))
  write.table(a, paste0(folderPredict,yvar,"_var_comp.csv",sep=""),sep=",",row.names=FALSE)
  
  #prediccion
  y2 = predict(out)
  write.table(y2,paste0(folderPredict,yvar,"_pred.csv",sep=""),sep=",",row.names=FALSE)
  pdf(file=paste0(folderPredict, yvar,"_scatterplot.pdf"))
  plot(y,predict(out))
  dev.off()
  
  #Conditional means of the random effects
  out2=ranef_uvcov(out)
  write.table(out2,paste0(folderPredict,yvar,"_CMeans.csv", sep=""), sep=",", row.names = FALSE)
} 


predict_lme4gs_cv<- function(datos, Markers, Pedigree, 
                      TotalRandoms, TotalFixeds,
                      Gen, y, yvar,
                      MarkerBox, PedigreeBox,
                      RandomBox, FixedBox, typeCV, nfolds, CVSets, NumberCV) {
  
  folderPredict = "Predict_CV/"
  if(!file.exists(folderPredict))  dir.create(folderPredict)
  
  if(typeCV=="Folds")
  {

    sizeFold <- floor(length(y)/nfolds)
    exced <- length(y) - sizeFold*nfolds
    theFolds <- rep(1:nfolds,sizeFold)
    if(exced>=1) theFolds <- c(theFolds,1:exced)
    theFolds <- sample(theFolds)

    TableCorr <-  matrix(NA,ncol=5,nrow=nfolds)
    gen <- as.character(datos[, Gen])
    names(y) = gen
    
    
    for(i in 1:nfolds)
    {
      tst <- theFolds==i
      trn <- theFolds!=i
      
      y_tst = y[tst]
      y_trn = y[trn]

      
      #Preparaci�n del modelo
      random_tst = NULL #Test
      random_trn = NULL #Training
      
      if(MarkerBox=="true"){
        X<-Markers
        Z<-scale(X,center=TRUE,scale=TRUE)
        G<-tcrossprod(Z)/ncol(Z)
        G<-as.matrix(G)
        rownames(G) <- colnames(G) <- gen 
        mrk_tst=list(K=G,id=names(y_tst))
        mrk_trn=list(K=G,id=names(y_trn))
        random_tst= c(random_tst, mrk = list(mrk_tst))
        random_trn = c(random_trn, mrk = list(mrk_trn))
      }
      
      
      if(PedigreeBox=="true")
      {
        A<-as.matrix(Pedigree)
        rownames(A) <- colnames(A) <- gen 
        ped_tst=list(K=A,id=names(y_tst))
        ped_trn=list(K=A,id=names(y_trn))
        random_tst = c(random_tst, ped = list(ped_tst))
        random_trn = c(random_trn, ped = list(ped_trn))
      }

      if(RandomBox == "true")
      {

      }
 
      if(FixedBox=="true"){

      }
      
      out<-lmer_uvcov_beta(y = y_trn, fixed="1",random=random_trn)
      pred_trn = predict(out)
      blup_tst = predict_uvcov(out,random_tst)
      blup_total = 0
      if(MarkerBox == "true") {
        blup_total = blup_total + blup_tst$mrk
      }
      if(PedigreeBox == "true") {
        blup_total = blup_total + blup_tst$ped
      }
      pred_tst = fixef(out)[1]+blup_total
      
      TableCorr[i,1] <- i
      TableCorr[i,2] <- cor(y_tst,pred_tst)      # correlation in the testing set
      TableCorr[i,3] <- mean((y_tst-pred_tst)^2) # MSE in the testing set
      TableCorr[i,4] <- cor(y_trn, pred_trn)    # correlation in the training set
      TableCorr[i,5] <- mean((y_trn-pred_trn)^2) # MSE in the training set
    }
    
    colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining")
    write.table(round(TableCorr,3), paste(folderPredict, yvar,"_CorrTable.csv",sep=""),sep=",",row.names=FALSE)
    theFolds <- as.matrix(theFolds,ncol=1)
    colnames(theFolds) <- "CrossVal"
    write.table(theFolds,paste(folderPredict, yvar,"_FoldSets.csv",sep=""),sep=",",row.names=FALSE)
    TotalCV <- nfolds
    Size_Testing=sum(tst)
    
  } else if(typeCV=="Load Sets"){
    theFolds <- CVSets
    valuesFolds <- unique(theFolds)
    nfolds <- length(valuesFolds)
    TableCorr <-  matrix(NA,ncol=5,nrow=nfolds)
    gen <- as.character(datos[, Gen])
    names(y) = gen
    
    for(i in 1:nfolds){
      tst <- theFolds==valuesFolds[i]
      trn <- theFolds!=valuesFolds[i]
      y_tst = y[tst]
      y_trn = y[trn]
      
      random_tst = NULL #Test
      random_trn = NULL #Training
      
      if(MarkerBox=="true"){
        X<-Markers
        Z<-scale(X,center=TRUE,scale=TRUE)
        G<-tcrossprod(Z)/ncol(Z)
        G<-as.matrix(G)
        rownames(G) <- colnames(G) <- gen 
        mrk_tst=list(K=G,id=names(y_tst))
        mrk_trn=list(K=G,id=names(y_trn))
        random_tst= c(random_tst,mrk=list(mrk_tst))
        random_trn = c(random_trn,mrk=list(mrk_trn))
      }
      
      if(PedigreeBox=="true")
      {
        A<-as.matrix(Pedigree)
        rownames(A) <- colnames(A) <- gen 
        ped_tst=list(K=A,id=names(y_tst))
        ped_trn=list(K=A,id=names(y_trn))
        random_tst = c(random_tst,ped=list(ped_tst))
        random_trn = c(random_trn,ped=list(ped_trn))
      }
      
      if(RandomBox == "true")
      {
        
      }
    
      if(FixedBox=="true"){
        
      }

      
      out<-lmer_uvcov_beta(y_trn, fixed="1",random=random_trn)
      pred_trn = predict(out)
      blup_tst = predict_uvcov(out,random_tst)
      blup_total = 0
      if(MarkerBox == "true") {
        blup_total = blup_total + blup_tst$mrk
      }
      if(PedigreeBox == "true") {
        blup_total = blup_total + blup_tst$ped
      }
      pred_tst = fixef(out)[1]+blup_total
      
      TableCorr[i,1] <- valuesFolds[i]
      TableCorr[i,2] <- cor(y_tst,pred_tst)      # correlation in the testing set
      TableCorr[i,3] <- mean((y_tst-pred_tst)^2) # MSE in the testing set
      TableCorr[i,4] <- cor(y_trn,pred_trn)    # correlation in the training set
      TableCorr[i,5] <- mean((y_trn-pred_trn)^2) # MSE in the training set
    }
    colnames(TableCorr) <- c("Fold","CorrTesting","MSETesting","CorrTraining","MSETraining")
    write.table(round(TableCorr,3),paste(folderPredict, yvar,"_CorrTable.csv",sep=""),sep=",",row.names=FALSE)
    TotalCV <- nfolds
    Size_Testing=sum(tst)
    
  }else{ ##"Training\Testing %"
    
    TestSizeNum <- floor(length(y)*TestSize/100)
    TableCorr <-  matrix(NA,ncol=5,nrow=numberCV)
    Randomizations <-  matrix(NA,ncol=numberCV,nrow=TestSizeNum)
    gen <- as.character(datos[, Gen])
    names(y) = gen
    
    for(i in 1:numberCV){
      tst <- sample(1:length(y),TestSizeNum)
      y_tst = y[tst]
      y_trn = y[-tst]

      random_tst = NULL #Test
      random_trn = NULL #Training
      
      if(MarkerBox=="true"){
        X<-Markers
        Z<-scale(X,center=TRUE,scale=TRUE)
        G<-tcrossprod(Z)/ncol(Z)
        G<-as.matrix(G)
        rownames(G) <- colnames(G) <- gen 
        mrk_tst=list(K=G,id=names(y_tst))
        mrk_trn=list(K=G,id=names(y_trn))
        random_tst= c(random_tst,mrk=list(mrk_tst))
        random_trn = c(random_trn,mrk=list(mrk_trn))

      }
      
      if(PedigreeBox=="true")
      {
        A<-as.matrix(Pedigree)
        rownames(A) <- colnames(A) <- gen 
        ped_tst=list(K=A,id=names(y_tst))
        ped_trn=list(K=A,id=names(y_trn))
        random_tst = c(random_tst,ped=list(ped_tst))
        random_trn = c(random_trn,ped=list(ped_trn))
 
      }
      
      if(RandomBox == "true")
      {
        
      }

      if(FixedBox=="true"){
        
      }

      
      out<-lmer_uvcov_beta(y_trn, fixed="1",random=random_trn)
      pred_trn = predict(out)
      blup_tst = predict_uvcov(out,random_tst)
      blup_total = 0
      if(MarkerBox == "true") {
        blup_total = blup_total + blup_tst$mrk
      }
      if(PedigreeBox == "true") {
        blup_total = blup_total + blup_tst$ped
      }
      pred_tst = fixef(out)[1]+blup_total
      
      Randomizations[,i] <- sort(tst)
      
      TableCorr[i,1] <- i
      TableCorr[i,2] <- cor(y_tst,pred_tst)      # correlation in the testing set
      TableCorr[i,3] <- mean((y_tst-pred_tst)^2) # MSE in the testing set
      TableCorr[i,4] <- cor(y_trn,pred_trn)    # correlation in the training set
      TableCorr[i,5] <- mean((y_trn-pred_trn)^2) # MSE in the training set			
    }
    colnames(TableCorr) <- c("CrossVal","CorrTesting","MSETesting","CorrTraining","MSETraining")
    write.table(round(TableCorr,3),paste(folderPredict, yvar,"_CorrTable.csv",sep=""),sep=",",row.names=FALSE)
    colnames(Randomizations) <- paste("CV",1:numberCV,sep="_")
    write.table(Randomizations,paste(folderPredict, yvar,"_RandomTestingSets.csv",sep=""),sep=",",row.names=FALSE)
    TotalCV <- numberCV
    Size_Testing=length(tst)
    
  }
  
  pdf(paste(folderPredict, yvar, "_Boxplot_Corr_CV.pdf",sep=""))
  boxplot(TableCorr[,c(2,4)],col=c("red","blue"),names=c("Testing Set","Training Set"),ylim=c(0,1))
  legend("bottomright", paste(c("Cor Testing:", "MSE Testing:", "Cor Training:", "MSE Training:"),
                              round(colMeans(TableCorr[,2:5]),2),sep=" "),text.col =c("red","red","blue","blue"),fill=c("red","red","blue","blue"))
  dev.off()  
}

#predict_lme4gs_cv(datos, Markers, Pedigree, 
#                  TotalRandoms, TotalFixeds,
#                  Gen, y, yvar,
#                  MarkerBox, PedigreeBox,
#                  RandomBox, FixedBox, CVType, nfolds, CVSets, NumberCV)

