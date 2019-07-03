# Configuración Temporal --------------------------------------------------
gc(reset=TRUE)

libpath = "C:/Users/NBAUTISTA/Documents/NetBeansProjects/BASE/win-library/3.5"
.libPaths(normalizePath(libpath))

library(BGLR)
library(lme4)

codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/lmer_uvcov_beta.R", sep = "")
source(codeR)

codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/predict.R", sep = "")
source(codeR)

codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/relfac.R", sep = "")
source(codeR)

codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/theta_optim.R", sep = "")
source(codeR)


###################################Configuraciones ##############################

#1) Configuracion general

libpath = "C:/Users/NBAUTISTA/Documents/NetBeansProjects/BASE/win-library/3.5"
mainpath = "C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1";
Sys.setenv('JAVA_HOME' = 'C:/Program Files/Java/jdk1.8.0_211/')


#2) Configuracion para cargar datos

path = "C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/Examples"
original_path = path;
archivo = "C:/BGLR/Examples/Wheat_GrainYield.csv"
datos=archivo
outputFolder = "Analysis1"

#3) Configuracion para cargar marcadores

MarkersDir = "C:/BGLR/Examples/Wheat_Markers.csv"
#MarkersDir = "C:/BGLR/Examples/TC-10-Genotypes-AC_GT.csv"
needRecodification = FALSE;

#4) Configuracion para Cargar relationships

PedDir = "C:/BGLR/Examples/Wheat_Pedigree.csv"

#5) Configuracion del analisis

AnalysesToDo = "Prediction" #Load Sets, Training/testing

formatPlot = "PDF (*.pdf)";

#No se que sea la variable Site, solo que tiene el valor de Choose
Site = NULL
Gen = "GID"

Responses = c(1, 2, 4, 5);

MarkerBox = "true";

if(MarkerBox == "true") {
  #BGLRMarker = "Ridge"
}

PedigreeBox = "true";

 
RandomBox = "false";
#Si RandomBox es TRUE, debes indicar la variables aleatorias
if(RandomBox == "true") {
#Modificar las variables random (que no se repitan con las Response)
  RandomVars = c(1, 2, 3, 5);
}
 
FixedBox = "false";
#Si FixedBox es TRUE, debes indicar las variables fijas
if(FixedBox == "true") {
#Modificar las variables fixed (que no se repitan con las Response)
  FixedVars = c(1, 2, 3, 5);
}
 
CVbox = "true";
#Si CVbox es TRUE, debes indicar el tipo de crossvalidation y parametros adicionales
if(CVbox == "true") {
   CVType = "Folds"
   nfolds = 5;
   TestingSize = 80
}

SVbox= "true"


###################################Cargar Archivos R########################################
.libPaths(normalizePath(libpath))
print(normalizePath(libpath))

setwd(normalizePath(path))

codeR = paste(mainpath, "/RCodes/Read_data.R", sep = "")
source(codeR)
codeR = paste(mainpath, "/RCodes/Recode.R", sep = "")
source(codeR)
codeR = paste(mainpath, "/RCodes/Impute.R", sep = "")
source(codeR)
#codeR = paste(mainpath, "/RCodes/TestF_lmer_uvcov1.R", sep = "")
#source(codeR)
codeR = paste(mainpath, "/RCodes/Models2V4.R", sep = "")
source(codeR)


datos <- try(ReadData(archivo))

Responses = c(2,3,4,5); ## que lea las variables respuesta desde java

errorRead <- ifelse(inherits(datos,'try-error'),99,0)
outputvalue = errorRead;

if(outputvalue == 99){
  outputvalue = datos;
  paste("Unexpected error reading your data set ", outputvalue);
} else {
  print("Datos cargados exitosamente")
}

gc()



###################################Cargar marcadores#######################################

path=original_path
setwd(normalizePath(path))
if(!file.exists('Output_BGLR')) dir.create('Output_BGLR')
path=paste(path, 'Output_BGLR', sep='/')
setwd(normalizePath(path))
if(!file.exists('Analysis1')) dir.create('Analysis1')
path=paste(path, outputFolder, sep='/')
setwd(normalizePath(path))
if(!file.exists('Info_Markers')) dir.create('Info_Markers')
pathM=paste(path, 'Info_Markers', sep='/')
setwd(normalizePath(pathM))

#En este punto netbeans pregunta si quieres sobreescribir los archivos
#if(!file.exists(outputFolder)) dir.create(outputFolder)



#Aqui cargamos el archivo de marcadores
Markers <- try(read.csv(MarkersDir,header=TRUE,check.names=FALSE,row.names=1,na.strings=c(NA,'.','','?_?','?.?','??'),stringsAsFactors=FALSE))
errorMark = inherits(Markers,'try-error')
if(errorMark == 1){
  print("Unexpected error reading the Markers");
} else {
  print("Markers matrix has been loaded succesfully");
  k = 0
  InfoMarkersFoder <- FALSE
  
  if(needRecodification) {
      k = 2
      ErrorMark <- 0
      InfoMarkersFoder <- TRUE
  
      NO_NA <- which(!is.na(Markers[,1]))
      Char_allele <- try(unlist(strsplit(Markers[NO_NA[1],1],split=character())))
      if(inherits(Char_allele,'try-error')) ErrorMark <- 1;
      if(ErrorMark == 1) {
        print("Unexpected error recoding the Markers")
        print(Char_allele)
      }
      
      separador <- ifelse(length(Char_allele)==3,Char_allele[2],'')
      out <- try(recode(Markers,separador))
      if(inherits(out,'try-error')) ErrorMark <- 1
      if(ErrorMark == 1) {
        print("Unexpected error recoding the Markers")
        print(out)
      }
      Markers <- out
      print("Recodification successful");

  }
  
  Markers <- as.matrix(Markers)
  print("Imputing markers")
  out <- try(Impute(Markers))
  errorMark = inherits(out,'try-error')
  if(errorMark == 1) {
    print("Unexpected error in Markers Quality")
    print(out)
  }
  
  Markers <- out$X
  messageMono <- out$messageMono
  MonoOMaf <- out$MonoOMaf
  rm(out)
  ncols=ifelse(ncol(Markers)>50, 50, ncol(Markers))
  nrows=ifelse(nrow(Markers)>50, 50, nrow(Markers))
  
  rowsMarkers = nrows;
  colnamesMarkers = colnames(Markers)[1:ncols]
  rowi=1
  for(i in rowi:nrows) {
    colnamesMarkers = as.character(Markers[rowi,1:ncols])
    paste("Colnamesmarkers ", colnamesMarkers)
  }
  
  
  rm(rowi,ncols,nrows)
  if(k==2 || MonoOMaf==1){
    actualOutputDir = getwd();
    print(actualOutputDir)
    print("Markers Quality process has finished")
    paste("Imputation done ", messageMono)
    
    #En este punto se crea un objecto arbol para desplegar folderes
    
  } else {
    print("Markers Quality process has finished")
    paste("Imputation done ", messageMono)
    
  }

}
path=original_path
setwd(normalizePath(path))
gc()

###################################Cargar relationships##########################


Pedigree <- try(read.csv(PedDir,header=TRUE,check.names=FALSE,row.names=1));

errorMark = inherits(Pedigree,'try-error')
if(errorMark==1){
  paste("Unexpected error reading the Pedigree", Pedigree)
} else {
  print("Pedigree matrix has been loaded succesfully")
  ncols=ifelse(ncol(Pedigree)>50, 50, ncol(Pedigree))
  nrows=ifelse(nrow(Pedigree)>50, 50, nrow(Pedigree))
} 

rm(ncols,nrows)

gc()

###################################Hacer analisis#################################

library(BGLR)
library(MASS)

codeR = paste(mainpath, "/RCodes/Models2V4.R", sep = "")
source(codeR)


setwd(normalizePath(path))
path=original_path
#if(!file.exists('Output_BGLR')) dir.create('Output_BGLR')
path <- paste(path,'Output_BGLR',sep='/')
setwd(normalizePath(path))
if(file.exists(outputFolder)) creadir <- 'no'
if(!file.exists(outputFolder)) creadir <- 'si'
path=paste(path, outputFolder,sep='/')
setwd(normalizePath(path))
if(!file.exists('Predict')) dir.create('Predict')
pathP=paste(path, 'Predict', sep='/')
setwd(normalizePath(pathP))

returnedValue = 0




#Realizar analisis para cada variable de Repuesta

for(i in 1:length(Responses)) {
  Response = Responses[i];
  yname=colnames(datos[Responses])[i]
  yvar = Response;
  datos[,yvar]=as.numeric(datos[,yvar])
  y=datos[,yvar]
  if(SVbox == "true")
  {
    y=(datos[,yvar]-mean(datos[,yvar],na.rm=TRUE))/sd(datos[,yvar],na.rm=TRUE)
  }
  
  
  #Realizar prediccion para la variable Respuesta en curso
  #IMPORTANTE gen es un vector que contiene el nombre de los genotipos
  
  if(AnalysesToDo == "Prediction"){
    savename=pathP
    t <- proc.time();
    #ETA <- try(Predict())
    ETA <- try(Models_lme4gs(PathP, datos, yy=y, gen=gen, RandomBox="false", FixedBox="false"))
    tPred <- proc.time() - t
  }
  
  #Despues del analisis de la variable Respuesta, ver si hay errores

  if(inherits(ETA,'try-error')){ returnederror <- ETA; returnedValue <- 99}

  message = returnedValue;
  if(message == 99 || message == 55){
    print("Analisis prematurely ended...")
    break;
  }
  # 
  #Checar si tambien hay que realizar crossvalidation
  #Solo se ejecutara el siguiente codigo si CVbox esta seleccionado
  if(CVbox == "true"){
    
    folderPredict <- "CV_Predict"
    path=original_path
    pathCV=paste0(original_path, outputFolder)
    if(!file.exists(folderPredict))  dir.create(folderPredict)
    typeCV = CVType;
    CVType <- try(CV_Predict(yy = y,datos=datos, gen=gen, typeCV, Markers=NULL, Pedigree=NULL, RandomBox="false", FixedBox="false", nfolds=10))

     if(CVType == "Folds"){
       
     nfolds <- nfolds;
     timeCV = round(tPred[3]*nfolds/60,2);
     }else if(CVtype.getSelectedItem() == "Load Sets"){
     timeCV = round(tPred[3]*length(unique(CVSets))/60,2);
     }else{
     TestSize = TestingSize;
     numberCV = NumberCV;
     timeCV = round(tPred[3]*numberCV/60,2)
     }

    CV_ERROR <- try(CV_Predict(yy = y,datos=datos, gen=gen, Markers=Markers, Pedigree=Pedigree, RandomBox="false", FixedBox="false", nfolds=10))
    
  }
  
  print(paste("Finishing analisys of variable", Response))
  
}

print("For ended....")
  
#Checar si hubo errores en algun paso anterior
outputvaluedir = getwd()
if(message == 99){
  #Hubo algun error en el analisis
  messageS = returnederror;
  ResponseError = yvar;
  paste("Unexpected error analyzing trait ",ResponseError, messageS)
} else if(message == 55) {
  #Se paro externamente el analisis
  print("Analisis was broken(stopped)");
} else if(message == 0){
  #Analisis exitoso     
  print("Successful analysis");
}

print(paste("Analisis stored in", outputvaluedir))



