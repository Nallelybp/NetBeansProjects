
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




test_lme4gs<- function(savename_param, #Ruta donde se guardara el archivo csv 
                       yvar_param, #Nombre del archivo csv
                       datos_param, # Dataframe de datos
                       Markers_param, # Dataframe de marcadores
                       Pedigree_param){ #Dataframe de pedigree
  
    X<-Markers_param
    Z<-scale(X,center=TRUE,scale=TRUE)
    
    
    G<-tcrossprod(Z)/ncol(Z) #En este punto G es una matrix no un dataframe
    A<- as.matrix(Pedigree_param) #En este punto A es una matrix no un dataframe
    
    y<-datos_param[,2]
    
    #gen debe ser un vector de Ids de tipo caracter, obligatoriamente
    gen <- as.character(datos_param[,1])


    
    
    #random siempre debe de seguir este patron, es una lista
    #Cada elemento de esta lista, es otra lista con los elementos K y id.
    #K debe ser una matrix cuadrada (no un dataframe)
    #id debe ser un vector de tipo character. Este vector indica el nombre de las 
    #rows y columns de su matrix correspondiente K.
    
    
    rownames(G)<-colnames(G)<-gen  #Esta linea es para poder indexar G correctamente
    rownames(A)<-colnames(A)<-gen  #Esta linea es para poder indexar A correctamente
    
    print("G debe ser de tipo matrix y cuadrada")
    print(class(G))
    print(dim(G))
    print("A debe ser de tipo matrix y cuadrada")
    print(class(A))
    print(dim(A))
    
    print("Los rows y cols de G deben ser igual a gen")
    print(all(rownames(G) == gen) & all(rownames(G) == gen))
    print("Los rows y cols de A deben ser igual a gen")
    print(all(rownames(A) == gen) & all(rownames(A) == gen))
    
    
    random<-list(mrk=list(K=G,id=gen), 
                 ped=list(K=A,id=gen))


    out<-lmer_uvcov_beta(y,fixed="1",random=random)
    summary(out)
    pdf(file="scatterplot")
    plot(y,predict(out))
    dev.off()
    a = data.frame(VarCorr(out,comp="Variance"))
    write.table(a,paste(savename_param,"var_comp.csv",sep=""),sep=",",row.names=FALSE)
    y2 = predict(out)
    write.table(y2,paste(savename_param,"_",yvar_param,"_pred.csv",sep=""),sep=",",row.names=FALSE)
  
}





#cargar y, markers y pedgree

datos = read.csv("C:/BGLR/Examples/Wheat_GrainYield.csv")
Markers = read.csv("C:/BGLR/Examples/Wheat_Markers.csv")
Pedigree=read.csv("C:/BGLR/Examples/Wheat_Pedigree.csv", row.names = 1, header = TRUE)

MarkerBox="true"
PedigreeBox="true"
savename = "./"
yvar = "var1"


out = test_lme4gs(savename_param = savename, 
                    yvar_param = yvar, 
                    datos_param = datos, 
                    Markers_param = Markers, 
                    Pedigree_param = Pedigree)



