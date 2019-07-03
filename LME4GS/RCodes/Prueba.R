gc(reset=TRUE)

libpath = "C:/Users/NBAUTISTA/Documents/NetBeansProjects/BASE/win-library/3.5"
.libPaths(normalizePath(libpath))


codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/lmer_uvcov_mod.R", sep = "")
source(codeR)

codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/predict.R", sep = "")
source(codeR)

codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/relfac.R", sep = "")
source(codeR)

codeR = paste("C:/Users/NBAUTISTA/Documents/NetBeansProjects/BGLR1/RCodes/theta_optim.R", sep = "")
source(codeR)

library(BGLR)
library(lme4)

test_lme4gs <- function() {
  
  #Example 1, wheat 
  data(wheat)
  X1<-wheat.X
  Z1<-scale(X1,center=TRUE,scale=TRUE)
  G1<-tcrossprod(Z1)/ncol(Z1)
  A1<-wheat.A
  rownames(G1)<-colnames(G1)<-rownames(A1)
  y1<-wheat.Y[,1]
  
  #id a vector with ids
  random1<-list(mrk=list(K=G1,id=rownames(G1)),
                ped=list(K=A1,id=rownames(A1)))
  
  #La funcion la renombre como lmer_uvcov_beta, es la funcion corregida
  out<-lmer_uvcov_beta(y1,fixed="1",random=random1)
  summary(out)
  
  plot(y1,predict(out))
}


test_lme4gs()
