recodeSNP<-function(x,i,separa=separa)
{
	N<-length(x)
	sumNA<-sum(is.na(x))
 
	freqNA<-sumNA/N
  
	if(sumNA==N){

		x<-rep(NA,N)

		minorAllele<-NA

		majorAllele<-NA

		minorAlleleFreq<-NA
	
		freqHetero<-NA


	}else{

		tmp<-unlist(strsplit(x=x,split=character()))

		if(sumNA>0){

			tmp<-tmp[-which(is.na(tmp))]

		}

		tmp<-tmp[which(tmp!=separa)]

		alleleCount<-table(tmp)

		if(length(alleleCount)==1){

			minorAllele<-NA
		
			majorAllele<-names(alleleCount)

			minorAlleleFreq<-0

			freqHetero<-0
		
			x<-ifelse(is.na(x),NA,0)
		}
   
	
		if(length(alleleCount)==2){
	
			alleleCount<-alleleCount[order(alleleCount)]   
		
			minorAllele<-names(alleleCount)[1]
		
			majorAllele<-names(alleleCount)[2]
		
			minorAlleleFreq<-alleleCount[1]/sum(alleleCount)

		
			homo1<-paste(minorAllele,separa,minorAllele,sep='')
		
			homo2<-paste(majorAllele,separa,majorAllele,sep='')
		
			x<-as.integer(ifelse(is.na(x),NA,ifelse(x==homo1,2,ifelse(x==homo2,0,1))))
		
			freqHetero<-mean(x==1,na.rm=TRUE)

		}
	
	
		if(length(alleleCount)>2){
		
			stop(paste('Marker ',i,' has more than two alleles',sep=''))

		}

	}
   
	out<-list(minorAllele=minorAllele,majorAllele=majorAllele,
		minorAlleleFreq=minorAlleleFreq,freqNA=freqNA,
       
		x=x,freqHetero=freqHetero)

	return(out)

}




recode=function(Markers,separa="_")
{

	X<-matrix(NA,nrow=nrow(Markers),ncol=ncol(Markers))

	colnames(X)<-colnames(Markers)

	minorAllele<-character()

	majorAllele<-character()

	minorAlleleFreq<-numeric()
   
	freqNA<-numeric()

	freqHetero<-numeric()

	chromosome<-numeric()

	bp<-numeric()

	for(i in 1:ncol(X))
{


		x <- Markers[,i]
		tmp <- try(recodeSNP(x=x,i=i,separa=separa)
)

		if(inherits(tmp,'try-error')) stop(tmp[1])
	
		minorAllele[i]<-tmp$minorAllele
	
		if(length(tmp$minorAllele)!=1) stop(i)

		majorAllele[i]<-tmp$majorAllele

		minorAlleleFreq[i]<-tmp$minorAlleleFreq

		freqNA[i]<-tmp$freqNA

		freqHetero[i]<-tmp$freqHetero
	
	X[,i]<-tmp$x
	
		print(i)

	}
	RecodeInfo <- cbind(Marker=colnames(Markers),minorAllele=minorAllele,
majorAllele=majorAllele,
		minorAlleleFreq=minorAlleleFreq,freqNA=freqNA,freqHetero=freqHetero)

	write.table(RecodeInfo,"Recode_Markers_Info.csv",sep=",",row.names=FALSE)
	return(X=X)	           	
	#return(list(X=X,minorAllele=minorAllele,
	           
	#	majorAllele=majorAllele,minorAlleleFreq=minorAlleleFreq,

	#	freqNA=freqNA,freqHetero=freqHetero))

}
