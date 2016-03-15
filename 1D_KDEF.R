#!/usr/bin/env Rscript

NormalKernalF=function(u){
  
  return( (1/sqrt(2*pi)) * exp(-u^2/2) )
  
}

OneKDEF=function(DataXV,BandWidth){
  
  Range=max(DataXV)-min(DataXV)
  ObsXV=seq( min(DataXV)-0.1*Range , max(DataXV)+0.1*Range , 0.3094)
  n=length(DataXV)
  EstiDenV=vector()
  
  for ( i in 1:length(ObsXV) ){
    EstiDenV[i]=sum( NormalKernalF( (ObsXV[i]-DataXV)/BandWidth ) )/ n*BandWidth
  }
  return(list(x=ObsXV,y=EstiDenV))
}


DataXV=rchisq(10000,1)

kde=OneKDEF(DataXV,1)
hist(DataXV)
lines(x=kde[[1]],y=kde[[2]]*length(DataXV),col="red")
