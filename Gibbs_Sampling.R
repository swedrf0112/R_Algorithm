
pxy = matrix(c(0.5,0.2,0.1,0.2),byrow=T,2,2)


x = pxy
side = 1

condPMF = function(x,side=1){
  
  marPV = apply(x,side,function(x) sum(x))
  
  if( side == 1 ){
    
    return( x/marPV )
    
  }else{
    
    return( t(t(x)/marPV) )
    
  }
  
  
}

##  P(Y|X)
# condPMF(pxy,1) 
##  P(X|Y)
# condPMF(pxy,2)

x = c(0,0)
m = 100000
samplem = matrix({0},ncol=2,nrow=m)
samplem[1,]=x
count=1

repeat{

  if ( count >= m ){
    
    break
    
  }
  
  for ( i in 1:2 ){
    
      count = count + 1
 
      if ( i == 1 ){
        
        condv = condPMF(pxy,2)[,x[2]+1]
        x = c( sample(size=1,x=c(0,1),prob=condv) , x[2] ) 
      
        
      }else{
        
        condv = condPMF(pxy,1)[x[1]+1,]
        x = c( x[1] , sample(size=1,x=c(0,1),prob=condv) )
        
      }
      
      samplem[count,]=x
    
  }
  
}


table(paste0(samplem[,1],samplem[,2]))/m






