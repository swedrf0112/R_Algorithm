## 2D Convex Hull
## Andrew's Monotone Chain

library(dplyr)
library(magrittr)

CheckClockwiseF=function(PO,PA,PB){
  
  MatDet=det(matrix(c(PO,1,PA,1,PB,1),nrow=3,byrow=T))
  ifelse(MatDet>0,return(TRUE),return(FALSE))
  
}

ConvexHullF=function(InputDataD){

  names(InputDataD)=c("x","y")
  InputDataD %<>% arrange(x,y)

  LPointM=matrix({0},nrow=1,ncol=2)
  UPointM=matrix({0},nrow=1,ncol=2)

  i=0
  l=0
  u=0

  repeat{
  
      i=i+1
    
      while( l >= 2 ){
    
             if ( CheckClockwiseF(LPointM[(l-1),],LPointM[l,],InputDataD[i,]) == FALSE ){
      
                  LPointM=LPointM[-l,]
                  l=l-1
      
            }else{
      
                  break
              
            }
    
       }
  
       while( u >= 2 ){
    
              if ( CheckClockwiseF(UPointM[(u-1),],UPointM[u,],InputDataD[i,]) == TRUE ){
      
                   UPointM=UPointM[-u,]
                   u=u-1
                   
              }else{
                 
                   break
              }
    
      }
  
    if ( i == 1 ){
    
        LPointM[1,]=unlist(InputDataD[1,])
        UPointM[1,]=unlist(InputDataD[1,])
    
    }else{
    
        LPointM=rbind(LPointM,unlist(InputDataD[i,]))
        UPointM=rbind(UPointM,unlist(InputDataD[i,]))
    }
  

    l=l+1
    u=u+1
    
    if ( i==nrow(InputDataD) ){
    
         break
    
     }
  
    }

  LowCHPointD=data.frame(LPointM,row.names=NULL)
  UpCHPointD=data.frame(UPointM,row.names=NULL) %>% slice(1:(nrow(.)-1)) %>% arrange(desc(x))
  CHPointD=rbind(LowCHPointD,UpCHPointD)
  
  return(CHPointD)
  
}


#### Testing

TestD=data.frame(x=rnorm(1000,-0.5,0.5),y=rnorm(1000,-0.3,0.3))
g=ConvexHullF(TestD)
plot(x=TestD$x,y=TestD$y,xlim=c(-3,3),ylim=c(-2,2))
lines(x=g$x,y=g$y,col="red",lwd=5)
points(x=g$x,y=g$y,col="red",pch=19,cex=1.5)



