
## Given a sample from data distribution
x = rgamma(1,2,1)

## 
M = 10000

t = 1
samplex = vector()
samplex[1] = x

## Metropolis Hasting
repeat{
  
  q = rexp(1,0.2)
  
  c = dexp(x,0.2)/dexp(q,0.2)
  
  alpha = min(1, dgamma(q,2,1)/dgamma(x,2,1) * c)

  u = runif(1,0,1)  
  
  t = t + 1
  if ( u<=alpha ){
    
    samplex[t] = q
    
  }else{
    
    samplex[t] = samplex[(t-1)]
    
  }
  
  if ( length(samplex) == M ){
    
    break
  }
  
}

plot(density(rgamma(10000,2,1)))
lines(density(samplex),col="red")
