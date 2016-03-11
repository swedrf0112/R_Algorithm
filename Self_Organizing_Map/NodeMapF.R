NodeMapF=function(NL,NR,NU,ND){
  
  CenX=NL+(NR-NL)/2
  CenY=NU+(ND-NU)/2
  NodeMapM=cbind(id=(1:(length(CenX)*length(CenY))),expand.grid(x=CenX,y=CenY))
  
  return(NodeMapM)
  
}
