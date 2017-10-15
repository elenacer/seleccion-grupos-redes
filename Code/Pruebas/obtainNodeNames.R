obtainNodeNames <-function(vResults, graphOptimized,numOptNodes = 10, highest=TRUE){

  if(highest){
    vOptsPos <- (length(vResults)-numOptNodes+1):length(vResults)
  }else{
    vOptsPos <- 1:numOptNodes
  }
  
  vSorted <-sort(vResults)
  
  vMatch<-match(vResults, vSorted[vOptsPos], 0)
  
  vMatch<-(vMatch>0)
  
  if(is.named(graphOptimized)){
    vNames<-as_ids(V(graphOptimized))
  } else{
    vNames <- 1:vcount(graphOptimized)
  }
  
  return(vNames[vMatch])
}
