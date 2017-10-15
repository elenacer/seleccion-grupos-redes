#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ
#DATE: 08/06/2016


#Modelo General de Umbral - Greedy simple

thresholdGreedy <- function(graph, group = 1, loops = 10, maAd = NULL, fixedThreshold = NULL){
  initialTime <- Sys.time()
  
  gr <- rep(0, group)
  nvert <- vcount(graph)
  adopters <- diag(nrow = nvert)
  
  if(!is.null(maAd)){
    adj <- maAd
  } else{
    adj <- as_adjacency_matrix(graph, attr = "weight")
  }
  
  selected <- rep(0, nvert)
  
  for(i in 1:group){
      #set.seed(112358)
      set.seed(16807)
      timeTotal <- rep(0, nvert)
      nadopters <- rep(0, nvert)
      
      for(k in 1:loops){
        time <- rep(0, nvert)
        timeStmp <- 0 
        adopt <- adopters
        
        if(!is.null(fixedThreshold)){
          threshold <- fixedThreshold
        } else{
          threshold <- runif(nvert)
        }
        
        col1 <- matrix(rep(1, nvert), ncol = 1)
        thresholdmatrix <- col1%*%threshold
        
        while(sum(time != 0) < nvert){
          mult <- adopt%*%adj
          influenced <- (mult > thresholdmatrix)
          
          aux <- apply((adopt | influenced), 2, as.numeric)
          
          changePerNode <- adopt - aux
          unchanged <- ((changePerNode %*% col1) == 0)
          
          if(timeStmp == 0){
            time <- time + ((unchanged * (time == 0)) * -1)
          } else{
            time <- time + ((unchanged * (time == 0)) * timeStmp)
            
          }
          adopt <- aux
          timeStmp <- timeStmp + 1
        }
        
        nadopters <- nadopters + (apply(adopt, 1, sum))
        timeTotal <- timeTotal + time
      }
        
    nadopters <- nadopters / loops
    timeTotal <- timeTotal / loops
    
    opt <- (nvert * nadopters) - timeTotal + selected
    
    tie <- opt == max(opt)
    if(sum(tie) > 1){
      opt <- (opt * tie) + degree(graph, mode = "out")
    }
    
    optimal <- which.max(opt)
    
    gr[i] <- optimal
    adopters[,optimal] <- 1
    selected[optimal] <- 0 - (nvert * nvert)
  }
  
  finalTime <- Sys.time()
  differenceTime <- finalTime - initialTime
  
  if(is.named(graph)){
    gr <- V(graph)$name[gr]
  } 
  
  if(timeTotal[optimal] == -1){
    timeTotal[optimal] <- 0
  }
  
  list(group = gr, executionTime = differenceTime, numVertex = nvert, numAdopters = nadopters[optimal], totalTime = timeTotal[optimal])
}