#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ
#DATE: 08/06/2016


thresholdGHeur3 <- function(graph, group = 1, quantileHeur = "25%", loops = 10, maAd = NULL, fixedThreshold = NULL){
  
  initialTime <- Sys.time()
  
  if(quantileHeur != 1 && quantileHeur != 2 && quantileHeur != 3 && quantileHeur != 4 && quantileHeur != 5 && quantileHeur != "0%" && quantileHeur != "25%" && quantileHeur != "50%" && quantileHeur != "75%" && quantileHeur != "100%"){
    stop("El valor del parametro quantileHeur debe ser un número del 1 al 5, o los valores 0%, 25%, 50%, 75% o 100%")
  }
  
  if(quantileHeur == 1 || quantileHeur == "0%"){
    cat("Entra en Greedy \n")
    greedy <- thresholdGreedy(graph, group, loops, maAd, fixedThreshold)
    
    finalTime <- Sys.time()
    differenceTime <- finalTime - initialTime
    greedy$executionTime <- differenceTime
    
    return(greedy)
  }
  
  gr <- rep(0, group)
  nvert <- vcount(graph)
  adopters <- diag(nrow = nvert)
  
  if(!is.null(maAd)){
    adj <- maAd
  } else{
    adj <- as_adjacency_matrix(graph, attr = "weight")
  }
  
  degreeGraph <- degree(graph, mode = "out")
  
  quantileDegree <- quantile(degreeGraph)
  
  quantileGr <- unname(quantileDegree[quantileHeur])
  
  degreeGrLogic <- degreeGraph >= quantileGr
  
  grLogic <- 1:nvert
  grLogic[!(grLogic %in% gr)] <- 0
  degreeGrLogic <- degreeGrLogic | grLogic
  
  nodesDegree <- 1:nvert
  nodesDegree <- nodesDegree * degreeGrLogic
  nodesDegree <- nodesDegree[nodesDegree != 0]
  lengthNodesDegree <- length(nodesDegree)
  #print(nodesDegree)
  #print(lengthNodesDegree)
  
  if(group >= lengthNodesDegree){
    cat("Devuelve greedy porque el grupo de nodesDegree es demasiado pequeño \n")
    greedy <- thresholdGreedy(graph, group, loops, maAd, fixedThreshold)
    
    finalTime <- Sys.time()
    differenceTime <- finalTime - initialTime
    greedy$executionTime <- differenceTime
    
    return(greedy)
  }
  
  col1Total <- matrix(rep(1, nvert), ncol = 1)
  
  for(i in 1:group){
    selected <- rep(0, lengthNodesDegree)
    positionSelected <- 1:lengthNodesDegree
    posGr <- match(gr, nodesDegree)
    selected[positionSelected %in% posGr] <- 0 - (nvert * nvert)
    
    #set.seed(112358)
    set.seed(16807)
    timeTotal <- rep(0, lengthNodesDegree)
    nadopters <- rep(0, lengthNodesDegree)
    
    for(k in 1:loops){
      time <- rep(0, lengthNodesDegree)
      timeStmp <- 0 
      adopt <- adopters[nodesDegree,]
      #print(adopt)
      
      if(!is.null(fixedThreshold)){
        threshold <- fixedThreshold
      } else{
        threshold <- runif(nvert)
      }
      
      col1 <- matrix(rep(1, lengthNodesDegree), ncol = 1)
      thresholdmatrix <- col1%*%threshold
      
      while(sum(time != 0) < lengthNodesDegree){
        mult <- adopt%*%adj
        influenced <- (mult > thresholdmatrix)
        
        aux <- apply((adopt | influenced), 2, as.numeric)
        
        changePerNode <- adopt - aux
        unchanged <- ((changePerNode %*% col1Total) == 0)
        
        if(timeStmp == 0){
          time <- time + ((unchanged * (time == 0)) * -1)
        } else{
          time <- time + ((unchanged * (time == 0)) * timeStmp)
          
        }
        adopt <- aux
        timeStmp <- timeStmp + 1
      }
      
      nadopters <- nadopters + (apply(adopt, 1, sum))
      #print(nadopters)
      timeTotal <- timeTotal + time
    }
    
    nadopters <- nadopters / loops
    #print(nadopters)
    timeTotal <- timeTotal / loops
    
    opt <- (nvert * nadopters) - timeTotal + selected
    
    tie <- opt == max(opt)
    if(sum(tie) > 1){
      opt <- (opt * tie) + degree(graph, v = nodesDegree, mode = "out")
    }
    
    optimal <- which.max(opt)
    
    gr[i] <- nodesDegree[optimal]
    adopters[,nodesDegree[optimal]] <- 1
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
