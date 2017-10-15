#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ & ALBERTO IBARRONDO LUIS
#DATE: 08/06/2016


thresholdGSHeur2 <- function(graph, group = 2, fixedGroup = 1, loops = 10, maAd = NULL, fixedThreshold = NULL){
  initialTime <- Sys.time()
  
  if(fixedGroup >= group){
    stop("El número de nodos fijos debe ser menor que el número de nodos del grupo óptimo")  
  }
  
  greedy <- thresholdGreedy(graph, group, loops, maAd, fixedThreshold)
  
  if(is.named(graph)){
    gr <- match(greedy$group, V(graph)$name)
  } else{
    gr <- greedy$group
  }
  
  if(group == 1){
    finalTime <- Sys.time()
    differenceTime <- finalTime - initialTime
    greedy$executionTime <- differenceTime
    
    return(greedy)
  }
  
  grCompare <- rep(0, group)
  lengthGroup <- length(gr)
  nvert <- vcount(graph)
  
  if(!is.null(maAd)){
    adj <- maAd
  } else{
    adj <- as_adjacency_matrix(graph, attr = "weight")
  } 
  
  positionSelected <- 1:nvert
  j <- 0
  lastOpt <- greedy$numAdopters*nvert - greedy$totalTime
  newOpt <- 0
  
  fixedGr <- gr[(lengthGroup + 1 - fixedGroup):lengthGroup]
  #print(fixedGr)
  
  while((all.equal(sort(gr), sort(grCompare)) != TRUE) && (j < (group + 1) || ((1 - lastOpt/newOpt) > 0.001))){
    grCompare <- gr
    if(group > 2 && (group > (fixedGroup + 1))){
      variableGr <- gr[!(gr %in% fixedGr)]
      
      matrixComb <- combinations(length(variableGr), (length(variableGr)-1), variableGr)
      colum1 <- matrix(rep(1, nrow(matrixComb)), ncol = 1)
      mfixGr <- colum1 %*% fixedGr
      matrixComb <- cbind(matrixComb, mfixGr)
      matrixComb <- cbind(matrixComb, rep(0, nrow(matrixComb)))
    } else{
      matrixComb <- matrix(c(fixedGr, 0), nrow = 1)
    }
    
    optimalValues <- rep(0, nrow(matrixComb))
    optimalAdopters <- optimalValues
    optimalTime <- optimalValues
    
    for(i in 1:nrow(matrixComb)){
      adopters <- diag(nrow = nvert)
      selected <- rep(0, nvert)
      initialGroup <- matrixComb[i,]
      #print(initialGroup)
      
      adopters[col(adopters) %in% initialGroup] <- 1
      selected[positionSelected %in% initialGroup] <- 0 - (nvert * nvert)
      
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
      
      optimalValues[i] <- max(opt)
      
      tie <- opt == max(opt)
      if(sum(tie) > 1){
        opt <- (opt * tie) + degree(graph, mode = "out")
      }
      
      optimal <- which.max(opt)
      
      optimalAdopters[i] <- nadopters[optimal]
      optimalTime[i] <- timeTotal[optimal]
      
      print(nadopters[optimal])
      print(timeTotal[optimal])
      
      initialGroup[length(gr)] <- optimal
      #print(initialGroup)
      matrixComb[i,] <- initialGroup
    }
    
    posOptVal <- which.max(optimalValues)
    
    gr <- matrixComb[posOptVal,]
    print(gr)
    
    lastOpt <- newOpt
    newOpt <- max(optimalValues)
    j <- j + 1
  }
  
  finalTime <- Sys.time()
  differenceTime <- finalTime - initialTime
  #print(differenceTime)
  
  if(is.named(graph)){
    gr <- V(graph)$name[gr]
  } 
  
  if(optimalTime[posOptVal] == -1){
    optimalTime[posOptVal] <- 0
  }
  
  list(group = gr, executionTime = differenceTime, numVertex = nvert, numAdopters = optimalAdopters[posOptVal], totalTime = optimalTime[posOptVal])
}