#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ
#DATE: 08/06/2016


thresholdGSHeur3 <- function(graph, group = 1, quantileHeur = "25%",loops = 10, maAd = NULL, fixedThreshold = NULL, modeGreedy = 1){
  initialTime <- Sys.time()
  
  if(quantileHeur != 1 && quantileHeur != 2 && quantileHeur != 3 && quantileHeur != 4 && quantileHeur != 5 && quantileHeur != "0%" && quantileHeur != "25%" && quantileHeur != "50%" && quantileHeur != "75%" && quantileHeur != "100%"){
    stop("El valor del parametro quantileHeur debe ser un n?mero del 1 al 5, o los valores 0%, 25%, 50%, 75% o 100%")
  }
  
  if(quantileHeur == 1 || quantileHeur == "0%"){
    cat("Entra en Stepwise \n")
    greedyStepwise <- thresholdGreedyStepwise(graph, group, loops, maAd, fixedThreshold)
    
    finalTime <- Sys.time()
    differenceTime <- finalTime - initialTime
    greedyStepwise$executionTime <- differenceTime
    
    return(greedyStepwise)
  }
  
  if(modeGreedy == 2){
    greedy <- thresholdGHeur3(graph, group, quantileHeur, loops, maAd, fixedThreshold)
  } else{
    greedy <- thresholdGreedy(graph, group, loops, maAd, fixedThreshold)
  }
  
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
  col1Total <- matrix(rep(1, nvert), ncol = 1)
  
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
    cat("Devuelve greedy porque el grupo de nodesDegree es demasiado peque?o \n")
    finalTime <- Sys.time()
    differenceTime <- finalTime - initialTime
    greedy$executionTime <- differenceTime
    
    return(greedy)
  }
  
  positionSelected <- 1:lengthNodesDegree
  j <- 0
  lastOpt <- greedy$numAdopters*nvert - greedy$totalTime
  newOpt <- 0
  
  while((all.equal(sort(gr), sort(grCompare)) != TRUE) && (j < (group + 1) || ((1 - lastOpt/newOpt) > 0.001))){
    grCompare <- gr
    if(group > 2){
      grWithoutLast <- gr[1:(lengthGroup-1)]
      
      matrixComb <- combinations((lengthGroup-1), (lengthGroup-2), grWithoutLast)
      matrixComb <- cbind(matrixComb, rep(gr[lengthGroup], nrow(matrixComb)))
      matrixComb <- cbind(matrixComb, rep(0, nrow(matrixComb)))
    } else{
      matrixComb <- matrix(c(gr[lengthGroup], 0), nrow = 1)
    }
    
    optimalValues <- rep(0, nrow(matrixComb))
    optimalAdopters <- optimalValues
    optimalTime <- optimalValues
    
    for(i in 1:nrow(matrixComb)){
      adopters <- diag(nrow = nvert)
      initialGroup <- matrixComb[i,]
      
      posInitialGr <- match(initialGroup, nodesDegree)
      
      selected <- rep(0, lengthNodesDegree)
      selected[positionSelected %in% posInitialGr] <- 0 - (nvert * nvert)
      
      adopters[col(adopters) %in% initialGroup] <- 1
      adopters <- adopters[nodesDegree,]
      
      #set.seed(112358)
      set.seed(16807)
      timeTotal <- rep(0, lengthNodesDegree)
      nadopters <- rep(0, lengthNodesDegree)
      
      for(k in 1:loops){
        time <- rep(0, lengthNodesDegree)
        timeStmp <- 0 
        adopt <- adopters
        
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
        timeTotal <- timeTotal + time
      }
      
      nadopters <- nadopters / loops
      timeTotal <- timeTotal / loops
      
      opt <- (nvert * nadopters) - timeTotal + selected
      
      optimalValues[i] <- max(opt)
      
      tie <- opt == max(opt)
      if(sum(tie) > 1){
        opt <- (opt * tie) + degree(graph, v = nodesDegree, mode = "out")
      }
      
      optimal <- which.max(opt)
      
      optimalAdopters[i] <- nadopters[optimal]
      optimalTime[i] <- timeTotal[optimal]
      
      print(nadopters[optimal])
      print(timeTotal[optimal])
      
      initialGroup[length(gr)] <- nodesDegree[optimal]
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