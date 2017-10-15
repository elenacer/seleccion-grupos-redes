#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ
#DATE: 08/06/2016


thresholdGSHeur1 <- function(graph, group = 1, loops = 10, maAd = NULL, fixedThreshold = NULL, modeGreedy = 1){
  initialTime <- Sys.time()
  
  if(modeGreedy == 2){
    greedy <- thresholdGHeur1(graph, group, loops, maAd, fixedThreshold)
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
      
      neighborsGroup <- unlist(sapply(initialGroup[1:(lengthGroup-1)], function(x) neighbors(graph, x)))
      neighborsGroup[neighborsGroup %in% initialGroup] <- NA
      neighborsGroup <- unique(neighborsGroup[!is.na(neighborsGroup)])
      
      adopters[col(adopters) %in% initialGroup] <- 1
      
      noNeighborsGroup <- 1:nvert
      noNeighborsGroup[noNeighborsGroup %in% neighborsGroup] <- NA
      noNeighborsGroup <- noNeighborsGroup[!is.na(noNeighborsGroup)]
      
      #print(length(noNeighborsGroup))
      #print(length(initialGroup))
      
      if((length(noNeighborsGroup) - length(initialGroup)) >= 0){
        adopters <- adopters[noNeighborsGroup,]
        
        #print(neighborsGroup)
        #print(noNeighborsGroup)
        #print(adopters)
        posInitialGr <- match(initialGroup, noNeighborsGroup)
        
        selected <- rep(0, nvert - length(neighborsGroup))
        positionSelected <- 1:(nvert- length(neighborsGroup))
        selected[positionSelected %in% posInitialGr] <- 0 - (nvert * nvert)
        
        #set.seed(112358)
        set.seed(16807)
        timeTotal <- rep(0, nvert - length(neighborsGroup))
        nadopters <- rep(0, nvert - length(neighborsGroup))
        
        for(k in 1:loops){
          time <- rep(0, nvert - length(neighborsGroup))
          timeStmp <- 0 
          adopt <- adopters
          #print(adopt)
          
          if(!is.null(fixedThreshold)){
            threshold <- fixedThreshold
          } else{
            threshold <- runif(nvert)
          }
          
          col1 <- matrix(rep(1, (nvert - length(neighborsGroup))), ncol = 1)
          thresholdmatrix <- col1%*%threshold
          #print(thresholdmatrix)
          
          while(sum(time != 0) < (nvert - length(neighborsGroup))){
            mult <- adopt%*%adj
            #print(mult)
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
            #print(time)
          }
          
          nadopters <- nadopters + (apply(adopt, 1, sum))
          timeTotal <- timeTotal + time
          
        }
        
        nadopters <- nadopters / loops
        timeTotal <- timeTotal / loops
        
        #print(nadopters)
        #print(timeTotal)
        #print(selected)
        opt <- (nvert * nadopters) - timeTotal + selected
        
        tie <- opt == max(opt)
        if(sum(tie) > 1){
          #print(opt)
          #print(tie)
          #print(degree(graph, v = noNeighborsGroup, mode = "out"))
          opt <- (opt * tie) + degree(graph, v = noNeighborsGroup, mode = "out")
        }
        
        optimal <- which.max(opt)
        optimalValues[i] <- max(opt)
        
        
        optimalAdopters[i] <- nadopters[optimal]
        optimalTime[i] <- timeTotal[optimal]
        
        #print(nadopters[optimal])
        #print(timeTotal[optimal])
        
        initialGroup[length(gr)] <- noNeighborsGroup[optimal]
        matrixComb[i,] <- initialGroup
      } else{
        optimalValues[i] <- 0 - (nvert * nvert)
        
        
        optimalAdopters[i] <- group
        optimalTime[i] <- 0
        
        matrixComb[i,] <- gr
      }
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