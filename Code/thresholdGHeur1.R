#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ
#DATE: 08/06/2016


thresholdGHeur1 <- function(graph, group = 1, loops = 10, maAd = NULL, fixedThreshold = NULL){
      initialTime <- Sys.time()
      
      gr <- rep(0, group)
      nvert <- vcount(graph)
      adopters <- diag(nrow = nvert)
      
      if(!is.null(maAd)){
            adj <- maAd
      } else{
            adj <- as_adjacency_matrix(graph, attr = "weight")
      }
      
      col1Total <- matrix(rep(1, nvert), ncol = 1)
      
      for(i in 1:group){
            noNeighborsGroup <- 1:nvert
            
            if(i > 1){
                  #print(gr[1:i])
                  neighborsGroup <- unlist(sapply(gr[1:(i - 1)], function(x) neighbors(graph, x)))
                  neighborsGroup[neighborsGroup %in% gr] <- NA
                  neighborsGroup <- unique(neighborsGroup[!is.na(neighborsGroup)])
                  
                  noNeighborsGroup[noNeighborsGroup %in% neighborsGroup] <- NA
                  noNeighborsGroup <- noNeighborsGroup[!is.na(noNeighborsGroup)]
                  #print(noNeighborsGroup)
                  
                  if(length(noNeighborsGroup) == (i - 1)){
                        cat("Todos son vecinos")
                        noNeighborsGroup <- 1:nvert
                  }
            }
            
            lengthNoNeig <- length(noNeighborsGroup)
            
            selected <- rep(0, lengthNoNeig)
            positionSelected <- 1:lengthNoNeig
            posGr <- match(gr, noNeighborsGroup)
            selected[positionSelected %in% posGr] <- 0 - (nvert * nvert)
            
            #set.seed(112358)
            set.seed(16807)
            timeTotal <- rep(0, lengthNoNeig)
            nadopters <- rep(0, lengthNoNeig)
            
            for(k in 1:loops){
                  time <- rep(0, lengthNoNeig)
                  timeStmp <- 0 
                  adopt <- adopters[noNeighborsGroup,]
                  #print(adopt)
                  
                  if(!is.null(fixedThreshold)){
                        threshold <- fixedThreshold
                  } else{
                        threshold <- runif(nvert)
                  }
                  
                  col1 <- matrix(rep(1, lengthNoNeig), ncol = 1)
                  thresholdmatrix <- col1%*%threshold
                  
                  while(sum(time != 0) < lengthNoNeig){
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
            
            tie <- opt == max(opt)
            if(sum(tie) > 1){
                  opt <- (opt * tie) + degree(graph, v = noNeighborsGroup, mode = "out")
            }
            
            optimal <- which.max(opt)
            
            gr[i] <- noNeighborsGroup[optimal]
            adopters[,noNeighborsGroup[optimal]] <- 1
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