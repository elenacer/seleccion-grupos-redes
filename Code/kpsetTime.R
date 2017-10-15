#Método para añadir el tiempo transcurrido en la ejecución de la función kpset

kpsetTime <- function(madjacency, nodes, typeCentrality, methodCent = "min", modeDegree = "total"){
  initialTime <- Sys.time()
  
  returnVal <- kpset(adj.matrix = madjacency, size = nodes, type = typeCentrality, method = methodCent, cmode = modeDegree)
  
  finalTime <- Sys.time()
  differenceTime <- finalTime - initialTime
  print(differenceTime)
  
  returnVal
}