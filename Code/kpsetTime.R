#M�todo para a�adir el tiempo transcurrido en la ejecuci�n de la funci�n kpset

kpsetTime <- function(madjacency, nodes, typeCentrality, methodCent = "min", modeDegree = "total"){
  initialTime <- Sys.time()
  
  returnVal <- kpset(adj.matrix = madjacency, size = nodes, type = typeCentrality, method = methodCent, cmode = modeDegree)
  
  finalTime <- Sys.time()
  differenceTime <- finalTime - initialTime
  print(differenceTime)
  
  returnVal
}