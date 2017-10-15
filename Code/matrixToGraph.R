#Función para transformar una matriz normal a una de adyacencia y, posteriormente, a 
#un grafo

matrixToGraph <- function(madjacency){
  #library(Matrix)
  madj <- as(madjacency, "dgCMatrix")
  g <- graph.adjacency(madj, mode = "directed", weighted = TRUE)
  
  g
}