#Librerías necesarias para ejecutar las pruebas
library(igraph)
library(igraphdata)
library(keyplayer)
library(gtools)
library(Matrix)

#Archivos .R donde están almacenados todos los códigos desarrollados para las diferentes pruebas del 
#modelo de difusión de innovaciones basado en el umbral lineal
source("matrixToGraph.R")
source("kpsetTime.R")
source("threshold.R")
source("carga.R")

#Creación de los 4 grafos reales sobre los que se probará el modelo de difusión. Entre ellos se 
#encuentran: dos grafos de amistades, gFriends y gFaculty, un grafo sobre libros de política, gBooks 
#y un grafo sobre relaciones entre delfines, gDolphins.

#SE INVIERTEN LOS VALORES DE LAS ARISTAS: ARISTA <- (1- ARISTA); El objetivo es que los que
# mas convencen tengan menos coste (ya que las medidas de centralidad van por coste)

#AMIGOS
datafr <- read.table("amigos.txt", header = TRUE)
datafr$weight<- (1-datafr$weight)
gFriendsInd <- graph.data.frame(datafr, directed = TRUE)
#LIBROS
datafr <- read.table("libros.txt", header = TRUE)
datafr$weight<- (1-datafr$weight)
gBooksInd <- graph.data.frame(datafr, directed = TRUE)
#DELFINES
datafr <- read.table("delfines.txt", header = TRUE)
datafr$weight<- (1-datafr$weight)
gDolphinsInd <- graph.data.frame(datafr, directed = TRUE)
#FACULTY
data("UKfaculty")
madj <- (as_adjacency_matrix(UKfaculty, attr = "weight"))*6/100
madj[madj>0] <- (1- madj[madj>0])
gFacultyInd <- graph.adjacency(madj, mode = "directed", weighted = TRUE)


#EJECUTAR PASO A PASO
betwG <- betweenness(gFacultyInd)
closeG <- closeness(gFacultyInd, mode = "out")
degrG <- degree(gFacultyInd, mode = "out")
evceG <- evcent(gFacultyInd, directed = TRUE)$vector
pagRG <- page.rank(gFacultyInd)$vector

betwGroup <- obtainNodeNames(betwG, gFacultyInd, numOptNodes = 4)
closeGroup <- obtainNodeNames(closeG, gFacultyInd, numOptNodes = 4)
degrGroup <- obtainNodeNames(degrG, gFacultyInd, numOptNodes = 4)
evceGroup <- obtainNodeNames(evceG, gFacultyInd, numOptNodes = 4)
pagRGroup <- obtainNodeNames(pagRG, gFacultyInd, numOptNodes = 4)

