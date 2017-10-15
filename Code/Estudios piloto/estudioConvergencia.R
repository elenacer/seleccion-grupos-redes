#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ
#DATE: 08/06/2016


#Librerías necesarias para ejecutar las pruebas
library(igraph)
library(igraphdata)
library(gtools)
library(Matrix)

#Archivos .R donde están almacenados todos los códigos desarrollados para las diferentes pruebas del 
#modelo de difusión de innovaciones basado en el umbral lineal
source("matrixToGraph.R")
source("kpsetTime.R")
source("threshold.R")
source("thresholdGreedy.R")
source("thresholdGHeur1.R")
source("thresholdGHeur3.R")
source("thresholdGreedyStepwise.R")
source("thresholdGSHeur1.R")
source("thresholdGSHeur2.R")
source("thresholdGSHeur3.R") 

#Creación de los 4 grafos reales sobre los que se probará el modelo de difusión. Entre ellos se 
#encuentran: dos grafos de amistades, gFriends y gFaculty, un grafo sobre libros de política, gBooks 
#y un grafo sobre relaciones entre delfines, gDolphins.
datafr <- read.table("amigos.txt", header = TRUE)
gFriends <- graph.data.frame(datafr, directed = TRUE)
datafr <- read.table("libros.txt", header = TRUE)
gBooks <- graph.data.frame(datafr, directed = TRUE)
datafr <- read.table("delfines.txt", header = TRUE)
gDolphins <- graph.data.frame(datafr, directed = TRUE)
data("UKfaculty")
madj <- (as_adjacency_matrix(UKfaculty, attr = "weight"))*6/100
gFaculty <- graph.adjacency(madj, mode = "directed", weighted = TRUE)


#Estudio de convergencia de la solución subóptima de los grafos reales a probar

#Pruebas sobre gFriends
tgsfri1 <- thresholdGreedyStepwise(gFriends, 10, loops = 1)
tgsfri2 <- thresholdGreedyStepwise(gFriends, 10, loops = 2)
tgsfri5 <- thresholdGreedyStepwise(gFriends, 10, loops = 5)
tgsfri10 <- thresholdGreedyStepwise(gFriends, 10, loops = 10)
tgsfri20 <- thresholdGreedyStepwise(gFriends, 10, loops = 20)
tgsfri50 <- thresholdGreedyStepwise(gFriends, 10, loops = 50)
tgsfri100 <- thresholdGreedyStepwise(gFriends, 10, loops = 100)
tgsfri200 <- thresholdGreedyStepwise(gFriends, 10, loops = 200)
tgsfri500 <- thresholdGreedyStepwise(gFriends, 10, loops = 500)
tgsfri1000 <- thresholdGreedyStepwise(gFriends, 10, loops = 1000)
tgsfri2000 <- thresholdGreedyStepwise(gFriends, 10, loops = 2000)
tgsfri5000 <- thresholdGreedyStepwise(gFriends, 10, loops = 5000)
tgsfri10000 <- thresholdGreedyStepwise(gFriends, 10, loops = 10000)

#Pruebas sobre gBooks
tgsb1 <- thresholdGreedyStepwise(gBooks, 8, loops = 1)
tgsb2 <- thresholdGreedyStepwise(gBooks, 8, loops = 2)
tgsb5 <- thresholdGreedyStepwise(gBooks, 8, loops = 5)
tgsb10 <- thresholdGreedyStepwise(gBooks, 8, loops = 10)
tgsb20 <- thresholdGreedyStepwise(gBooks, 8, loops = 20)
tgsb50 <- thresholdGreedyStepwise(gBooks, 8, loops = 50)
tgsb100 <- thresholdGreedyStepwise(gBooks, 8, loops = 100)
tgsb200 <- thresholdGreedyStepwise(gBooks, 8, loops = 200)
tgsb500 <- thresholdGreedyStepwise(gBooks, 8, loops = 500)
tgsb1000 <- thresholdGreedyStepwise(gBooks, 8, loops = 1000)
tgsb2000 <- thresholdGreedyStepwise(gBooks, 8, loops = 2000)
tgsb5000 <- thresholdGreedyStepwise(gBooks, 8, loops = 5000)
tgsb10000 <- thresholdGreedyStepwise(gBooks, 8, loops = 10000)

#Pruebas sobre gDolphins
tgsd1 <- thresholdGreedyStepwise(gDolphins, 4, loops = 1)
tgsd2 <- thresholdGreedyStepwise(gDolphins, 4, loops = 2)
tgsd5 <- thresholdGreedyStepwise(gDolphins, 4, loops = 5)
tgsd10 <- thresholdGreedyStepwise(gDolphins, 4, loops = 10)
tgsd20 <- thresholdGreedyStepwise(gDolphins, 4, loops = 20)
tgsd50 <- thresholdGreedyStepwise(gDolphins, 4, loops = 50)
tgsd100 <- thresholdGreedyStepwise(gDolphins, 4, loops = 100)
tgsd200 <- thresholdGreedyStepwise(gDolphins, 4, loops = 200)
tgsd500 <- thresholdGreedyStepwise(gDolphins, 4, loops = 500)
tgsd1000 <- thresholdGreedyStepwise(gDolphins, 4, loops = 1000)
tgsd2000 <- thresholdGreedyStepwise(gDolphins, 4, loops = 2000)
tgsd5000 <- thresholdGreedyStepwise(gDolphins, 4, loops = 5000)
tgsd10000 <- thresholdGreedyStepwise(gDolphins, 4, loops = 10000)

#Pruebas sobre gFaculty
tgsfa1 <- thresholdGreedyStepwise(gFaculty, 4, loops = 1)
tgsfa2 <- thresholdGreedyStepwise(gFaculty, 4, loops = 2)
tgsfa5 <- thresholdGreedyStepwise(gFaculty, 4, loops = 5)
tgsfa10 <- thresholdGreedyStepwise(gFaculty, 4, loops = 10)
tgsfa20 <- thresholdGreedyStepwise(gFaculty, 4, loops = 20)
tgsfa50 <- thresholdGreedyStepwise(gFaculty, 4, loops = 50)
tgsfa100 <- thresholdGreedyStepwise(gFaculty, 4, loops = 100)
tgsfa200 <- thresholdGreedyStepwise(gFaculty, 4, loops = 200)
tgsfa500 <- thresholdGreedyStepwise(gFaculty, 4, loops = 500)
tgsfa1000 <- thresholdGreedyStepwise(gFaculty, 4, loops = 1000)
tgsfa2000 <- thresholdGreedyStepwise(gFaculty, 4, loops = 2000)
tgsfa5000 <- thresholdGreedyStepwise(gFaculty, 4, loops = 5000)
tgsfa10000 <- thresholdGreedyStepwise(gFaculty, 4, loops = 10000)



save.image(file = "estudioConvergencia.RData", ascii = TRUE)