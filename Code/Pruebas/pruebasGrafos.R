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
source("thresholdGreedy.R")
source("thresholdGHeur1.R")
source("thresholdGHeur3.R")
source("thresholdGreedyStepwise.R")
source("thresholdGSHeur1.R")
source("thresholdGSHeur2.R")
source("thresholdGSHeur3.R") 
source("randomNetworks.R")
source("carga.R")

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


#Realización de todas las pruebas sobre cada uno de los grafos generados, ya sea real o aleatorio

#Grafo de amistades gFriends
tgfri <- thresholdGreedy(gFriends, 10, loops = 5000)
tgsfri <- thresholdGreedyStepwise(gFriends, 10, loops = 5000)
tgsh1fri <- thresholdGSHeur1(gFriends, 10, loops = 5000)
tgh1fri <- thresholdGHeur1(gFriends, 10, loops = 5000)
tgs2fri <- thresholdGreedyStepwise(gFriends, 10, loops = 5000, modeGreedy = 2)
tgsh1fri2 <- thresholdGSHeur1(gFriends, 10, loops = 5000, modeGreedy = 2)
tgsh2fri <- thresholdGSHeur2(gFriends, 10, loops = 5000, fixedGroup = 5)
tgsh3fri <- thresholdGSHeur3(gFriends, 10, loops = 5000, quantileHeur = 3)
tgh3fri <- thresholdGHeur3(gFriends, 10, loops = 5000, quantileHeur = 3)
tgsh3fri2 <- thresholdGSHeur3(gFriends, 10, loops = 5000, quantileHeur = 3, modeGreedy = 2)

#Grafo sobre relación de compras entre libros de política gBooks
tgb <- thresholdGreedy(gBooks, 8, loops = 1500)
tgsb <- thresholdGreedyStepwise(gBooks, 8, loops = 1500)
tgsh1b <- thresholdGSHeur1(gBooks, 8, loops = 1500)
tgh1b <- thresholdGHeur1(gBooks, 8, loops = 1500)
tgs2b <- thresholdGreedyStepwise(gBooks, 8, loops = 1500, modeGreedy = 2)
tgsh1b2 <- thresholdGSHeur1(gBooks, 8, loops = 1500, modeGreedy = 2)
tgsh2b <- thresholdGSHeur2(gBooks, 8, loops = 1500, fixedGroup = 4)
tgsh3b <- thresholdGSHeur3(gBooks, 8, loops = 1500, quantileHeur = 3)
tgh3b <- thresholdGHeur3(gBooks, 8, loops = 1500, quantileHeur = 3)
tgsh3b2 <- thresholdGSHeur3(gBooks, 8, loops = 1500, quantileHeur = 3, modeGreedy = 2)

#Grafo sobre comunidad de delfines gDolphins
tgd <- thresholdGreedy(gDolphins, 4, loops = 1000)
tgsd <- thresholdGreedyStepwise(gDolphins, 4, loops = 1000)
tgsh1d <- thresholdGSHeur1(gDolphins, 4, loops = 1000)
tgh1d <- thresholdGHeur1(gDolphins, 4, loops = 1000)
tgs2d <- thresholdGreedyStepwise(gDolphins, 4, loops = 1000, modeGreedy = 2)
tgsh1d2 <- thresholdGSHeur1(gDolphins, 4, loops = 1000, modeGreedy = 2)
tgsh2d <- thresholdGSHeur2(gDolphins, 4, loops = 1000, fixedGroup = 2)
tgsh3d <- thresholdGSHeur3(gDolphins, 4, loops = 1000, quantileHeur = 3)
tgh3d <- thresholdGHeur3(gDolphins, 4, loops = 1000, quantileHeur = 3)
tgsh3d2 <- thresholdGSHeur3(gDolphins, 4, loops = 1000, quantileHeur = 3, modeGreedy = 2)

#Grafo sobre amistades ponderadas dentro de una facultad de UK gFaculty
tgfa <- thresholdGreedy(gFaculty, 4, loops = 1000)
tgsfa <- thresholdGreedyStepwise(gFaculty, 4, loops = 1000)
tgsh1fa <- thresholdGSHeur1(gFaculty, 4, loops = 1000)
tgh1fa <- thresholdGHeur1(gFaculty, 4, loops = 1000)
tgs2fa <- thresholdGreedyStepwise(gFaculty, 4, loops = 1000, modeGreedy = 2)
tgsh1fa2 <- thresholdGSHeur1(gFaculty, 4, loops = 1000, modeGreedy = 2)
tgsh2fa <- thresholdGSHeur2(gFaculty, 4, loops = 1000, fixedGroup = 2)
tgsh3fa <- thresholdGSHeur3(gFaculty, 4, loops = 1000, quantileHeur = 3)
tgh3fa <- thresholdGHeur3(gFaculty, 4, loops = 1000, quantileHeur = 3)
tgsh3fa2 <- thresholdGSHeur3(gFaculty, 4, loops = 1000, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 1 rg1, con 200 nodos y una densidad de conexiones entre nodos en la red del 1%
tgr1 <- thresholdGreedy(rg1, 10, loops = 1000)
tgsr1 <- thresholdGreedyStepwise(rg1, 10, loops = 1000)
tgsh1r1 <- thresholdGSHeur1(rg1, 10, loops = 1000)
tgh1r1 <- thresholdGHeur1(rg1, 10, loops = 1000)
tgs2r1 <- thresholdGreedyStepwise(rg1, 10, loops = 1000, modeGreedy = 2)
tgsh1r12 <- thresholdGSHeur1(rg1, 10, loops = 1000, modeGreedy = 2)
tgsh2r1 <- thresholdGSHeur2(rg1, 10, loops = 1000, fixedGroup = 5)
tgsh3r1 <- thresholdGSHeur3(rg1, 10, loops = 1000, quantileHeur = 3)
tgh3r1 <- thresholdGHeur3(rg1, 10, loops = 1000, quantileHeur = 3)
tgsh3r12 <- thresholdGSHeur3(rg1, 10, loops = 1000, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 2 rg2, con 200 nodos y una densidad de conexiones entre nodos en la red del 10%
tgr2 <- thresholdGreedy(rg2, 10, loops = 1000)
tgsr2 <- thresholdGreedyStepwise(rg2, 10, loops = 1000)
tgsh1r2 <- thresholdGSHeur1(rg2, 10, loops = 1000)
tgh1r2 <- thresholdGHeur1(rg2, 10, loops = 1000)
tgs2r2 <- thresholdGreedyStepwise(rg2, 10, loops = 1000, modeGreedy = 2)
tgsh1r22 <- thresholdGSHeur1(rg2, 10, loops = 1000, modeGreedy = 2)
tgsh2r2 <- thresholdGSHeur2(rg2, 10, loops = 1000, fixedGroup = 5)
tgsh3r2 <- thresholdGSHeur3(rg2, 10, loops = 1000, quantileHeur = 3)
tgh3r2 <- thresholdGHeur3(rg2, 10, loops = 1000, quantileHeur = 3)
tgsh3r22 <- thresholdGSHeur3(rg2, 10, loops = 1000, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 3 rg3, con 200 nodos y una densidad de conexiones entre nodos en la red del 0.8%
tgr3 <- thresholdGreedy(rg3, 10, loops = 1000)
tgsr3 <- thresholdGreedyStepwise(rg3, 10, loops = 1000)
tgsh1r3 <- thresholdGSHeur1(rg3, 10, loops = 1000)
tgh1r3 <- thresholdGHeur1(rg3, 10, loops = 1000)
tgs2r3 <- thresholdGreedyStepwise(rg3, 10, loops = 1000, modeGreedy = 2)
tgsh1r32 <- thresholdGSHeur1(rg3, 10, loops = 1000, modeGreedy = 2)
tgsh2r3 <- thresholdGSHeur2(rg3, 10, loops = 1000, fixedGroup = 5)
tgsh3r3 <- thresholdGSHeur3(rg3, 10, loops = 1000, quantileHeur = 3)
tgh3r3 <- thresholdGHeur3(rg3, 10, loops = 1000, quantileHeur = 3)
tgsh3r32 <- thresholdGSHeur3(rg3, 10, loops = 1000, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 4 rg4, con 500 nodos y una densidad de conexiones entre nodos en la red del 0.7%
tgr4 <- thresholdGreedy(rg4, 15, loops = 100)
tgsr4 <- thresholdGreedyStepwise(rg4, 15, loops = 100)
tgsh1r4 <- thresholdGSHeur1(rg4, 15, loops = 100)
tgh1r4 <- thresholdGHeur1(rg4, 15, loops = 100)
tgs2r4 <- thresholdGreedyStepwise(rg4, 15, loops = 100, modeGreedy = 2)
tgsh1r42 <- thresholdGSHeur1(rg4, 15, loops = 100, modeGreedy = 2)
tgsh2r4 <- thresholdGSHeur2(rg4, 15, loops = 100, fixedGroup = 7)
tgsh3r4 <- thresholdGSHeur3(rg4, 15, loops = 100, quantileHeur = 3)
tgh3r4 <- thresholdGHeur3(rg4, 15, loops = 100, quantileHeur = 3)
tgsh3r42 <- thresholdGSHeur3(rg4, 15, loops = 100, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 5 rg5, con 500 nodos y una densidad de conexiones entre nodos en la red del 5%
tgr5 <- thresholdGreedy(rg5, 15, loops = 100)
tgsr5 <- thresholdGreedyStepwise(rg5, 15, loops = 100)
tgsh1r5 <- thresholdGSHeur1(rg5, 15, loops = 100)
tgh1r5 <- thresholdGHeur1(rg5, 15, loops = 100)
tgs2r5 <- thresholdGreedyStepwise(rg5, 15, loops = 100, modeGreedy = 2)
tgsh1r52 <- thresholdGSHeur1(rg5, 15, loops = 100, modeGreedy = 2)
tgsh2r5 <- thresholdGSHeur2(rg5, 15, loops = 100, fixedGroup = 7)
tgsh3r5 <- thresholdGSHeur3(rg5, 15, loops = 100, quantileHeur = 3)
tgh3r5 <- thresholdGHeur3(rg5, 15, loops = 100, quantileHeur = 3)
tgsh3r52 <- thresholdGSHeur3(rg5, 15, loops = 100, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 6 rg6, con 500 nodos y una densidad de conexiones entre nodos en la red del 1.5%
tgr6 <- thresholdGreedy(rg6, 15, loops = 100)
tgsr6 <- thresholdGreedyStepwise(rg6, 15, loops = 100)
tgsh1r6 <- thresholdGSHeur1(rg6, 15, loops = 100)
tgh1r6 <- thresholdGHeur1(rg6, 15, loops = 100)
tgs2r6 <- thresholdGreedyStepwise(rg6, 15, loops = 100, modeGreedy = 2)
tgsh1r62 <- thresholdGSHeur1(rg6, 15, loops = 100, modeGreedy = 2)
tgsh2r6 <- thresholdGSHeur2(rg6, 15, loops = 100, fixedGroup = 7)
tgsh3r6 <- thresholdGSHeur3(rg6, 15, loops = 100, quantileHeur = 3)
tgh3r6 <- thresholdGHeur3(rg6, 15, loops = 100, quantileHeur = 3)
tgsh3r62 <- thresholdGSHeur3(rg6, 15, loops = 100, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 7 rg7, con 1000 nodos y una densidad de conexiones entre nodos en la red del 5%
tgr7 <- thresholdGreedy(rg7, 20)
tgsr7 <- thresholdGreedyStepwise(rg7, 20)
tgsh1r7 <- thresholdGSHeur1(rg7, 20)
tgh1r7 <- thresholdGHeur1(rg7, 20)
tgs2r7 <- thresholdGreedyStepwise(rg7, 20, modeGreedy = 2)
tgsh1r72 <- thresholdGSHeur1(rg7, 20, modeGreedy = 2)
tgsh2r7 <- thresholdGSHeur2(rg7, 20, fixedGroup = 10)
tgsh3r7 <- thresholdGSHeur3(rg7, 20, quantileHeur = 3)
tgh3r7 <- thresholdGHeur3(rg7, 20, quantileHeur = 3)
tgsh3r72 <- thresholdGSHeur3(rg7, 20, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 8 rg8, con 1000 nodos y una densidad de conexiones entre nodos en la red del 2.5%
tgr8 <- thresholdGreedy(rg8, 20)
tgsr8 <- thresholdGreedyStepwise(rg8, 20)
tgsh1r8 <- thresholdGSHeur1(rg8, 20)
tgh1r8 <- thresholdGHeur1(rg8, 20)
tgs2r8 <- thresholdGreedyStepwise(rg8, 20, modeGreedy = 2)
tgsh1r82 <- thresholdGSHeur1(rg8, 20, modeGreedy = 2)
tgsh2r8 <- thresholdGSHeur2(rg8, 20, fixedGroup = 10)
tgsh3r8 <- thresholdGSHeur3(rg8, 20, quantileHeur = 3)
tgh3r8 <- thresholdGHeur3(rg8, 20, quantileHeur = 3)
tgsh3r82 <- thresholdGSHeur3(rg8, 20, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 9 rg9, con 5000 nodos y una densidad de conexiones entre nodos en la red del 2.5%
tgr9 <- thresholdGreedy(rg9, 20)
tgsr9 <- thresholdGreedyStepwise(rg9, 20)
tgsh1r9 <- thresholdGSHeur1(rg9, 20)
tgh1r9 <- thresholdGHeur1(rg9, 20)
tgs2r9 <- thresholdGreedyStepwise(rg9, 20, modeGreedy = 2)
tgsh1r92 <- thresholdGSHeur1(rg9, 20, modeGreedy = 2)
tgsh2r9 <- thresholdGSHeur2(rg9, 20, fixedGroup = 10)
tgsh3r9 <- thresholdGSHeur3(rg9, 20, quantileHeur = 3)
tgh3r9 <- thresholdGHeur3(rg9, 20, quantileHeur = 3)
tgsh3r92 <- thresholdGSHeur3(rg9, 20, quantileHeur = 3, modeGreedy = 2)

#Grafo aleatorio 10 rg10, con 10000 nodos y una densidad de conexiones entre nodos en la red del 2.5%
tgr10 <- thresholdGreedy(rg10, 20)
tgsr10 <- thresholdGreedyStepwise(rg10, 20)
tgsh1r10 <- thresholdGSHeur1(rg10, 20)
tgh1r10 <- thresholdGHeur1(rg10, 20)
tgs2r10 <- thresholdGreedyStepwise(rg10, 20, modeGreedy = 2)
tgsh1r102 <- thresholdGSHeur1(rg10, 20, modeGreedy = 2)
tgsh2r10 <- thresholdGSHeur2(rg10, 20, fixedGroup = 10)
tgsh3r10 <- thresholdGSHeur3(rg10, 20, quantileHeur = 3)
tgh3r10 <- thresholdGHeur3(rg10, 20, quantileHeur = 3)
tgsh3r102 <- thresholdGSHeur3(rg10, 20, quantileHeur = 3, modeGreedy = 2)


#Centralidades individuales grafos


save.image(file = "datosPruebasThreshold.RData", ascii = TRUE)