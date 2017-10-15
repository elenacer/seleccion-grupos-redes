#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSIÓN APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ & ALBERTO IBARRONDO LUIS
#DATE: 08/06/2016


set.seed(112358)
#set.seed(16807)

#Grafo 1
madj <- matrix(rbinom(40000, 1, 0.01), 200, 200)
diag(madj) <- 0
madj <- madj * matrix(runif(40000), 200, 200)
rg1 <- matrixToGraph(madj)

#Grafo 2
madj <- matrix(rbinom(40000, 1, 0.1), 200, 200)
diag(madj) <- 0
madj <- madj * matrix(runif(40000), 200, 200)
rg2 <- matrixToGraph(madj)

#Grafo 3
madj <- matrix(rbinom(40000, 1, 0.008), 200, 200)
diag(madj) <- 0
madj <- madj * matrix(runif(40000), 200, 200)
rg3 <- matrixToGraph(madj)

#Grafo 4
madj <- matrix(rbinom(250000, 1, 0.007), 500, 500)
diag(madj) <- 0
madj <- madj * matrix(runif(250000), 500, 500)
rg4 <- matrixToGraph(madj)

#Grafo 5
madj <- matrix(rbinom(250000, 1, 0.05), 500, 500)
diag(madj) <- 0
madj <- madj * matrix(runif(250000), 500, 500)
rg5 <- matrixToGraph(madj)

#Grafo 6
madj <- matrix(rbinom(250000, 1, 0.01), 500, 500)
diag(madj) <- 0
madj <- madj * matrix(runif(250000), 500, 500)
rg6 <- matrixToGraph(madj)

#Grafo 7
madj <- matrix(rbinom(1000000, 1, 0.007), 1000, 1000)
diag(madj) <- 0
madj <- madj * matrix(runif(1000000), 1000, 1000)
rg7 <- matrixToGraph(madj)

#Grafo 8
madj <- matrix(rbinom(1000000, 1, 0.008), 1000, 1000)
diag(madj) <- 0
madj <- madj * matrix(runif(1000000), 1000, 1000)
rg8 <- matrixToGraph(madj)

#Grafo 9
madj <- matrix(rbinom(25000000, 1, 0.001), 5000, 5000)
diag(madj) <- 0
madj <- madj * matrix(runif(25000000), 5000, 5000)
rg9 <- matrixToGraph(madj)

#Grafo 10 -- NO PROBAR EN MI PORTATIL
madj <- matrix(rbinom(100000000, 1, 0.001), 10000, 10000)
diag(madj) <- 0
madj <- madj * matrix(runif(100000000), 10000, 10000)
rg10 <- matrixToGraph(madj)