#Matrices de adyacencia para los 7 grafos que se van a probar con los c�digos 
#realizados. Los valores esperados son para la medida thresholdGreedy

#Matriz 1: Prueba incial, grafo sin caracter�sticas especiales.
ma1 <- matrix(c(0, 0, 0, 0, 0.3, 0, 0.7, 0, 0, 0, 0, 0.4, 0.65, 0, 0.2, 0), nrow = 4)
threshold1 <- c(0.7, 0.6, 0.3, 0.6)
g1 <- matrixToGraph(ma1)

#Matriz 2: Grafo de 3 nodos que no se encuentran conectados entre s�. Deber�a 
#seleccionar los nodos 1 y 2 porque los elige en orden ascendente en caso de 
#empate.
ma2 <- matrix(rep(0, 9), nrow = 3)
threshold2 <- c(0.4, 0.1, 0.2)
g2 <- matrixToGraph(ma2)

#Matriz 3: Grafo de 6 nodos conectados de forma circular. No se llega desde 
#ningun nodo a otro ya que los umbrales son mas altos que los pesos de las aristas.
#Se elegir�n los nodos 1 y 2 al igual que en el caso del grafo 1.
ma3 <- matrix(c(0, 0, 0, 0, 0, 0.6, 0.5, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0.3, 0, 0, 0, 0, 0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0.1, 0), nrow = 6)
threshold3 <- c(0.7, 0.6, 0.5, 0.4, 0.3, 0.2)
g3 <- matrixToGraph(ma3)

#Matriz 4: Grafo de 6 nodos en los que todos ellos est�n conectados entre s�. Todos
#los nodos pueden influir a los dem�s en el tiempo t = 1. Para mayor facilidad todos
#los enlaces valdr�n 0.5 y los umbrales 0.1. Se elegir�n los nodos 1 y 2 por el mismo
#motivo que en el grafo 1.
ma4 <- matrix(rep(0.5, 36), nrow = 6) - (diag(x = 0.5, nrow = 6))
threshold4 <- rep(0.1, 6)
g4 <- matrixToGraph(ma4)

#Matriz 5: Grafo de 6 nodos colocados en forma de estrella. Las �nicas aristas son 
#las del nodo 1 que llega a todos los dem�s. No hay aristas entre los dem�s nodos ni
#entre estos y el nodo 1. Se elegir� en primer lugar el nodo 1, y en el segundo paso 
#se elegir� el 2 por el mismo motivo que en el grafo 1.
ma5 <- matrix(c(0, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0), nrow = 6)
threshold5 <- c(0.3, 0.4, 0.5, 0.4, 0.5, 0.6)
g5 <- matrixToGraph(ma5)

#Matriz 6: Grafo compuesto por 3 subgrafos, uno estrella, uno circular de 4 nodos y 
#otro circular de 3. Este caso sirve para comprobar que hace el c�digo en caso de 
#empate entre nodos, pero con distinto tiempo, y la ponderaci�n del tiempo sobre la 
#f�rmula de selecci�n de nodos, entre  el nodo 5 que alcanza solo 3 nodos pero en un
#�nico periodo, y el nodo 1 que alcanza 4 nodos pero en 2 periodos.
#El orden correcto de elecci�n de los nodos ser�a: 10 (porque llega a los mismos 
#nodos que el 1 pero en menos tiempo), el 1 (porque aunque tarde m�s tiempo que el 5,
#llega a m�s nodos) y el 5, ya que con esos 3 se accede a los 11 nodos del grafo.
ma6 <- matrix(c(0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0,
                0.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0.4, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0.4, 0, 0.2, 0, 0, 0, 0, 
                0, 0, 0, 0, 0.7, 0.7, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0.6, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0.9, 0), nrow = 11)
threshold6 <- c(0.1, 0.6, 0.3, 0.5, 0.2, 0.3, 0.6, 0.2, 0.3, 0.1, 0.5)
g6 <- matrixToGraph(ma6)

#Matriz 7: Grafo compuesto por 6 nodos, el 1 conectado al 2, y del 2 al 6 conectados 
#de forma circular. Todas las aristas tienen valores superiores a los del umbral, 
#as� que todos los nodos pueden llegar a los dem�s dentro del c�rculo. Se selecciona
#primero el nodo 1, ya que es el �nico capaz de alcanzar a todos los nodos del grafo
#y como segundo nodo se selecciona al 4 ya que es el nodo que permite minimizar el 
#tiempo a t = 2
ma7 <- matrix(c(0, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0.5, 0, 0.6, 0, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0, 0, 0.8, 0), nrow = 6)
threshold7 <- c(0.3, 0.4, 0.5, 0.4, 0.5, 0.6)
g7 <- matrixToGraph(ma7)

#Matriz 8: Hub de hubs. Est� formado por 21 nodos, en el que el nodo 11 esta formando 
#una estrella con los nodos centrales de otras 4 estrellas. Grafo para probar el 
#funcionamiento de la funci�n de thresholdGreedyStepwise. En el caso del thresholdGreedy
#deber�a elegir los nodos 11, 1, 6 y 12, y el thresholdGreedyStepwise deber�a mejorar el
#tiempo de acceso eligiendo el 1, 6, 12 y 17.
ma8 <- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0.6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0.4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0.8, 0, 0, 0, 0, 0.8, 0, 0, 0, 0, 0, 0.8, 0, 0, 0, 0, 0.8, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.5, 0, 0, 0, 0), nrow = 21)
threshold8 <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.3, 0.3, 0.3, 0.3, 0.3, 0.7, 0.6, 0.6, 0.6, 0.6, 0.6, 0.4, 0.4, 0.4, 0.4, 0.4)
g8 <- matrixToGraph(ma8)
