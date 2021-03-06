#PROJECT: SELECCION OPTIMA DE GRUPOS DE DIFUSI�N APLICADO AL MARKETING VIRAL
#AUTHOR: ELENA CERRATO HERNANDEZ & ALBERTO IBARRONDO LUIS
#DATE: 08/06/2016


#Modelo General del Umbral para un grupo de nodos dado

threshold <- function(graph, group, loops = 10, maAd = NULL, fixedThreshold = NULL){
  #Almacenamos el momento en el que inicia la ejecuci�n de la funci�n 
  initialTime <- Sys.time()
  
  #Si los nodos del grafo tienen nombre, contrastamos los valores pasados por par�metro
  #con los nombres de la lista de nodos del grafo, y almacenamos sus posiciones en gr
  #Si no tiene nombre, buscamos en las posiciones de los nodos directamente. Este paso lo 
  #hacemos para comprobar despu�s si alguno de los nodos dados en el grupo no pertenece al
  #grafo.
  if(is.named(graph)){
    gr <- match(group, V(graph)$name)
  } else{
    gr <- match(group, V(graph))
  }
  
  #Si alguno de los nodos del grupo no pertenece al grafo, en gr se habr� almacenado un NA
  #por cada uno de ellos. En caso de que as� sea se para la ejecuci�n de la funci�n.
  if(sum(is.na(gr)) > 0){
    stop("Uno o varios nodos del grupo no pertenecen al grafo")
  }
  
  nvert <- vcount(graph)
  
  #Inicializamos el vector adopters todo a 0 y ponemos un 1 en aquellas posiciones que se
  #correspondan con las de los nodos pasados por par�metro.
  adopters <- rep(0, nvert)
  positionGr <- 1:nvert
  adopters[positionGr %in% gr] <- 1
  
  #Creamos un vector con tantas posiciones como repeticiones se vayan a realizar para 
  #almacenar los �ptimos de cada vuelta y calcular la desviaci�n est�ndar
  optimal <- rep(0, loops)
  
  #Comprobamos si la matriz de adyacencia se est� pasando por par�metro o no
  if(!is.null(maAd)){
    adj <- maAd
  } else{
    adj <- as_adjacency_matrix(graph, attr = "weight")
  }
  
  #Fijamos la semilla para el c�lculo aleatorio del vector de umbrales threshold
  #set.seed(112358)
  set.seed(16807)
  
  #Inicializamos a 0 las variables donde se van a almacenar el n�mero de nodos alcanzados
  #(nadopters) y el tiempo (timeTotal)
  timeTotal <- 0
  nadopters <- 0
  
  #Repetimos el c�lculo con threshold diferentes en caso de que sea aleatorio tantas veces 
  #como indique el par�metro loops (10 por defecto)
  for(k in 1:loops){
    #En cada vuelta inicializamos las variables donde se van almacenando los c�lculos de 
    #los nodos que se alcanzan y el tiempo transcurrido
    timeStmp <- 0 
    adopt <- adopters
    change <- rep(1, nvert)
    
    #Si nos pasan el vector de umbrales por par�metro, lo almacenamos en threshold, si no 
    #se calcula de forma aleatoria con una distribuci�n uniforme 0-1
    if(!is.null(fixedThreshold)){
      threshold <- fixedThreshold
    } else{
      threshold <- runif(nvert)
    }
    
    #En change almacenamos si el vector de nodos alcanzados ha variado en el �ltimo c�lculo
    #o no. Es un vector l�gico, de tal forma que si no ha variado nada, su suma valdr� 0,
    #sirviendo de condici�n de salida del while
    while(sum(change) != 0){
      
      #Multiplicamos de forma matricial el vector de nodos alcanzados con la matriz de 
      #adyacencia. El vector mult tendr� los pesos que ejercen todos los nodos que ya han 
      #sido alcanzados sobre cada uno de los nodos del grafo. Estos valores se comparan con
      #el vector de umbrales, dando un vector l�gico con todos aquellos nodos alcanzados en
      #la iteraci�n.
      mult <- adopt%*%adj
      influenced <- (mult > threshold)
      
      #Se almacena en una variable auxiliar el valor num�rico (0 � 1) de la operaci�n OR 
      #sobre los nodos que ya hab�an sido alcanzados en las iteraciones anteriores y los 
      #nodos alcanzados en esta iteraci�n.
      #Y se almacena en change la diferencia.
      aux <- as.numeric(adopt | influenced)
      change <- adopt - aux
      
      #Se actualizan los valores de adopt y timeStmp con los que luego se calculan 
      #nadopters y timeTotal.
      adopt <- aux
      timeStmp <- timeStmp + 1
    }
    
    #Se van almacenando los resultados obtenidos en cada vuelta para posteriormente hacer 
    #la media y la cuasi-desviaci�n t�pica
    nadopters <- nadopters + sum(adopt)
    timeTotal <- timeTotal + (timeStmp - 1)
    optimal[k] <- nvert*(sum(adopt)) - (timeStmp - 1)
    #print(optimal[k])
  }
  
  #Se calcula la media dividiendo los resultados entre el n�mero de vueltas que ha 
  #realizado el bucle for
  nadopters <- nadopters / loops
  timeTotal <- timeTotal / loops
  
  #Y se calcula la desviaci�n t�pica de los diferentes �ptimos
  sdOptimal <- sd(optimal)
  print(mean(optimal))
  
  #Se almacena en result el n�mero de nodos del grafo, a cuantos se ha alcanzado en media
  #y en cuanto tiempo se han conseguido alcanzar en media. Y una vez asignado al vector se
  #da a cada valor el nombre que le corresponde.
  result <- c(nvert, nadopters, timeTotal, sdOptimal)
  names(result) <- c("Num Vertex", "Num Adopters", "Time", "Standard Deviation")
  
  #Se almacena el momento en el que ha acabado la ejecuci�n del programa, y la diferencia
  #entre el tiempo final y el inicial se imprime por pantalla.
  finalTime <- Sys.time()
  differenceTime <- finalTime - initialTime
  print(differenceTime)
  
  #La funci�n devuelve el valor almacenado en result
  result
}  