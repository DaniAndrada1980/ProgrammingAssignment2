## Como vemos la siguiente funcion se creo para almacenar en el cache 
##la inversa del objeto "matrix" ques igual a "x".
##resumiendo la funcion: Primero se establece la funcion "makeCacheMatrix"
## luego se cre el objeto "i" que seria para la inversa del objet "x" 
## "x" es igual al objeto "matrix"
##a "i" se le asigna un valor "NULL" para que luego adopte el valor inverso de "x"
## (si existiera el valor inverso de "x")
## luego se establecen los parametros para convertir "x" a su inversa "i"


makeCacheMatrix <- function(x = matrix()) {
 i <- NULL
  set <- function(y){
  x <<- y
  i <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
  setInverse = setInverse, 
  getInverse = getInverse)
}

## luego para finalizar a travez de la funcion "cacheSolve" 
## se obtiene o se calcula la inversa y se obtiene el resultado de "i"

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)){
  message("getting cached data")
  return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}

## Muchas Gracias por revisar esta tarea !! Daniel Andrada
