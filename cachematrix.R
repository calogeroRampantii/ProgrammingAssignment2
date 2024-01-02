## Put comments here that give an overall description of what your
## functions do

# Funzione per creare una matrice speciale con cache per l'inverso

makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  inv <- NULL
  
  setMatrix <- function(matrix) {
    mat <<- matrix
    inv <<- NULL
  }
  
  getMatrix <- function() mat
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(setMatrix = setMatrix,
       getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  # Recupera l'inverso dalla cache se è già stato calcolato
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  # Altrimenti, calcola l'inverso e memorizzalo nella cache
  matrix <- x$getMatrix()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  inverse
}