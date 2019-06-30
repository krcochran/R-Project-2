## These functions take some matrix and cahce the inverse of the matrix.
## So if the user passes the cacheSolve function a matrix that it has already
## taken the inverse of, it can return the inverse faster.

## This function creates a list of four functions that will then be passed to the
## cacheMatrix function where the inverse will be taken and stored.
## "set()" will set the matrix as the one the user passed to "makeCacheMatrix
## "get()" will retreive the matrix so it can be manipulated
## "setinverse()" will set the inverse of the function to a specific value which 
## will be cached
## "getinverse()" will return the value of the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    matrix <<- y
    i <<- NULL
  }
  get <- function() {matrix}
  setinverse <- function(inverse) {i <<- inverse}
  getinverse <- function() {i}
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function first sets "i" to the inverse of the matrix. If the inverse has
## already been found, the function will return the message "getting cached inverse"
## and return the cached inverse. If the inverse has not been found, the function 
## will then compute the inverse by calling the functions defined in "makeCacheMatrix"
## and then return the inverted matrix.

cacheSolve <- function(x, ...) {
  i <- matrix$getinverse()
  if(!is.null(i)){
    message("getting cached inverse")
    return(i)
  }
  data <- matrix$get()
  i <- solve(data, ...)
  matrix$setinverse(i)
  i
}
