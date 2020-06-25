##Functions to find inverse of matrix using cache

## This function is used to create a special matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(n){
    m <<- n
    i <<- NULL
  }
  
  get <- function(){
    return(m)
  }
  
  setInverse <- function(j){
    i <<- j
  }
  
  getInverse <- function(){
    return(i)
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}



## This function is used to return a matrix which is inverse of the 'x' which is passed as argument

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}