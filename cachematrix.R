##Basically the two functions together cache the inverse of a given matrix


##First we need to create the cache matrix - set m and y to be zeros, cache the matrix in the setmatrix function 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  y <- NULL 
  setmatrix <- function(y) { 
    x <<- y 
    m <<- NULL 
  }
  list(setmatrix = setmatrix, getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Then we compute the value of the inverse

cacheSolve <- function(x, ...) {
    m <- x$getinverse()  # In case we already have an inverse
  if(!is.null(m)){ # check if cacheSolve is already used 
    if(x$setmatrix() == x$getmatrix()) {
      return(m)
    }
    
    y <- x$getmatrix() 
    x$setmatrix(y) 
    m <- solve(y, ...) 
    x$setinverse(m) 
    m 
  }      ## Return a matrix that is the inverse of 'x'
}
