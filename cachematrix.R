## The following code contains two functions. The first function (makeCacheMatrix)
## creates a matrix object that can cache its inverse. The second function (cacheSolve) 
## inverses the matrix the user has created, but it checks first whether the inversion 
## has already been computated, and if so, returns the cached information instead 
## of doing the calculation again.
## The functions can be called by assigning a variable, let's say "input", to the 
## function makeCacheMatrix, e.g. input <- makeCacheMatrix(). Afterwards, one stores a
## matrix like this: input$set(matrix(1:4,2)). Lastly, cacheSolve(input) will call the 
## computation of the inversion and display the result.

## This function (makeCacheMatrix) creates a matrix object and defines functions which
## are called by the second function, cacheSolve(). 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function, cacheSolve(), first checks whether the inverse of the matrix has
## already been calculated. If so, it displays the cached inverse and writes a message
## that the cached data is used and displayed. If the inverse of the matrix has not
## been calculated yet, it calculates and stores it. It finally prints the result.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
