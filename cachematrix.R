  ## these functions work to calculate the inverse of a matrix
  ## once the calculation is performed the first time, it stores the answer.
  ## If the calculation is called again, instead of running the calculation
  ## the previous answer is called, as long as the matrix hasn't changed
makeCacheMatrix <- function(x = matrix()) {
  ## this function creates an object that can store a matrix and also its inverse
  ## if it has been previously calculated
  ## set m, which is the stored inverse, to null since this is a new matrix
  m <- NULL
  
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x
  ## check m to see if the inverse calculation has been previously performed
  ## if m is null, then perform the calculation and store the answer in m
  ## if m is not null, then return it since it is the solution to the inverse
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
