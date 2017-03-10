makeCacheMatrix <- function (M = matrix()) {
  ## By default N (the inverse matrix) is set to null
  N <- NULL
  ## Cache the matrix 
  set <- function (x) {
    M <<- x
    N <<- NULL
  }
  ## Retrieve the matrix
  get <- function()M
  ## Cache the inverse matrix
  setinverse <- function(x) N <<- x
  ## Retrieve the inverse
  getinverse <- function() N
  ## List with all the functions needed for inversing and caching
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function (x = list()) {
  N <- x$getinverse()
  ## Retrieve the inverse matrix in the cache if it has already been computed
  if (!is.null(N)) {
    ## Diagnostic message to check if the result is retrieved from the cache
    message("Getting cached data")
    return(N)
  }
  ## Compute the inverse matrix and store it in the cache
  Input <- x$get()
  N <- solve(Input)
  message("Getting uncached data")
  x$setinverse(N)
  N
}
