## These functions are used to store the inverse matrix of square matrixs in the cache, 
## thus reducing need to re-iterate computation and data intensive operations like "solve" 

## makeCacheMatrix is creating an environment for the specific matrix we want to invert and store
## these specifies also specific operations that will be allowed on the data cached

## cacheSolve is instead where the actual computation is performed. It uses the functions specified in
## makeCacheMatrix to evaluate whether an inverse matrix has been already computed or if it needs to be 
## done for the first time. In that case performs the computation and then stores the inverse matrix.

makeCacheMatrix <- function(x = numeric()) {
  
  ## Initializing the inverse matrix "inv" to NULL
  ## This value is chosen to be easy to test, e.g. with is.null
  inv <- NULL
  
  ## definition of the set function. This store the matrix in the environment
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## this function extract the values of the matrix from the environment
  get <- function() x
  
  ## "setinv" store a specific matrix (e.g. inverse) in the environment cache
  setinv <- function(inverse) inv <<- inverse
  
  ## "getinv" extract the "inv" variable from the cache 
  getinv <- function() inv
  
  ## this list is returned by the function 
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


cacheSolve <- function(x, ...) {
  ## To compute the inverse value we start by accessing the environment
  ## Extracting the value stored in the environment
  inv <- x$getinv()
  
  ## If it is not NULL it means that the inverse matrix has been already computed and cached
  if(!is.null(inv)) {
    message("getting cached data")
    
    ## In this case we can just pass the stored value and exit from the function
    return(inv)
  }
  
  ## This part of the code is executed only when we do not have a cached value for the inverse matrix
  
  ## we get the stored matrix
  data <- x$get()
  
  ## compute the inverse matrix use the "solve" function
  inv <- solve(data, ...)
  
  ## we store the result of the solve in the variable of the environment
  x$setinv(inv)
  
  ## The inverse matrix is finally returned
  inv
}