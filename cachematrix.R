## makeCacheMatrix and cacheSolve work together to initialize, cache, and calculate the inverse of a matrix
## makeCacheMatrix initializes the cached objects and methods for them
## cachSolve returns the cached inverse if it exist, or caculates, caches, and returns it if it does not exist

## makeCacheMatrix creates the data structure and methodes to calculate and cache a matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## method to initialize the value of the special matrix
  set <- function(y) {
    x <<- y       ## the cached matrix
    inv <<- NULL  ## the cached matrix inverse
  }
  
  ## method to get the value of the matrix
  get <- function() x
  
  ## method to calculate and set the inverse of the matrix
  setinv <- function(solve) inv <<- solve
  
  ## method to return the already cached inverse (inv is NULL if it has not already been cached)
  getinv <- function() inv
  
  ## returns the list of the functions, each element named after the function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the matrix in the special vector created in makeCachMatrix above
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Get the already calculated inverse, if it already exists
  inv <- x$getinv()
  
  ## If the inverted matrix already exists, return it with a message that we are using the cached data
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## Otherwise, get the matrix, calculate the inverse, cache inverse for future use, and return it.
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
