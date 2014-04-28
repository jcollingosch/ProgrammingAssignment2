# There are two functins defined below: makeCacheMatrix and cacheSolve.
# They help to cache the computations made during matrix inversion
# If a matricies inversere has previously been calculated, this value is
# stored away so that when asked for again it can quickly be returned 
# without the need to re-calculate.
#
# makeCacheMatrix: creates a "cache matrix" object whith setter and getter
#                  helpers to aid the cache process
# cacheSolve: this function takes in a "cache matrix" object output from the 
#             makeCacheMatrix function and returns the inverse. If the
#             inverse has been calculated then it will returned the 
#             cached value, else it will calculate the inverse.



# This function creates a "cache matrix" object 
# the list returned should be stored and used 
# in the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
      # Create a cache matrix object
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function takes as input a "cache matrix" created
# from the function makeCacheMatrix, and returns the inverse
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinv(i)
  i
}

