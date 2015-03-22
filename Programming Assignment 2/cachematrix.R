makeCacheMatrix <- function(x = matrix()) {
	xinv <- NULL 
      set <- function(y) {
	  x <<- y
	  xinv <<- NULL 
      }

      get <- function() x # return the input matrix
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # return the inversed matrix
      
      list(set = set, get = get,
	       setInv = setInv,
	       getInv = getInv)
}


cacheSolve <- function(x, ...) {
      m <- x$getInv() 
      # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
      if(!is.null(m)) { 
	  message("getting cached data")
	  return(m) 
      }
      data <- x$get()
      m <- solve(data)
      x$setInv(m) 
      m # return the solved result
}
