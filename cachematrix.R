##This assigment includes two functions that create and cache 
##the inverse of a square matrix 

##This function stores square matrix and its inverse 
##by get & set functions 

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y){
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set=set, get=get,setsolve=setsolve, getsolve=getsolve)
}


##This function calculates an inverse of square matrix or if it
##was already calculated, retrieves it from the cache

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
