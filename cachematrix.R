## In assignment 2, we are designing a pair of functions for caching the inverse of a matrix


## The first function will create a matrix object that can be used to cache its inverse


makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
      set <- function(y){
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

##This function case compute the inverse of the matrix produced by the 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
         inv <- x$getinverse()
      if(!is.null(inv)){
            message("getting cached data")
            return(inv)
      }
      matr <- x$get()
      inv <- solve(matr, ...)
      x$setinverse(inv)
      inv
}
