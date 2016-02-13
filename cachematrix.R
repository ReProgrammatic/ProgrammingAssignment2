# cachematrix.R: A set of functions that caches the inverse when working with
# matrices. Assignment for the third week of the Coursera/JHU R Programming class.


## A function that creates a list that caches the inverse of a matrix for use
## with the cacheSolve function.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # the cached inverse of the matrix
  
  get <- function(){x} # return the set or provided matrix
  
  set <- function(new_value){
    #set the matrix and NULL the cache since we have a new matrix
    x <<- new_value
    inverse <<- NULL
  }
  
  getinverse <- function(){inverse} # return the cached inverse (may be NULL)
  
  setinverse <- function(new_inverse){
    #set the inverse in the cache
    inverse <<- new_inverse
  }
  
  list(get=get, set=set, getinverse=getinverse, setinverse=setinverse)
}


## gets the inverse from a CacheMatrix list, using cache if available.
## No invalidation is enacted when using different arguments for the solve
## function, use with care.
cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        
        if (is.null(inverse)){
          message("inverse was not cached, computing and caching it")
          data <- x$get()
          inverse <- solve(data, ...)
          x$setinverse(inverse)
        }
        else {
          message("using cached inverse")
        }
        
        inverse
}