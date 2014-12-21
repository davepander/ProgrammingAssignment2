## Put comments here that give an overall description of what your
## functions do



## This function creates a matrix object and can cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    #The inverser will be stored in im "inverse matrix"
    im <- NULL
    
    set <- function(y) {
      x <<- y
      im <<- NULL
    }
    get  <- function () x
    
    setinverse <- function(inverse) im <<- inverse
    getinverse <- function() im
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of a matrix if not already stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        
        ## Is it in the cache?
        if (!is.Null(im)){
          message("retrieving cached data")
          return(im)          
        }
        
        ## Not in cache, so calculate
        data <- x$get()
        im <- solve(data,...)
        
        ## Add to cache
        x$setinverse(im)
        
        ## return
        im
}
