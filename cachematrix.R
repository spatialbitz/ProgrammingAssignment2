## The following two functions provide functionality to cache a matrix and retrieve its inverse from cache. 

## The function 'makeCacheMatrix' is a special object, returning a list of functions to set and get a matrix
## as well as setting and getting the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        
        set <- function(y){
                x <<- y
                im <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(inverse) im <<- inverse
        
        getinverse <- function() im 
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## The function 'cacheSolve' checks whether the inverse of the matrix is already available in the cache and if so returns it.
## If the inverse is not available it triggers the calculation of the inverse the storage of the result in the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        
        if(!is.null(im)) {
                message("getting cached data")
                return(im)
                
        }
        
        data <- x$get()
        im <- solve(data, ...)
        
        x$setinverse(im)

        im
}
