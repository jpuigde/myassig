makeCacheMatrix <- function(x = matrix()) {
        #This function generates a CacheMatrix 
        #which contains 4 functions to manage this object
        
        #$set()-> sets the value of the CacheMatrix
        #$get()-> gets the value of the CacheMatrix
        #$setinv()-> sets the value of the inverse of the CacheMatrix
        #$getinv()-> gets the value of the inverse of the CacheMatrix
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


cacheSolve <- function(x, ...) {
        #This function checks if a CacheMatrix has his inverse calculated
        # if CacheMatrix inverse is calculated, 
        #sends a massage and returns it
        # if CacheMatrix inverse is NOT calculated, 
        #calculats,sets and returns it
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}