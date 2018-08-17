## Creates an object that can caches a passed matrix and the inverse of passed matrix
## Matrices must be square
makeCacheMatrix <- function(regularM = matrix()) {
        inverseM <- NULL
        ## reset with new matrix
        set <- function(y) {
                regularM <<- y
                inverseM <<- NULL
        }
        ## get matrix
        get <- function() regularM
        ## cache inverse matrix
        setinverse <- function(receiveInverse) inverseM <<- receiveInverse
        ## retrieve cached inverse matrix
        getinverse <- function() inverseM
        ## return object
        list(set=set, get=get, setinverse=setinverse,getinverse=getinverse)
}

## Computes inverse of matrix returned by makeCacheMatrix or returns cached inverse

cacheSolve <- function(x, ...) {
        ## check for invertible matrix
        if (!class(try(solve(x$get()),silent=T))=="matrix") {
                message("Matrix has no inverse!")
                return()
        }
        ## retrieve cached inverse
        cachedM <- x$getinverse()
        ## if inverse already calculated, return cached inverse
        if (!is.null(cachedM)) {
                        message("Retrieving cached inverse...")
                        return(cachedM)
        }
        ## calculate and set inverse
        origM <- x$get()
        newInverse <- solve(origM)
        x$setinverse(newInverse)
        ## Return a matrix that is the inverse of regularM
        message(newInverse)
}
