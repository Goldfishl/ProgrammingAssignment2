## The two functions realize the storing and calculating the inverse matrix, so that if the matrix
## are not changing, we could directly get its inverse rather than recompute it.

## makeCacheMatrix() creates a list, which stores the original matrix and its inverse matrix.  
## The four functions are used to set or get the value of the matrix and the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                 # m will be the inverse of 'x', which initially set to NULL
        set <- function(y) {          
                x <<- y           # save the input 'y'
                m <<- NULL        # set the inverse to NULL
        }
        get <- function() x       #return the value of original matix
        setinverse <- function(inverse) m <<- inverse  # The first time we call cacheInverse() for a specific matrix, 
        #this function will be accessed and it will store the inverse of that matrix. 
        getinverse <- function() m  # reture the cached value to cacheInverse()
        list(set = set, get = get,             
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheInverse() accesses the list created when makeCacheMatrix was called. If the inverse has not yet been calculatd, 
## cacheInverse will calculate it and store it in the list. Else it will return the value of the inverse matrix.
cacheInverse <- function(x, ...) {
        m <- x$getinverse()    # access list and get the inverse matrix
        if(!is.null(m)) {      # if inverse matrix existed, return it
                message("getting cached data")
                return(m)
        }
        data <- x$get()        # if m was NULL, calculate the inverse matrix and store it in x
        m <- solve(data, ...)
        x$setinverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}
