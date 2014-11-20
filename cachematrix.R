

## creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set the inverse of the matrix
    setinverse <- function(solve) m <<- solve
    getinverse <-  function() m
    ## get the inverse of the matrix
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
    
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    ## if it exists, print the message
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## if not, compute the inverse
    data <- x$get()
    m <- solve(data,...)
    ## set the inverse
    x$setinverse(m)
    m
}
