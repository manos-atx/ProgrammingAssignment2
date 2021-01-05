## The two functions that follow are built to work together
## in order to avoid performing unnecessarily multiple times
## an expensive computation such as finding the inverse of a
## square matrix. To test the functions and how they work use
## the following lines of code:
## df <- matrix(rnorm(2000^2),2000,2000)
## funs <- makeCacheMatrix(df)
## cacheSolve(funs)
## cacheSolve(funs)

## The makeCacheMatrix function builds a list of four functions
## necessary to pass information around (set/get pair) and to
## store to of retrieve from the cache (setinverse/getinverse)
## the inverse of a matrix. The function also resets the state
## of the combination of the two functions by setting m <- NULL
## whenever called

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        set <- function(y) {
          x <<- y
          m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)

}

## The cacheSolve function is built to take advantage of the four
## functions prepared by the makeCacheMatrix function and either 
## retrieve the inverse matrix from cache or compute it and
## store it in the cache 

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m

}
