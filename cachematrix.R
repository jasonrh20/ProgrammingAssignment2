## Since matrix inversion is usually a costly computation, there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. 
## These below functions will cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) 
{
        ## This sets the value of the vector
	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

        ## This gets the value of the vector
	  get <- function() x
	  
	  ## This sets the value of the inverse
        setsolve <- function(solve) m <<- solve
	  
	  ## This gets the value of the inverse        
	  getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## This will return a matrix that is the inverse of 'x'

	  ## This checks to see if the inverse has already been calculated.
	  ## If so, it gets the inverse from the cache and skips the computation
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

	  ## If the inverse has not already been calculated, this calculates the inverse and 
	  ## sets the value of the inverse in the cache via the setsolve function.
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
