## This function creates a special vector, which sets abd gets the value of the vector and then sets and gets the value of the inverse of matrix
##  then it checks if the vlue has been aleady calculated or not


## This function creates a special vector, which sets abd gets the value of the vector

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
               x <<- y
               inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
                
                }
        

##This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve retrieves the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
        }

