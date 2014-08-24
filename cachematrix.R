## functions that caches the inverse of a matrix
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        j <- NULL                                
        set <- function(matrix)                          ## Setting the matrix
                m <<- matrix
                j <<- NULL
        }
        get <- function()                                ## Getting the matrix
        m                                                ## Prints the matrix
        setInverse <- function(inverse) j <<- NULL       ## Set the inverse of the matrix
        getInverse <- function() j                       ## Get the inverse of the matrix
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)   ## List of mini-functions
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {     
     j <- x$getInverse()                                 ## Return a matrix that is the inverse of 'x'
     if(!is.null(j)) {                                   ## Return inverse if it is already set
                message("getting cached data")
                return(j)
     }
     data <- x$get()                                     ## Get the matrix
     j <- solve(data) %*% data                           ## Find inverse by matrix multiplication
     x$setInverse(j)                                     ## Set the inverse
     j                                                   ## Prints the matrix
}