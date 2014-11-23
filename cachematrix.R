## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix is a wrapper around the matrix mat, that allows for
## storage and access of the inverse of mat.
## Contains get/set methods for both the original matrix and its inverse.

makeCacheMatrix <- function(mat = matrix()) {
        matInverse <- NULL  ## storage for the inverse of mat
        
        
        setMatrix <- function(newMatrix) {
                mat <<- newMatrix
                matInverse <<- NULL
        }
        
        getMatrix <- function() {
                mat
        }
        
        setInverse <- function(mInverse) {
                matInverse <<- mInverse
        }
        
        getInverse <- function() {
                matInverse
        }
        
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve computes and stores the inverse of the matrix mat 

cacheSolve <- function(cacheMatrix, ...) {
        
        matInverse <- cacheMatrix$getInverse()
        if (!is.null(matInverse)){
                message("Inverse is already computed.  Returning cached copy.")
                return(matInverse)
        }
        mat <- cacheMatrix$getMatrix()
        matInverse <- solve(mat)
        cacheMatrix$setInverse(matInverse)
        
        matInverse
        
        ## Return a matrix that is the inverse of 'mat'
        
}
