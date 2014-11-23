## cacheMatrix is a wrapper around a matrix object that allows for the 
## computation and storage of the inverse of the matrix, to avoid
## repeating expensive matrix inverse calculations.

## makeCacheMatrix allocates storage for the inverse of the matrix mat.
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


## cacheSolve checks if the inverse of the cachedMatrix has been computed, and
## computes and stores it if it has not.

cacheSolve <- function(cacheMatrix, ...) {
        
        ## Get the currently stored inverse, and return it if it's not NULL
        matInverse <- cacheMatrix$getInverse()
        if (!is.null(matInverse)){
                message("Inverse is already computed.  Returning cached copy.")
                return(matInverse)
        }
        
        ## Otherwise, use the solve command to invert the original matrix, and
        ## Store the result in the matInverse field of the cacheMatrix
        matOriginal <- cacheMatrix$getMatrix()
        matInverse <- solve(matOriginal)
        cacheMatrix$setInverse(matInverse)
        
        ## Return the inverse matrix
        return(matInverse)
}
