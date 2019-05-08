## These two functions help us to compute the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # NULL value for the inverseMatrix at initialization
    inverseMatrix <- NULL
    
    # set function changes the value of the matrix (also it sets to NULL the inverseMatrix)
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    # get function returns the value of the matrix
    get <- function(){
        x
    }
    
    # setInverse function loads the value of the inverseMatrix
    setInverse <- function(inverse){
        inverseMatrix <<- inverse
    } 
    
    # getInverse function returns the value of the inverseMatrix
    getInverse <- function(){
        inverseMatrix
    }
    
    # Return the list with the functions described above
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
cacheSolve <- function(x, ...) {
    
    # First of all, we verify if the inverse has been already calculated.
    inverseM<- x$getInverse()
    
    # If it is different to NULL, we take the value from the "special" matrix object 
    if(!is.null(inverseM)) {
        message("Getting cached data for the inverseMatrix")
        return(inverseM)
    }
    
    # If it is not NULL, we calculate it
    matrix <- x$get()
    inverseM <- solve(matrix, ...)
    x$setInverse(inverseM)
    
    ## Return a matrix that is the inverse of 'x'
    inverseM
}
