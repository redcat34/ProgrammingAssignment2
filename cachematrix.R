## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix())) {
        matrxInv <- NULL
        set <- function(y) {
                x <<- y
                matrxInv <<- NULL
        }
        ## get the value of the matrix.
        get <- function() x
        ## set the inverse of the matrix.
        setInv <- function(i) matrxInv <<- i
        getInv <- function() matrxInv
        ## get the inverse of the matrix.
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}
## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## get the inverse of the matrix.
        matrxInv <- x$getInv()
        ## check if there is the matrix, if yes: print the message.
        if(!is.null(matrxInv)) {
                print("getting cached data")
                return(matrxInv)
        }
        ## if not: get the inverse of the matrix.
        data <- x$get()
        matrxInv <- solve(data, ...)
        ## set the inverse of the matrix.
        x$setInv(matrxInv)
        matrxInv
}

