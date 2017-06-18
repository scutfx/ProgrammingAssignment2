
## the makeCasheMatrix calculate the inverse of input matrix and stroe it in a different environment
## the cacheSolve function checks if inverse matrix exists. If so, it will retrieve the calculated inverse matrix, otherwise will calculate the inverse and store it in a different environment


## the makeCasheMatrix calculate the inverse of input matrix and stroe it in a different environment

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverseMatrix <<- inverse
        getinverse <- function() inverseMatrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## the cacheSolve function checks if inverse matrix exists. If so, it will retrieve the calculated inverse matrix
## otherwise will calculate the inverse and store it in a different environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverse()
        if(!is.null(inverseMatrix)) {
                message("getting cached data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- solve(data, ...)
        x$setinverse(inverseMatrix)
        inverseMatrix
}
