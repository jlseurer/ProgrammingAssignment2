
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##      1. set the matrix
        ##      2. get the matrix
        ##      3. set the inverse
        ##      4. get the inverse
        
        inv <- NULL
        set <- function(y) {
                ## use `<<-` to assign a value to an object in an
                ## environment different from the current environment. 
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse 
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

cacheSolve <- function(x, ...) {
        ## return inverse of the original matrix input to makeCacheMatrix()
        
        inv <- x$getinv()
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        ## otherwise, calculate the inverse 
        mat.data = x$get()
        inv <- solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
}
