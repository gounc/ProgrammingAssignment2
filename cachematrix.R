## makeCacheMatrix creates a chaced version of the inverse of an invertable matrix 
## cacheSolve returns the cached inverstion of the matrix if available, 
##      else calculates the inversion.

makeCacheMatrix <- function(x = matrix()) {
## Creates a matrix which is cached along with its inverse
        inv <-NULL
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
        get <- function() x
        setinvert <- function(solve) inv <<- solve
        getinvert <- function() inv
        list(set = set, get = get,
            setinvert = setinvert,
            getinvert = getinvert)
}



cacheSolve <- function(x, ...) {
## Returns the inversion of a matrix, from special vector made with 
### make cache vector
        inv <- x$getinvert()
        if(!is.null(inv)){
                message("getting cached inversion data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat)
        x$setinvert(inv)
        inv
}