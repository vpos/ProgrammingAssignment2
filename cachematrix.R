## Overall - this function sets values of a new matrix x, sets values of an inverted matrix inv from matrix x, 
## and then caches the inverted matrix

## This part creates a new matrix which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        slve <- function(solve) inv <<- solve
        get2 <- function() inv
        list(set = set, get = get,
             slve = slve, get2 = get2)

}

## This part computes the inverse of a matrix created above
cacheSolve <- function(x, ...) {
        inv <- x$get2()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$slve(inv)
        inv
}

