## Example of usage:
## m <- matrix(1:4, 2, 2)
## cached_m <- makeCacheMatrix(m)
## cacheSolve(cached_m)
## -----------------------------------------
## First usage of cacheSolve() function on a given matrix
## is done explicitly by solve() and the result is cached.
## Further invokes benefit from cached data. 
 
## makeCacheMatrix() 
## argument: matrix to be cached
## return: list of 4 functions
##      $set() - sets the matrix x
##      $get() - gets the matrix x
##      $setinverce() - sets the inverce 
##          of matrix x which is m
##      $getinverce() - returns m
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverce <- function(inverce) m <<- inverce
    getinverce <- function() m
    list(set = set, 
         get = get, 
         setinverce = setinverce, 
         getinverce = getinverce)
}

## cacheSolve()
## arguments: matrix x
##          ... arguments that can be passed to solve()
## return: inverce of x
## -------------------------------------------------------
## if x has its inverce cached than the cached value is 
## returned, otherwise solve() function is invoked and
## the result both stored in x and returned
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverce()
    if (!is.null(m)) {
        message("getting cached inverce matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverce(m)
    m
}
