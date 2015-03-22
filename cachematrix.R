## The functions makeCacheMatrix and cacheSolve are intended to
## be used together to efficiently compute and cache the inverse
## of a matrix.
##
##      First, use makeCacheMatrix to create a cache.
##          (E.G. mc <- makeCacheMatrix(myMatrix))
##      Then, call cacheSolve, as needed, to calculate the inverse
##      of the matrix.
##          (E.G. solution <- cacheSolve(mc, ...))
##      If you need to calculate the inverse of a new matrix with
##      support for caching, the cache's set function can re-assign
##      the matrix to cache. However, it likely that you should use
##      a new cache for new matrices.


## makeCacheMatrix creates a structure with functions that
## construct and manage a cache for a matrix. Call it once to
## create a cache for the intended matrix.

makeCacheMatrix <- function(m = matrix()) {
    ## Initially, there is no result calculated, so
    ## r should have no value.
    r <- NULL

    ## set the matrix on which to perform calculations.
    set <- function(y) {
        ## If the new matrix is not equivalent to the previous
        ## one, reset the cached result.
        if (is.null(y) || dim(m) != dim(y) || !all(m == y)) {
            r <<- NULL
        }
        m <<- y
    }

    ## retrieve the matrix on which to perform calculations.
    get <- function() m

    ## set the result of the matrix calculation.
    setResult <- function(result) r <<- result

    ## retrieve the result of the matrix calculation.
    getResult <- function() r

    ## Use a List as the container for the cache functions.
    list(
        set = set,
        get = get,
        setResult = setResult,
        getResult = getResult
    )
}


## cacheSolve uses a cached matrix structure created by
## the makeCacheMatrix to compute the inverse of a matrix.
## Inovke as needed after creating the matrix cache.

cacheSolve <- function(cm, ...) {
    i <- cm$getResult()     ## Get the previous result.
    if (!is.null(i)) {      ## If it has a value,...
        return(i)           ## ...return it.
    }

    m <- cm$get()           ## Get the orinal matrix.
    i <- solve(m, ...)      ## Calculate its inverse.
    cm$setResult(i)         ## Cache the resulting inverse.

    i                       ## Return the result.
}
