##Function 1, makeCacheMatrix, is designed to cache
## a matrix given as an arguemtn

##Function 2, cacheSolve, is designed to take the
##matrix and return the inverse of that value


##Cache the matrix when handed an argument
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(mean) m <<- mean
    getmatrix <- function() m
    list(set = set, get = get,
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}


## Write a short comment describing this function
## Calculate or return cached inverse of matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmatrix(m)
    m
}
