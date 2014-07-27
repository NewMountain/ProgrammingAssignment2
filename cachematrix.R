##Function 1, makeCacheMatrix, is designed to cache
## a matrix given as an argument

##Function 2, cacheSolve, is designed to take the
##matrix and return the inverse of that value
##I have altered the convention of the original template to setmatrix and getmatrix
##However, both of these values actually return the inverse of the original matrix


##Cache the matrix when handed an argument
makeCacheMatrix <- function(x = matrix()) { ##Accept argument here
    m <- NULL ## Define local variables here
    set <- function(y) {
        x <<- y ## Set Public variables here
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(mean) m <<- mean
    getmatrix <- function() m
    list(set = set, get = get, ## Set list names and elements here
        setmatrix = setmatrix,
        getmatrix = getmatrix)
}

## Calculate or return cached inverse of matrix - labelled as getmatrix and setmatrix
cacheSolve <- function(x, ...) { ##Accept argument here
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {  ## Test if the data has already been cached - if yes, inform user and return cached data
        message("Data has been cached already - returning now")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)  ##If the inverse matrix has not been cached, perform the calculation here and cache
    x$setmatrix(m)
    message("Data has not yet been cached - calculating and cache-ing now")
    m  ##return "fresh" value
}
