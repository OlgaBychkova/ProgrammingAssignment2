## These two functions create a special object that stores a matrix and cache's its inverse.

## This function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {    ## set the value of the matrix
                x <<- y
                invm <<- NULL
        }
        get <- function() x     ## get the value of the matrix
        setinvm <- function(solve) invm <<- solve      ## set the value of the inverse matrix
        getinvm <- function() invm                     ## get the value of the inverse matrix
        list(set = set, get = get,
             setinvm = setinvm,
             getinvm = getinvm)
}


## This function calculates the inverse matrix of the special "matrix" created with the above function.
## However, it first checks to see if the inverse matrix has already been calculated. 
## If so, it gets the inverse matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse matrix of the data and sets the value of the inverse matrix
## in the cache via the setinvm function.

cacheSolve <- function(x, ...) {
        invm <- x$getinvm()
        if(!is.null(invm)) {      ## checking to see if the inverse matrix has already been calculated
                message("getting cached data")
                return(invm)      ## if so, let's get the inverse matrix from the cache and skip
                                  ## the computation
        }
        data <- x$get()
        invm <- solve(data, ...)  ## calculating the inverse matrix of the data
        x$setinvm(invm)           ## setting the value of the inverse matrix in the cache
        invm
}