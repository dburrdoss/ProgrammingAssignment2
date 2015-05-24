## The two functions makeCacheMatrix and cacheSolve create a special object that stores
## a square invertible matrix and computes (if necessary) and stores its inverse.
## Note the functions assume the user-supplied matrix is square and invertible.
##

## The first function, makeCacheMatrix, creates a special list object
## which contains four functions to:
##   set the value of the matrix
##   get the value of the matrix
##   set the value of the inverse of the matrix
##   get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
     xinv <- NULL
     set <- function(y){
         x <<- y
         xinv <<- NULL
     }
     get <- function() x
     setinv <- function(inv) xinv <<- inv
     getinv <- function() xinv
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## The second function, cacheSolve, calculates the inverse of the
## matrix in the special object created by makeCacheMatrix.  However,
## it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the
## computation.  Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setmean
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
     }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}


# An illustration:
amat <- matrix(c(0,1,1,1),nrow=2)
x <- makeCacheMatrix(amat)
cacheSolve(x)
# run it a second time; same inverse but from the cache:
cacheSolve(x)
# Note:  Errors result if you name the original matrix 'x' in the above code
