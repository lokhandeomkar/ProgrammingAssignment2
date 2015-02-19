## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function creates a special "matrix" object
#    that can cache its inverse.

# a matrix object is passed to makeCacheMatrix and it 
# returns a list with functions set,get,setinv,getinv  

makeCacheMatrix <- function(x = matrix()) {
		xinv <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setinv <- function(inv) xinv <<- inv
            getinv <- function() xinv
            list(set = set, get = get,
                 setinv = setinv,
                 getinv = getinv)
}


## Write a short comment describing this function
#This function computes the inverse of the special
#    "matrix" returned by `makeCacheMatrix` above. If the inverse has
#    already been calculated (and the matrix has not changed), then
#    `cacheSolve` should retrieve the inverse from the cache.

# this is called with the list returned by above function makeCacheMatrix
# and then returns the inverse


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
