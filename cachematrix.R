## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix stores the inverse of a matrix in cache and makes it available
## for future use via cacheSolve
## makeCacheMatrix takes as input a matrix, outputs a 4-vector of commands that
##put  the matrix and its inverse into cache and makes them available for later use

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inv<<-solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}
## Write a short comment describing this function
##cacheSolve takes output of makeCacheMatrix applied to a matrix.  If the inverse has
## previously been calculated, it is retrieved as output.  If not, it is calculated
## and given as output
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
