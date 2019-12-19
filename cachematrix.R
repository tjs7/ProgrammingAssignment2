## These two functions store a matrix and it's inverse in memory using "internal" functions and allow recall of both the matrix and it's inverse from memory if they are there, thus saving a calculation.

## This function returns a list of internal functions that set, get, set the inverse or get the inverse of the input matrix. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) m <<- solve
    getInverse <- function() m
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function first checks to see if the inverse of the matrix is in memory. If it is, it returns it, if not, it gets the matrix and calculates the inverse.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}


#This code demonstrates that these functions work. cacheSolve(x) equals solve(x). The second call of cacheSolve(x) illustrates recovery from memory (will see "getting cached data" text.) This works because the enclosing enviroment of x is not the global environment, but the it's own environment that can be used to store/cache values. 

i <- matrix(1:4,nrow=2)

x <- makeCacheMatrix(i)
cacheSolve(x)
solve(x)

cacheSolve(x)

