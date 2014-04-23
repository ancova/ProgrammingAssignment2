## Assignment: Caching the Inverse of a Matrix
## April 23rd, 2014

## The first function,  makeCacheMatrix creates a list containing a function to
## 1. set the value of the maxtrix
## 2. get the value of the maxtrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set  <- function(y){
        x <<- y
        inv <<- NULL
    }
    get  <- function() x
    setinverse <- function(inverse) inv <- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The second function, cacheSolve
## To compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)){
        message("getting the cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

x <- matrix(1:4,c(2,2))
y <- matrix(c(1,2,2,3),c(2,2))

x_cache <- makeCacheMatrix(x)
cacheSolve(x_cache)


y_cache <- makeCacheMatrix(y)
cacheSolve(y_cache)
