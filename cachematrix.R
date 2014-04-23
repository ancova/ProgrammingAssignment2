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
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## The second function, cacheSolve
## To compute the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    if (!is.null(inv)){
        message("getting the cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}

#ADDED a simple test code
a <- makeCacheMatrix(matrix(1:4,2))
a$get()
#       [,1] [,2]
# [1,]    1    3
# [2,]    2    4
a$getInverse()
# NULL
a$set(matrix(5:8,2))
a$get()
#       [,1] [,2]
# [1,]    5    7
# [2,]    6    8
cacheSolve(a)
#     [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
cacheSolve(a)
# getting the cached inverse
#       [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5
a$getInverse()
#     [,1] [,2]
# [1,]   -4  3.5
# [2,]    3 -2.5

#test inverse correctness
b <- a$getInverse()
b
a$get() %*% b     
#matrix multiplication should show identity matrix
#       [,1]         [,2]
# [1,]    1 3.552714e-15
# [2,]    0 1.000000e+00
