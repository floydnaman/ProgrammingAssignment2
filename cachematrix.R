## This code serves to calculate the inverse of a matrix
## However as matrix inversion is a costly computation, the code caches the inverse rather than compute it repeatedly

## The following function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix())
{
        inv <- NULL
        set <- function(y)
        {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The following function 'cacheSolve' computes the inverse of the special "matrix" returned by the function 'makeCacheMatrix'
## If the inverse has already been calculated (and the matrix not changed), it is retrieved from the cache

cacheSolve <- function(x, ...)
{
        inv <- x$getinverse()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        inv <- solve(x$get(), ...)
        x$setinverse(inv)
        inv
}