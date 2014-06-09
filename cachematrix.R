## Creates a vector of functions used to manage a cache
## of a given matrix's inversion

## Arguments:
##      X       The matrix that we'll eventuall invert

## Return Value:
##      Cache vector for that matrix

makeCacheMatrix <- function(x = matrix()) 
{
    inv <- NULL
    set <- function(y)
    {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, 
         get = get, 
         setInverse = setInverse,
         getInverse = getInverse)
}


## Gets the inverse of matrix "x".  If we have a cached
## value, return that.  Otherwise, compute the inverse
## and save the value to prevent subsequent re-computation

## Arguments:
##      x       Cache vector (as returned by MakeCacheMatrix)
##      ...     Values to compute

## Return Value:
##      Inverted matrix value

cacheSolve <- function(x, ...) 
{
    # Check the vector for an existing cached value.
    # If there is one, use that:
    
    inv <- x$getInverse()
    if (!is.null(inv))
    {
        message ("getting cached data")
        inv
    }

    # Otherwise, compute and store the value:
    
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
