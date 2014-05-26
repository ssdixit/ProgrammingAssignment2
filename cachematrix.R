## Matrix inversion is usually a costly computation and instead of computing it repeatedly,  we may  cache the inverse of a matrix.
## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## Function cacheSolve: If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache.

## Function makeCacheMatrix: return a list of functions to:
## 1. Setup the matrix
## 2. Get the matrix
## 3. Set the inversematrix
## 4. Get the inversematrix
makeCacheMatrix <- function(x = matrix()) {

        # inversematrix will store the cached inverse matrix 
        m <- NULL 
        
        # Set up the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # Get the matrix
        get <- function() x
        
        # Set the inverse matrix
        setinverse <- function(solve) m <<- solve
        
        # Get the inverse matrix
        getinverse <- function() m
        
        # Returns the matrix with functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


# Function cacheSolve: computes the inverse of the matrix. 
# If the inverse is already calculated, it would retrive from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        # If the inverse is already calculated, return it
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # Else the inverse is not calculated yet, calculate it now
        data <- x$get()
        m <- solve(data, ...)
        # Cache the inverse
        x$setinverse(m)
        # Return inverse
        m

}
