## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse (=setCache).

makeCacheMatrix <- function(x = matrix()) {
## Set cache to null if no subfunctions are called        
        m <- NULL
## Set subfunction: set x to y and m to null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
## Returns the matrix stored in x
        get <- function() x

## Saves the inversed matrix to setCache
        setCache <- function(solve) m <<- solve

## Returns the cache
        getCache <- function() m

## List of all possible subfunctions (e.g. makeCacheMatrix$get), which can be called
        list(set = set, get = get,
             setCache = setCache,
             getCache = getCache)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## Fetches the cache         
        m <- x$getCache()
## Checks that if the cache is null. If not, returns message that we're using cached data
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }

## Sets data to be the matrix
        data <- x$get()
## Calculates the inverse of the matrix
        m <- solve(data, ...)

## Sets the cache to the calculated inverse of matrix
        x$setCache(m)

## Returns the inverse
        m
}
