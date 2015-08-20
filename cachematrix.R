## Title: Caching the Inverse of a Matrix

## Often, it is a time-consuming to inverse a matrix. Therefore, it is more efficient to cache
## the inverse of a matrix rather than compute it repeatedly. With this regard, the
## below two functions were produced to store a matrix and cache its inverse.


# The intial function makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    set <- function(y) {
      
      x <<- y
      inv <<- NULL
      
    }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
    }


## The second function calculate the inverse of the specially created matrix
## by the function makeCacheMatrix. It first checks if the inverse has already been
## computed. If so, it should retrieve the inverse from the cache. Alternatively, it computes
## the inverse, sets the value in the cache via setinverse function.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  
  inv

  }


## Testing the functions:

Madi_matrix <- matrix(1:4, 2,2)

fun <- makeCacheMatrix(Madi_matrix)

fun$get()

##            [,1] [,2]
##      [1,]    1    3
##      [2,]    2    4

## Note, there is no cache in the first run.

cacheSolve(fun)

##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5

## Retrieving from the cache in the second run

cacheSolve(fun)

##getting cached data.
##        [,1] [,2]
##  [1,]   -2  1.5
##  [2,]    1 -0.5
