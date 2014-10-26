## The following functions caches the inverse of a Matrix
## functions do

## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

## Beginning the inverse property
im <- NULL
  
## Setting the matrix  
  set <- function(y) {
    x <<- y
    im <<- NULL
  }

## Getting the matrix
  get <- function() x

## Setting the inverse of the matrix 
  setinverse <- function(inverse) im <<- inverse

## Getting the inverse of the matrix
  getinverse <- function() im

## Returning the list of methods
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function below will compute the inverse of matrix returned by "makeCacheMatrix" above. 
## If the inverse has already been calculated (and if the matrix has not changed), 
## then the "cachesolve" function below  would retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

## Returning the matrix (inverse of 'x')
  im <- x$getinverse()

## Returning the inverse if its already available
  if(!is.null(im)) {
    message("getting cached data.")
    return(im)
  }

## Getting the matrix
  data <- x$get()

## Calculating inverse
  im <- solve(data)

## Setting the inverse to x
  x$setinverse(im)

## Returning the matrix 
  im
}

## Testing:
## x = rbind(c(1, 1/3), c(1/3, 1))
## m = makeCacheMatrix(x)
## m$get()

## No cache in the first run
## cacheSolve(m)

## Retrieving from the cache in the second run
## cacheSolve(m)
