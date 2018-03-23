## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix takes in a matrix and stores the information of this matrix. 
## In addition, makeCacheMatrix provides the following methods:
## 1. set(x): set a new matrix x to replace the originally stored matrix
## 2. get(): get access to the stored matrix
## 3. setInverse(i): store the inverse of the original matrix
## 4. getInverse(): get access to the stored inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  Inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) Inverse <- i
  getInverse <- function() Inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## cacheSolve takes in a makeCacheMatrix and returns its inverse.
## If makeCasheMatrix's inverse has been previously calculated, then cacheSolve
## will simply grab the inverse from makeCasheMatrix's cache and return it.
## Otherwise, cacheSolve runs solve() on the matrix to get its inverse and return it,
## and along the way, the inverse will be stored in makeCasheMatrix for future use.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if (!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
}
