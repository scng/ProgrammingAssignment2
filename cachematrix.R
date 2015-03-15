## Put comments here that give an overall description of what your
## functions do
#### The functions accept a matrix as input, calculate the 
#### "inversed matrix" and cache it for the first time.
#### For the second time if the same matrix is passed in, 
#### the cached "inversed matrix" will be returned instead of 
#### repeating the calculations again

## Write a short comment describing this function
#### this function accepts a matrix as input, 
#### and create a special matrix object that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # store inputted matrix, and reset the cached inverse matrix
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  
  # cache the inverse of the inputted matrix
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## Write a short comment describing this function
#### this function accepts a special matrix created 
#### from makeCacheMatrix function as input,
#### and return the inverse of it.
#### if the inverse of this matrix has been calculated before,
#### the cached inverse will be returned instead
cacheSolve <- function(x, ...) {
  # Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  # if the inverse of x was cached before
  # return the cached inverse
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  
  # Otherwise, calculate the inversed matrix, and store in cache
  inv <- solve(data, ...)
  x$setInverse(inv)
  # return the inversed matrix 
  inv
}

#### test case:
## source("cachematrix.R")
## A <- matrix(c(1,2,3,0,1,5,5,6,0), nrow=3, ncol=3)
## invA <- solve(A)
## cA <- makeCacheMatrix(A)
#### csA below should contain an inverse matrix 
#### with the same values as invA
## csA <- cacheSolve(cA)
## csA == invA
#### below should print the message "getting cached data"
#### and ccsA should be the same inverseMatrix values as csA
## ccsA <- cacheSolve(cA)
## ccsA == csA
