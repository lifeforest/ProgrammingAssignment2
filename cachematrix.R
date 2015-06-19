## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special vector that can:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse  
## 4. get the value of the inverse  
## 5. remove the value of the inverse (not required in the assignment)

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function () x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  
  releaseInverse <- function() {
    i <<- NULL
  } 
  
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse,
       releaseInverse = releaseInverse
  )
  

}


## This function takes a vector created by function makeCacheMatrix.
## It then check if the inverse has already been calculated. If so then 
## first print the sentence "getting cached data", then return the inverse.
## Otherwise, calcualte the inverse and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i 
}
