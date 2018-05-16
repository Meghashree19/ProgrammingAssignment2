## R program to take input a square matrix and return the inverse of the matrix.

## Function makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) { ## This function changes the matrix stored in makeCacheMatrix 
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x ## This function returns a matrix x stored in makeCacheMatrix
  setinverse <- function(inverse) inverseMatrix <<- inverse ## This stores the value of the input in a variable inverseMatrix
  getinverse <- function() inverseMatrix ## return the value of the variable inverseMatrix
  list(set = set, get = get, ## store the above 4 functions in the makeCacheMatrix
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function cacheSolve computes the inverse of the special "matrix" if the cache is empty or it return the cahced data.

cacheSolve <- function(x, ...) {
  ## Returns a matrix which is a inverse of 'x'
  inverseMatrix <- x$getinverse()
  if(!is.null(inverseMatrix)) {
    message("The matrix is getting cached data")
    return(inverseMatrix)
  }
  data <- x$get()
  inverseMatrix <- solve(data, ...) ## Computing the inverse of the square matrix.
  x$setinverse(inverseMatrix)
  inverseMatrix
}

