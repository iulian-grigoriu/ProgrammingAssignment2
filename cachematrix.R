##Functions support creating a matrix

## Function creates a structure adding a matrix
## and his inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  invMatr <<- NULL
  
  set <- function(y) {
    x <<- y
    invMatr <<- NULL
  }
  
  get <- function() x
  getInverse <- function() invMatr
  setInverse <- function(invers_Matr) invMatr <<- invers_Matr
  
  list(set = set, get = get, 
       getInverse = getInverse, 
       setInverse = setInverse)
}


## Returns the inverse of cached matrix x
## If isn't exists, creates the inverse for caching
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invMatr <- x$getInverse()
  if (is.null(invMatr)) {
    data <- x$get()
    x$setInverse(solve(data))
  }
  
  invMatr <- x$getInverse()
}

