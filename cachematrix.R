## makeCacheMatrix creates a new "special" matrix 
## container that supports functions to set, get and
## compute inverse of matrix
## 


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  invMtx <- NULL
  
  set <- function(y) {
    x <<- y
    invMtx <<- NULL 
  }
  
  get <- function() x
  
  setInv <- function(Inv) invMtx <<- Inv
  
  getInv <- function () invMtx
  
  list(set=set, get=get,
       setInv = setInv,
       getInv = getInv)
  
}


## cacheSolve computes inverse of a matrix, if not
## already computed. If computed returns value in cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invrs <- x$getInv()
  
  if (!is.null(invrs)) {
    message ("getting cached data")
    return(invrs)
  }
  
  data <- x$get()
  
  invrs <- solve(data)
  x$setInv(invrs)
  
  invrs  
}
