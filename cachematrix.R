## The assignment is to write a pair of functions that cache the inverse of a matrix. 

## To achieve this I copied the example (makeVector and cacheMean) 
## and adapted it to the purpose of the assignment. 

## setting and getting the value of the matrix
## setting and getting the value of the inversion

makeCacheMatrix <- function(x=matrix()){
  mInv <- NULL
  set <- function(y) {
    x <<- y
    mInv <<- NULL
  }
  get <- function() x
  setInv <- function(inv) mInv <<- inv
  getInv <- function () mInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## checking whether the inversion has already been calculated 
##    - if yes and the matrix has not been  changed, skip computation and prompt message 'getting cached data' 
##      and retrieve the inverse from the cache
##    - if no calculate inversion of the matrix and set the inversion in the cache via setInv
  cacheSolve <-  function(x, ...) {
    m <- x$getInv()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
}

