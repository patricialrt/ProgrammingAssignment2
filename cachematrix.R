## The assignment is to write a pair of functions that cache the inverse of a matrix. 
## To achieve this I copied the example (makeVector and cacheMean) 
## and adapted it to the purpose of the assignment. 


## Function 1: makeCacheMatrix:
## setting and getting the value of the matrix
## setting and getting the value of the inversion

makeCacheMatrix <- function(x=matrix()){
	
  # store in here the result of the inversion:
  mInv <- NULL  		
  
  # setter function, to set matrix to object created by makeCacheMatrix:
  set <- function(y) {	    x <<- y
    mInv <<- NULL
  }
  # return input matrix, set and get the inversed matrix and return a list containing these functions
  get <- function() x
  setInv <- function(inv) mInv <<- inv
  getInv <- function () mInv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Function 2: cacheSolve: 
## checking whether the inversion has already been calculated 
##    - if yes and the matrix has not been changed: skip computation, retrieve and return the inverse from the cache
##    - if no calculate inversion of the matrix and set the inversion in the cache via setInv

  cacheSolve <-  function(x, ...) {
  	# get the inversed matrix from x
    m <- x$getInv()
    # if available return calculated version
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    # if not get x, solve, set and return the result
    data <- x$get()
    m <- solve(data)
    x$setInv(m)
    m
}

