## This program caches the inverse of a matrix x read and returns the inverse
## upon request.

## The function makeCacheMatrix reads a matrix x as input and construct a list
## of 4 elements list(set,get,setinv,getinv) each containing sub-functions
## respectively called:

## set: which sets the value of matrix x to be inverted
## get which gets the value of the matrix x to be inverted
## setinv: which sets the inverse of the matrix x
## getinv: which gets the inverse of the matrix x


makeCacheMatrix <- function(x = matrix()) {
  
  invx <- NULL
  
  ### sets the value of the matrix x
  set <- function(y) {
    y <<- x
    invx <<- NULL
  }
  
  ### gets the value of the matrix x
  get <- function() x
  
  ### sets the inverse of the matrix x
  setinv <- function(inv) invx <<- inv

  ### gets the inverse of the matrix x
  getinv <- function() invx
  
  ### return the constructed object as a list.
  list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {

  ### Return a matrix that is the inverse of 'x'
  invx <- x$getinv()
  if(!is.null(invx)){
    
    ### returns the cached value of the inverse of x
    message("getting matrix inverse from the cache")
    return(invx)
  }
  
  ### gets the matrix x stored in the list object constructed
  matrix <- x$get()
  ### calculates the inverse of the stored matrix x
  invx <- solve(matrix)
  ### sets the inverse of the matrix x in the list object
  x$setinv(invx)
  
  ### Returns the new computation of the inverse of x
  invx
  
}
