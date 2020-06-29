
## "makeCacheMatrix" creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
invmx<-NULL # a variable for inverse matrix
set = function(y) {
  # function to set a value of matrix
  x <<- y #assign a value to an object in an environment different from current 
  invmx <<- NULL
}
get = function() x #function to get the value of matrix
setinv = function(inverse) invmx <<- inverse # function to set a value of Inverse
getinv = function() invmx # function to get a value of Inverse
list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Takes special matrix object output from makeCacheMatrix function and creates
## inverse of original matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invmx = x$getinv() # using getinv function from list of special matrix object
  
  # if the inverse has been previously calculated
  if (!is.null(invmx)){
    # get it from the cache and skips the computation. 
    message("getting inverse from cached data")
    return(invmx)
  }
  
  # if inverse has not been previously calculated 
  Mxdata = x$get() # get the matrix data
  invmx = solve(Mxdata, ...) # calculate the inverse through solve
  
  # set the value of the inverse in the cache through  "setinv function.
  x$setinv(invmx)
  
  return(invmx)
  
}
