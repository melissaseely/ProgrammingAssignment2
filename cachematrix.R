## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) { # set matrix value
    x <<- y
    inv <<- NULL
  }
  get <- function() x #get matrix value
  setinverse <- function(inverse) inv <<- inverse #set inverse
  getinverse <- function() inv  #get inverse
  list(set=set, get=get, 
       setinverse=setinverse, 
       getinverse=getinverse)
}


## Write a short comment describing this function
#cacheSolve returns inverse matrix

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
