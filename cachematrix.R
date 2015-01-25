## This here example shows how a large matrix can be inverted and
## stored for reuse in subsequent functions

## makeCacheMatrix creates 4 functions to provide get and set methods for our matrix storage object
## We provide get, set methods for the matrix and corresponding methods for the inverted matrix.

makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function (y) {
    x <<- y
    invm <<- NULL
  }
   get <- function() x
   setinvm <- function(inverse) invm <<- inverse
   getinvm <- function() invm
   list(set = set, get=get,
        setinvm=setinvm,
        getinvm=getinvm)
}

## cacheSolve inverts a matrix and saves it in the outer function ina variable called invm.
## Each time cacheSolve runs subsequently, it uses the cache to provide the data.
## Expensive reusable calculations may be stored for reuse in R

cacheSolve <- function(x, ...) {

  invm <- x$getinvm()
  
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm) 
  }
  data <- x$get()
  invm <- solve(data)
  x$setinvm(invm)
  invm 
}



