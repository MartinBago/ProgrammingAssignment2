makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
   
    x <<- y  ##assign a value in an different environment
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## makeCacheMatrix is creating a special object that can be invertd in cache

cacheSolve <- function(x, ...) {
  inv = x$getinv()
  
  
  if (!is.null(inv)){
    
    message("getting cached data")
    return(inv)
        ## Return an inverted matrix
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)   
}
