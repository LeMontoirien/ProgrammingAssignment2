
## create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y){
    
    x <<- y  
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
  
}


## Check to see the inverse matrix is already in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inv = x$getinv()
  
  if(!is.null(inv)){
    
    message("getting cached data")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setinv(inv)
  
  return(inv)
  
}
##> x<-rbind(c(1, -2), c(-2, 1))
##> m <- makeCacheMatrix(x)
##> m$get()
##[,1] [,2]
##[1,]    1   -2
##[2,]   -2    1
##> cacheSolve(m)
##[,1]       [,2]
##[1,] -0.3333333 -0.6666667
##[2,] -0.6666667 -0.3333333
##> cacheSolve(m)
##getting cached data
##[,1]       [,2]
##[1,] -0.3333333 -0.6666667
##[2,] -0.6666667 -0.3333333
## not sure if it worked would have to check compute time but it printed getting cached data
