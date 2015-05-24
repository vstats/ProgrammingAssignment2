##                                      @x: a square invertible matrix
##                                      return: a list containing functions to
makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  ##                                    set the matrix
  set = function(y) {
    ##                                  use `<<-` to assign a value to an object in an environment 
    x <<- y
    ##                                  different from the current environment. 
    inv <<- NULL
  }
  ##                                    get the matrix
  get = function() x
  ##                                    set the inverse
  setinv = function(inverse) inv <<- inverse 
  ##                                    get the inverse
  getinv = function() inv
  ##                                    this list is used as the input to cacheSolve()
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

##                                      Return a matrix that is the inverse of 'x'
##                                      @x: output of makeCacheMatrix()
##                                      return: inverse of the original matrix input to makeCacheMatrix()
cacheSolve <- function(x, ...) {
  
  inv = x$getinv()
  
  ##                                    if the inverse has already been calculated
  if (!is.null(inv)){
    ##                                  get it from the cache and skips the computation. 
    return(inv)
  }
  
  ##                                    otherwise, calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  ##                                    sets the value of the inverse in the cache via the setinv function.
  x$setinv(inv)
  
  return(inv)
}
