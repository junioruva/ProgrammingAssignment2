## These functions store the value of a matrix to cache
## and then calculate the inverse of that matrix

## makeCacheMatrix takes the value for a variable and
## stores it to cache

makeCacheMatrix <- function(x = matrix(,2,2)) {
  m <-NULL
  set <-function(y) {
    x<<-y
    m<<-NULL
  }
  get <-function() x 
  setmatrix <-function(matrix) m <<-matrix
  getmatrix <-function() m
  list(set=set,get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}
  

## cacheSolve retrieves the stored value from makeCacheMatrix
## it then calculates the inverse, and returns a matrix
## of the inverse

cacheSolve <- function(x,...) {
  m <-x$getmatrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <-x$get()
  m <-solve(matrix(data,nrow=2,ncol=2))
  x$setmatrix(m)
  m  
}
