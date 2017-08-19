## Overall these functions calculates inverse of a matrix and cache it.

## Function makeCacheMatrix stores the initial input (a matrix) and also caches the output if the input is
## used in cacheSolve to calculate the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<- function(y){
    x<<- y
    m<<- NULL
  }
  get<-function()x
  setInv<-function(inv) m<<-inv
  getInv<-function()m
  list(set=set,get=get,
       setInv=setInv,
       getInv=getInv)
}


## Function cacheSolve takes an input created by makeCacheMatrix function and outputs the inverse of the matrix
## if it's cache, if not it will calculate the inverse matrix and cache it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
}
