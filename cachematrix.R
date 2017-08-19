## Overall these functions calculates inverse of a matrix and cache it.

## Function makeCacheMatrix stores the initial input (a matrix) and also caches the output if the input is
## used in cacheSolve to calculate the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {   ##initiated x as matrix
  m<-NULL                                     ##initiated m as NULL
  set<- function(y){                        
    x<<- y                                    ##set the input to x in parent environment
    m<<- NULL                                 ##set m as NULL to reset m
  }
  get<-function()x                            ##get the input data to makeCacheMatrix
  setInv<-function(inv) m<<-inv               ##set the inverse matrix calculated from cacheSolve to m
  getInv<-function()m                         ##get the cached inverse matrix m
  list(set=set,get=get,                       ##set names for functions set,get,setInv,getInv so that they can
       setInv=setInv,                         ##be called using $
       getInv=getInv)
}


## Function cacheSolve takes an input created by makeCacheMatrix function and outputs the inverse of the matrix
## if it's cache, if not it will calculate the inverse matrix and cache it.
cacheSolve <- function(x, ...) {                   
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInv()                            ##get the cached inverse matrix m
  if(!is.null(m)) {                          ##if the cached inverse matrix is not NULL (means there is cached data)
    message("getting cached data")           ##"getting cached data" message will be shown followed by the inverse matrix
    return(m)
  }
  data <- x$get()                            ##if m==NULL, the input data will be called using x$get()
  m <- solve(data, ...)                      ##inverse matrix is calculated
  x$setInv(m)                                ##inverse matrix is stored in m using x$set()
  m                                          ##inverse matrix is displayed on the console.
}
