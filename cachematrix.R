## Caching the inverse of a matrix

## First, the function makeCacheMatrix creates a special
## matrix, which is really a list containint a function to
## 1. set the value of the matrix, 2. get the value of the matrix
## 3. set the values of the inverse matrix,
## 4. get the values of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inv) m<<-solve
  getinv<-function() m
  list(set=set, get=get,
       setinv=setinv,
       getinv=getinv)

}


## The following function calculates the inverse matrix
## created with the above function. It first chechs to see
## if the inverse matrix has already been calculated.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m<-x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data, ...)
  x$setinv(m)
  m
}

