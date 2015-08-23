## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mInverse<-NULL
  set <-function(y){
    xtrnlMatrix<<-y
    mInverse<<-NULL
  }
  get <-function() xtrnlMatrix
  setinverse <- function(solve) mInverse<<-solve
  getinverse <- function() mInverse
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mInverse <-x$getinverse()
  if(!is.null(mInverse)){
    message ("getting inverse of matrix from cached data")
    return(mInverse)
  }
  mInverse<-solve(x$get())
  x$setinverse(mInverse)
  mInverse}
