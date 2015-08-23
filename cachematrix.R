## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function is a funtional, which is a function that encompases
## a number of other functions.  These sub-functions can then be called 
## by using the $ operator.

makeCacheMatrix <- function(x = matrix()) {
  ## first we set the inverse of the matrix to Null.  Thereby eliminating the possibility
  ## of providing the wrong inverse 
  mInverse<-NULL
  ## the "set" function will then assign the value of the matrix to the xtrnlMatrix
  ## variable.  Since set is a sub function, we need to sue the "<<-" assignation to
  ## to ensure that values are assigned to the variables in the parent function space.
  set <-function(y){
    xtrnlMatrix<<-y
    mInverse<<-NULL
  }
  ## The "get" sub function just displays the matrix in the xtrnlMatrix variable.
  get <-function() xtrnlMatrix
  ## The "setinverse" sub-function actually execute the built-in "solve" R command.
  ## To ensure that the value of the inverse is not stored in the sub-function,
  ## the value is assigned to the parent variable "mInverse".
  setinverse <- function(solve) mInverse<<-solve
  ## The "getinverse" is used to get the inverse of a mastix if the matrix in the parent
  ## "xtrnlMatrix" variable has not changed.
  getinverse <- function() mInverse
  ## The list function just enumerates the available sub-functions of "makeCacheMatrix"
  list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function

##The "cacheSolve" function provides the value of the inverse of the matrix submitted to it.
##This function will first check if the mInverse value exists, if not , then it will
## execute the "solve" command
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## First get a copy of the "mInverse" and save it to the local variable of the same name
  mInverse <-x$getinverse()
  ## Now we verify if the local copy of "mInverse" has some data
  if(!is.null(mInverse)){
    ## if "mInverse" contains data, then we display it and save the compute time.
    message ("getting inverse of matrix from cached data")
    ## by issuing the "return" command, we by pass the next set of commands and exit
    ## this function.
    return(mInverse)
  }
  ## if the local copy of "mInverse" is NULL, then we solve for the inverse.
  mInverse<-solve(x$get())
  ## we set the value of the inverse in the parent variable using the "setinverse" sub function
  ## of "makeCacheMatrix"
  x$setinverse(mInverse)
  ## displays the value of "mInverse"
  mInverse
}
