## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix function, argument is x the matrix
## Creates a list containing a function to
##   1. Set the value of the matrix
##   2. Get the value of the matrix
##   3. Set the value of the inverse of the matrix
##   4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<- NULL
  
  set<-function(y) {
    x<<-y
    inv<<-NULL
    }
  
  get<-function()x
  setinverse<-function(inverse) inv<<-inverse
  getinverse<-function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## Write a short comment describing this function
##  cacheSolve function calculates the inverse of a matrix

##  First checks to see if inverse has already been calculated in cache
##  If so, gets the answer from cache and skips calculation
##  If not, calculates the inverse
##  then sets the value in cache for future calculations

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  inv<- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting data from cache")
    return(inv)
  }
  
  data<- x$get()
  inv<- solve(data)
  x$setinverse(inv)
  inv
}
