## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## It has 2 elements: x, input matrix, and inversa, the inverse of x if it
## has been calculated before.
## Example to verify the function:
## h<-matrix(c(2,5,1,3),nrow=2,ncol=2)
## matriz <- makeCacheMatrix(h)

makeCacheMatrix <- function(x = matrix()) {
  
  inversa<-NULL
  set<-function(y){
    x <<- y
    inversa <<- NULL
  }
  get<-function(){ x }
  setinverse<-function(solve) inversa <<- solve 
  getinverse<-function() inversa
  
  list(set=set,
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above and actualize the value of inversa in the object "matrix".
## Example: Calling the function with the matrix defined before we will obtain the 
## inverse in the first call, and if we call again with the same matrix the value
## returned is not calculated but obtained from cache. In this case appears the
## message "getting cached data"
## > cacheSolve(matriz)
## [,1] [,2]
## [1,]    3   -1
## [2,]   -5    2
## > cacheSolve(matriz)
## getting cached data
## [,1] [,2]
## [1,]    3   -1
## [2,]   -5    2

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getinverse()
  if(!is.null(inversa)){
    message ("getting cached data")
    return (inversa)
  }
  data <- x$get()
  inversa <- solve(data,...)
  x$setinverse(inversa)
  inversa
}