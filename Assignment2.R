##This function create a special 'matrix' Object that can cache its inverse. 
##This contains the following LIST:
##set the values of matrix, get the values of martix, set of values of solve 
##and getthe values of 
## solve

makeCacheMatrix <- function(x = matrix()) { 
  m <- NULL 
  set <- function(y) { 
    x <<- y 
    m <<- NULL 
  } 
  get <- function() x 
  setsolve <- function(solve) { 
    m <<- solve 
  } 
  getsolve <- function() m 
  list(set = set, get = get, 
       setsolve = setsolve, 
       getsolve = getsolve) 
} 

##This function computes inverse on the Matrix.
##If the inverse has alreadly been calculated,cachesolve 
##returns the inverse from the cache

cachesolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}

##Calling the Function and testing it by 
# defining the Matrix
k<-matrix(c(1,2,4,6),byrow=TRUE,nrow=2 )
k
z<-makeMatrix(k)
cachesolve(k)