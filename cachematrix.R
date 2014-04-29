## Coursera R Programming -Programming Assignment 2 
## Finding the matrix inverse and caching the result
## 04/27/2014  CD



## makeCacheMatrix : Creates matrix object that cache its inverse

## Note ** The function assumes that the matrix supplied is always invertible.
## functions in makeCacheMatrix::
##1.  set: set the value of the matrix 
##2.  get: get the value of the matrix 
##3.  setInverse: set the value of the matrix inverse
##4.  getInverse: get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

  i<-NULL
  set <- function(y=matrix()) {
    x<<-y
    i <<- NULL
  }
  get<- function() x 
  setInverse <- function(x) {
    i <<- solve(x)
  }
  getInverse <- function() i
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}

## Returns inverse of a matrix from cache if exists
## Note** The function assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
       i<-x$getInverse()
       if(!is.null(i)) {
         message("getting cached matrix inverse")
         return(i)
       }
       data<-x$get()
       i<-solve(data, ...)
       x$setInverse(i)
       i
}


##test
m <- makeCacheMatrix(matrix(3:6,2)) 
i <- cacheSolve(m)
i
i <- cacheSolve(m)
i


m <-makeCacheMatrix(matrix(c(4,7,2,6), 2,2))
i <-cacheSolve(m)
i
i <-cacheSolve(m)
i
