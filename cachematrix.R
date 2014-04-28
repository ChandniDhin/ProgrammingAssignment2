## Coursera R Programming -Programming Assignment 2 
## Finding the matrix inverse and storing the result in cache
## 04/27/2014  CD

## makeCacheMatrix : Creates matrix object that cache its inverse

##1.  set the value of the matrix 
##2.  get the value of the matrix 
##3.  setinverse the value of the matrix inverse
##4.  getinverse the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
	
	i<-NULL
	set <- function(y=matrix()) {
		x<<-y
		i <<- NULL
	}
	get<- function() x 
	setinverse <- function(x) {
		i <<- solve(x)
	}
	getmean <- function() i
	
	
}

## Return a cache inverse matrix else solve for the inverse of 'x'
cacheSolve <- function(x, ...) {
	i<-x$getinverse()
	if(!is.null(i)) {
		message("getting cached matrix inverse")
		return(i)
	}
	data<-x$get()
	i<-solve(data)
	x$setinverse(i)
	i
}


##test

