## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Comment - Sanjoy Chowdhury 
## *** This function accepts a matrix as an input and returns a list of functions ***

makeCacheMatrix <- function(x = matrix()) {
	mat_inv <- NULL				## *** mat_inv stores the inverse of the matrix and it is initialised to NULL on every call to the makeCacheMatrix() function***
	set <- function(y)			## *** This function assigns the value of the matrix passed into the 'set' function to x ***
	{
		x <<- y
		mat_inv <<- NULL
	}

  get <- function() x			## *** This function returns the value of the matrix that was passed during the initial function call
  setinv <- function(m_inv) mat_inv <<- m_inv	## *** This function is used to store the value of the matrix inverse through superassignment *** 
  getinv <- function() mat_inv	## *** This function returns the cached value of the matrix inverse for calls to cacheSolve(), subsequent to the first call
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)	## *** This list is returned with a new object containing the functions which are a part of the list.
}

## Write a short comment describing this function

## Comment - Sanjoy Chowdhury 
## *** The input to this function is an object created by the makeCacheMatrix() function. It calculates the inverse of the matrix if not already calculated. Else it returns the inverse value from cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	mat_inv <- x$getinv()		## *** Gets the inverse matrix of the object 'x' ***
  if(!is.null(mat_inv))			## *** Checks to see if the inverse matrix exists or not ***
  {
    message("Getting data from cache...!")		## *** If the inverse of matrix 'x' exists, then it is called from cache and a message is displayed accordingly ***
    return(mat_inv)
  }
  data <- x$get()				## *** assign to 'data' the matrix passed into the makeCacheMatrix() function ***
  mat_inv <- solve(data)		## *** calculate the inverse of the matrix using solve() assuming that the 'matrix supplied is always invertible'; we can also use the ginv() function instead to calculate the inverse of the matrix. *** 
  x$setinv(mat_inv)				## *** this piece of the code will store the inverse matrix in the mat_inv object ***
  
  mat_inv						## *** returns the inverse matrix to the calling function ***
}
