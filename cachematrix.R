## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	mat_inv <- NULL
	set <- function(y)
	{
		x <<- y
		mat_inv <<- NULL
	}

  get <- function() x
  setinv <- function(m_inv) mat_inv <<- m_inv
  getinv <- function() mat_inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	mat_inv <- x$getinv()
  if(!is.null(mat_inv))
  {
    message("Getting data from cache...!")
    return(mat_inv)
  }
  data <- x$get()
  mat_inv <- ginv(data)
  x$setinv(mat_inv)
  
  mat_inv
}
