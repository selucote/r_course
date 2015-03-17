## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# In the first function, makeCacheMatrix, on one hand it's defined the object 
# which we focus on, and, on the other hand, we define what's the inverse of this
# object (in this case, the matrix). 
makeCacheMatrix <- function(x = matrix()) {

	m <- NULL
	
	def <- function(y) {
		x <<- y
		m <<- NULL
	} 
	cog <- function() x
	defsolv <- function(solve) m <<- solve
	cogsolv <- function() m
	list(def = def, cog = cog, defsolv =defsolv, cogsolv = cogsolv)

}


## Write a short comment describing this function
# Next function, cacheSolve, take the inverse of the matrix from previous
# function and whether it's the first time you launch the function, you will 
# see the inverse of the matrix, but if it's lunched a second time, the message
# will appear.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	m <- x$cogsolv()
      if(!is.null(m)) {
      	message("getting cached data")
      	return(m)
        }
	data <- x$cog()
      m <- solve(data, ...)
      x$defsolv(m)
      m
}
