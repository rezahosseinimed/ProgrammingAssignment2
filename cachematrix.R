####### Put comments here that give an overall description of what your
####### functions do ##########################################################

# The overall purpose of these two functions is to make the inverse of a matrix
# and store it in the cache for further use. The reason for doing this is that
# some calculations, including finding the inverse of a matrix, can be very
# resource-consuming in R. 


############## Write a short comment describing this function ##############

# The concept of R's lexical scoping is necessary here to fully understand how
# things work. The makeCacheMatrix function gets a matrix, x, as its input, and
# returns a list of four functions. These four functions are used to set and
# get the values of the other two objects created within the environment of
# makeCacheMatrix(), namely x (as a formal argument, representing the original
# matrix) and i (as a local variable, representing the inverse matrix). The
# makeCacheMatrix() does not calculate the inverse matrix itself, but allocates
# memory for caching the result of the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) i <<- inverse
  get_inverse <- function() i
  list(set = set,
       get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


############## Write a short comment describing this function ##############

# The cacheSolve function gets the returned value of the makeCacheMatrix
# function as its first formal argument. Then uses the get_inverse function
# created within makeCacheMatrix() to access the value of i (representing the
# inverse matrix). If a previously calculated inverse matrix has been assigned
# to i, then the cacheSolve() gives us a message indicating that it gets the
# cached data and returns the value of i. However, if i is NULL, the
# cacheSolve() will get the original matrix from makeCacheMatrix(), find the
# inverse matrix, put the result in the set_inverse function within
# makeCacheMatrix(), and return the inverse matrix.

# Every time we set another matrix using the set() function within
# makeCacheMatrix, we need to run the cacheSolve again to re-calculate and save
# the inverse matrix in cache.

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  i
}
