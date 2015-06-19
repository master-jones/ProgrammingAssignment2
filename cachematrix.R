# The following methods provide functionality to calculate 
# the inverse of a matrix or retrieve the result from the cache
# if the calculation has already taken place.

# Special function to manage the original matrix as well as 
# the calculated inverse matrix. Provides necessary getter 
# and setter methods and acts as a cache.
# 
# Args:
#   x: The original matrix from which to calculate the inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
  # Holds the calculated inverse matrix
  inverse_matrix <- NULL
  
  # Setter to update the original matrix and 
  # reset the old inverse matrix
  # 
  # Args:
  #   new_matrix: the new matrix
  set <- function(new_matrix) {
    x <<- new_matrix
    inverse_matrix <<- NULL
  }
  
  # Returns the current matrix 
  get <- function() x
  
  # Updates the inverse matrix object
  # 
  # Args:
  #   new_inverse_matrix: The newly calculated inverse matrix
  setinverse <- function(new_inverse_matrix) inverse_matrix <<- new_inverse_matrix
  
  # Returns the cached inverse matrix
  getinverse <- function() inverse_matrix
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


# If the inverse matrix has not been calculated it will
# calculate the inverse of the given matrix, otherwise 
# the inverse matrix will be retrieved from the cache.
#
# Args:
#   x: Special function (list) that holds the matrix and acts as cache
cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  
  # check if inverse matrix has already been calculated
  if(!is.null(inverse_matrix)) {
    message("getting cached inverse matrix")
    # return cached result
    return(inverse_matrix)
  }
  
  # get original matrix and calculate inverse matrix
  org_matrix <- x$get()
  inverse_matrix <- solve(org_matrix, ...)
  x$setinverse(inverse_matrix)
  # return inverse matrix
  inverse_matrix
}
