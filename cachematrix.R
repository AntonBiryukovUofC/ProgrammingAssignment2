## Put comments here that give an overall description of what your
## functions do
## m -cached value
## makeCacheMatrix makes a "special" matrix with a cached inverse matrix 

makeCacheMatrix <- function(x = matrix()) {
  cached_value<- NULL
  set <- function(y) { # We set a new value, so cached value is yet undefined
    x <<- y
    cached_value <<- NULL # this is a cached value
  }
  get <- function() x
  set_inverse <- function(inverse_matrix) cached_value <<- inverse_matrix
  get_inverse <- function() cached_value
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## cacheSolve returns a matrix that is the inverse of the input matrix x
## If the inverse matrix exists in the cache - then it returns the cached value
## otherwise it calls the solve() function on the input and caches that value for 
## the future use

cacheSolve <- function(x, ...) { # Returns a matrix that is inverse of x
  cached_value <- x$get_inverse() # Try to fetch the value. If it does not exist - we get NULL
  if(!is.null(cached_value)) { # Does exist -> inform the user
    message("getting cached inverse matrix")
    return(cached_value)
  }
  # Otherwise, we do calculate the inverse:
  matrix_to_inverse <- x$get() # First get the matrix
  inverse_matrix <- solve(matrix_to_inverse, ...) # Solve for the inverse
  x$set_inverse(inverse_matrix)
  
}
