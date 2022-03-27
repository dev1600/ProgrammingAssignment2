
# Below function creates the special matrix 
# 1.sets the element
# 2.get the element
# 3.set the element of inverse of matrix
# 4. get the element of inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}

# Checks whether we have already calculated the inverse if not then uses function above defined calculates and caches it
cacheinverse <- function(x, ...) {
  
  inv <- x$get_inverse()
  if(!is.null(inv)) {
    message("Getting the cached data ...")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$set_inverse(inv)
  inv
}

my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
my_Matrix$get_inverse()
cacheinverse(my_Matrix)

