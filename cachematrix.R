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
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Checks whether we have already calculated the inverse if not then uses function above defined calculates and caches it
cacheinverse <- function(x, ...) {
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix_to_invert <- x$get()
  inv <- solve(matrix_to_invert, ...)
  x$setinverse(inv)
  inv
}

my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_Matrix$get()
my_Matrix$getinverse()
cacheinverse(my_Matrix)
