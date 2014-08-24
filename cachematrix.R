## Objective of the function - Creates a matrix and stores it in a cache for easy resue
## Input - Matrix
## Output - list of functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  set_matrix <- function(matrix) m <<- matrix
  get_matrix <- function() m
  list(set = set, get = get,
       set_matrix = set_matrix,
       get_matrix = get_matrix)

}


## Objective of the function - Calculates the inverse of the matrix for the first time
## Input - matrix whose inverse needs to be solved
## Output - inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_matrix()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$set_matrix(m)
  m
}
