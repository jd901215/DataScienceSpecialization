## This asignment is intended to show an example on 
## how to cache possible time-consuming functions (i.e the inverse of a matrix)
## using the advantage provided for lexical scoping


#' makeCacheMatrix
#'
#' @param x An invertible matrix
#'
#' @return A list with setters and getters needed in cacheSolve()
#' @export
#'
#' @examples
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



#' Title
#'
#' @param x A matrix created with the makeCacheMatrix() function
#' @param ... 
#'
#' @return Return a matrix that is the inverse of 'X'
#' @export
#'
#' @examples
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}


