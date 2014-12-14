makeCacheMatrix <- function(orig.matr = matrix()) {
  
  if (!is.matrix(orig.matr)) {
    stop("It is not the matrix")
  }
  
  inv.matr <- NULL
  
  set <- function(y) {
    orig.matr <<- y
    inv.matr <<- NULL
  }
  
  get <- function() orig.matr
  set.inverse <- function(solve) inv.matr <<- solve
  get.inverse <- function() inv.matr
  
  list(
    set = set, 
    get = get,
    set.inverse = set.inverse,
    get.inverse = get.inverse)
  
}


cacheSolve <- function(cach.matr, ...) {
  inv.matr <- cach.matr$get.inverse()
  if(!is.null(inv.matr)) {
    message("Getting cached inverse matrix")
    return(inv.matr)
  }
  matrix.to.inverse <- cach.matr$get()
  inv.matr <- solve(matrix.to.inverse)
  cach.matr$set.inverse(inv.matr)
  inv.matr
}
