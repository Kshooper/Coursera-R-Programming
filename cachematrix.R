
## Simply put, this function creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {   # this defines the argument, setting x as a matrix so you can only pass a matrix through the function
  inv <- NULL                                 # initializes inv as NULL, so nothing, & will later hold the value of matrix inverse
  set <- function(y){                         # this defines the "set" function as new 
    x <<- y                                   # this places the value of the matrix in the parent environment
    inv <<- NULL                              # so that if there is new matrix, the inv is reset to NULL
  }
  get <- function() x                          # this defines the "get" function, which returns the value of the matrix
  setinv <- function(inverse) inv <<- inverse  # defines the "setinv" function and assigns the value of inv to the parent environment
  getinv <- function() inv                     # defines the "getinv" function and retrieves the value of inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)  # without this, we would not be able to refer to 
                                                                # functions with the $ operator, which is key

}


## this function takes the matrix created by makeCacheMatrix above and computes the inverse
## it also will retrieve the inverse of the matrix if it has already been calculated and the matrix is still the same
cacheSolve <- function(x, ...) {      # defines the function as cacheSolve
  inv <- x$getinv()                   # sets inv to the getinv function for x 
  if (!is.null(inv)) {                # if the inv is not NULL or has been calculated
    message("getting cached data")    # then give message "getting cached data"
    return (inv)                      # and return the inverse
  }
  data <- x$get()                     # if not, then get the value of the matrix x 
  inv <- solve(data, ...)             # compute the inverse of data, which is matrix x 
  x$setinv(inv)                       # sets the inverse of matrix x in the parent environment using setinv
  inv
  
}
