##  This couple of functions will compute or cache the inverse of a matrix.
##  The first function, makeCacheMatrix(), will build a set of functions and will
##  return those functions to the parent environment by means of a list.
##  Then, the second function, cacheSolve(), uses the argument returned by
##  makeCacheMatrix() in order to retrieve the inverse of the matrix from the
##  cached value stored in the makeCacheMatrix() environment, or compute the
##  inverse of the matrix by means of the solve() function.

##  makeCacheMatrix() function
##  makeCacheMatrix will build a set of functions (set, get, setInvMat and getInvMat)
##  and will return those functions within a list to parent environment

makeCacheMatrix <- function(x = matrix()) {  ## x is initialized as an empty matrix
  mInv <- NULL  # mInv is also initialized as NULL
  ## Definition of functions
  ## 1. Definition of set
  set <- function(y) {
    x <<- y		## assign te input argument 'y' to object 'x' in
    ## parent environment
    mInv <<- NULL	## assign value NULL to mInv in parent environment
  }
  ## 2. Definition of get.  The x outside parenthesis means it is retrieved
  ##                        from the parent environment
  get <- function() x
  ## 3. Definition of setInvMat and getInvMat
  ## mInv is defined in parent environment and we need to access it after
  ## setinvMat() completes; <<- assings argument solveMatrix to mInv in
  ## parent environment
  setInvMat <- function(solveMatrix) mInv <<- solveMatrix
  getInvMat <- function() mInv  ## The mInv outside parenthesis means it is
  ## retrieved from the parent environment
  ##  list containing the functions
  list(set = set, get = get,
       setInvMat = setInvMat,
       getInvMat = getInvMat)
}


## cacheSolve() function
## cacheSolve() is required to compute or get the Inverse Matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInv <- x$getInvMat()  ## attempts to retrieve de Inverse Matrix from the
  ## passed object
  if(!is.null(mInv)) {
    message("getting cached inverse matrix")
    return(mInv)
    ## since makeMatrix sets the inverse matrix (mInv) to NULL, whenever
    ## a new matrix is set into the object it checks whether it is not NULL,
    ## and if so, it returns the cached inverse matrix
  }
  ## if mInv is NULL, cacheSolve gets the matrix and computes the Inverse
  ## Matrix by invoking the solve() function
  data <- x$get()
  mInv <- solve(data)
  x$setInvMat(mInv)
  mInv
}
