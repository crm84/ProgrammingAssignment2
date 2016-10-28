## These functions are used to create a special object that stores a matrix and caches its inverse.


## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
{

  cachedMatrix <- NULL
  set <- function(y)
  {
    x <<- y
    cachedMatrix <<- NULL
  }
  
  get<-function() x
  setInverseMatrix <- function(solveMatrix) cachedMatrix <<- solveMatrix
  getInverseMatrix <- function() cachedMatrix
  list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix )
  
  
}


## This function will return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) 
{
  thisMatrix <- x$getInverseMatrix()
  if(!is.null(thisMatrix))
  {
    return(thisMatrix)
  }
  
  d <- x$get()
  thisMatrix <- solve(d, ...)
  x$setInverseMatrix(thisMatrix)
  thisMatrix

}
