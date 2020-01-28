## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {

 inverseMatrix <- NULL
 
 set <- function(y)
 {
   x <<- y
   inverseMatrix <<- NULL
 }
 get <- function() x
 
 setInverseMatrix <- function(solveMatrix) inverseMatrix <<- solveMatrix
 
 getInverseMatrix <- function() inverseMatrix
 
 list(set = set, get = get, setInverseMatrix = setInverseMatrix, getInverseMatrix = 
        getInverseMatrix)
}


##In this function, we're checking whether the inverse matrix has already been calculated

##If the matrix is not a null value,it's already been calculated and we have to return the cached data

##Else, we have to solve for the value of the inverse matrix and set the value

cacheSolve <- function(x, ...) {
  
  inverseMatrix <- x$getInverseMatrix()
  
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    return(inverseMatrix)
  }
  
  data <- x$get()
  inverseMatrix <- solve(data)
  
  x$setInverseMatrix(inverseMatrix)
  
  inverseMatrix
}
