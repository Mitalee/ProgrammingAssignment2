## OVERALL DESCRIPTION: The first function below requires an input of an inversible matrix 
## so that it can create a "matrix" object. This object consists of the original matrix, the inverse 
## of the matrix, and four functions that get and set the original and inverse matrices appropriately.
## The second function is a wrapper function that uses the first function to retrieve the inverse matrix.
## Since R uses lexical scoping, the first function takes advantage of the same by allowing certain 
## variables (the original as well as inverse matrices) to be modified in 2 environments, without being 
## visible in the global environment, effectively creating a level of encapsulation.


#Description of makeCacheMatrix() function
#x is a matrix obtained by the user in the environment of makeCacheMatrix (let's say Env A). 
#'matinv' is the object used to store the inverse matrix object and is defined in Env A as well.
#Four functions are defined for setting and getting the original matrix and setting and getting the
#inverse matrix as well.
# Since the four functions above will operate in a different environment( say Env B), we need the 
#values of 'matinv' and 'x' to persist in both Env A and Env B. Thus the '<<-' operator is used in 
#conjunction with the four 'closure' functions (i.e., functions that can modify the parent function's 
#variables in Env A)

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL 
  
  setmat <- function(orig) {
        x <<- orig #x will persist in the makeCacheMatrix function's environment as well
        mat <<- NULL # initially inverse matrix set to NULL
       }
  
  getmat <- function() x #return the original matrix 
  
  setinvmat <- function(inverse) matinv <<- inverse #matinv will persist in the parent environment
  getinvmat <- function() matinv #return the inverse matrix
  
  list(setmat=setmat, getmat=getmat, setinvmat=setinvmat, getinvmat=getinvmat)

}


##Description of cacheSolve() function

#this function takes in an matrix object created by the makeCacheMatrix function above
#and tries to get the inverse matrix by invoking the getinvmat() function
#
#

cacheSolve <- function(x, ...) {
  
  matinv <- x$getinvmat() #x is the matrix object (with 4 functions) created by the function above when 
  if(!is.null(matinv)) { #the original matrix was given in matrix form 
    message("Cached value of inverse matrix obtained") 
  }
    data <- x$getmat() #get the original matrix through the constructor function
    matinv <- solve(data,...) #compute the inverse of the matrix
    x$setinvmat(matinv) #feed the value to be stored in the cache (parent as well as child environments)
    matinv
}