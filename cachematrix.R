## Put comments here that give an overall description of what your
## Functions: 
#     makeCacheMatrix() - takes one arg, a 2x2 matrix
#     cacheSolve() - takes one arg, the function makeCacheMatrix()
#  
#  BY:    Garrett Fletcher
#  Date:  10/29/2016 


## Write a short comment describing this function
#this funciton will take matrix. You can call the 
#function with no args and it will default to use a null matrix
#function will store a matrix, and the inverse of the matrix
#function will aslo return matrix, an its inverse.


makeCacheMatrix <- function(m = matrix()) {
  #set inverse matrix to null
  im <- NULL
  
  #define function set inverse or matrix
  set <- function(y) {
    m <<- y
    im <<- NULL
    
  }
  
  #defines function to return the starting matrix
  get <- function() m
  #defines function to set inverse matrix
  setInverse <- function(inv) im <<- inv
  #defines function to get inverse matrix
  getInverse <- function() im
  #returns list of functions for getting/setting matrix and inverse
  list(set = set, get = get, setInverse = setInverse, 
       getInverse = getInverse)
}


## takes the makeCacheMatrix function and calculates
#or returns the inverse of that matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    #look for cached inverse matrix
    im <- x$getInverse()
    #check to see if inverse matrix is calculated
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    #gets original matrix
    data <- x$get()
    #calculate inverse of matrix
    im <- solve(data)
    #set invers to cache
    x$setInverse(im)
    #return inverse of matrix
    im
}












