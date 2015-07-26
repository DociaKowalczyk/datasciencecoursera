## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  x_inv = NULL #This is where the inverse will be stored 
  
  #The following is a setter function, is it used to set a matrix to an object  
  #created by my_Cache_matrix
  set <- function(y) {
    x <<- y
    inv_x <<- NULL #initialises inv_x to NULL
}
  get <- function()x #this returns the input of the matrix
  set_Inv <- function(inv) inv_x <<- inv #sets the inverse of the matrix 
  get_Inv <- function() inv_x #returns the inverse of the matrix 
  
  #The following returns a list that contains these functions, so that it
  #is possible to use makeCacheMatrix objects to get the setted matrix and
  #then set and get the inversed matrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$get_Inv() #this gets the inverse of the matrix from object x
  if(!is.null(m)) { #if the inverse result is present
    message("Getting the Cached Data")
    return(m) #return the calculated inverse
  }
  
  data <- x$get() #if the inverse result is not present, x$get gets the 
  #object matrix
  m <- solve(data) #solve calculates the inverse of the matrix
  x&set_Inv(m) #sets it to the object 
  m #returns the solved result
}
