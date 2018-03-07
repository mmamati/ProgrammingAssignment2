## Functions to compute the inverse of a square matrix.  Assumed that entered matrices are invertible.
## Combined, the functions accept a square matrix and return its inverse.  However, inverses are stored in cache
## so that if the inverse of the same matrix is requested again, before the cache is replaced, the inverse from
## cache is returned, avoiding the computation of the inverse.  


## makeCacheMatrix accepts a standard square matrix, assumed to be invertible, and returns a list with 
## four functions:
##    set:  stores the current matrix in the parent environment
##    get:  retrieves the current matrix from the parent environmen
##    setinv:   sets the inverse of the matrix into the parent environment
##    getinv:   retrieves the inverse of the current matrix from the parent environment

makeCacheMatrix <- function(M = matrix()) {  #M is a square matrix, assumed to be invertible
  inv<- NULL  # this is setting the inverse, inv, to null in the makeCacheMatrix environment
  set <- function(y) {
    M<<- y  ## M the matrix of interest,  is assigned to y,in the parent environment
    inv <<- NULL # assign the value of NULL to the inv object in the parent environment, clearing any value of inv from a previous run of cacheinverse().
  }
  get <- function() M  # this retrieves M from the parent environment, indicated by M on the outside of the ()
  setinverse <- function(inverse) inv <<- inverse  #creates a function that calculates the inverse of M.  Not completely following the outside the () statement.
  getinverse <- function() inv # retrieves the inverse from the parent environment
  # Here a list is returned with the 4 functions, specific to matrix M
  list(set = set, #sets the set funciton to set for output
       get = get, #etc... 
       setinverse = setinverse,
       getinverse = getinverse)
  
}



## Cache solve takes a list of tye type returned from makeCacheMatrix and returns the inverse.  If the inverse is in cache, it uses that version.
## Otherwise it calls the $setinverse() function from the input list. 

cacheSolve <- function(makeCacheList, ...) {
        
  inv <- makeCacheList$getinverse()  # retrieves the inverse in cache
  if(!is.null(inv)) {
    message("getting cached data")  # if the inverse is present in cache, return it and exit the function
    return(inv)
  }
  data <- makeCacheList$get()  # execution will only reach here if cache was empty.  Now retrieve the matrix we care about.
  inv <- solve(data) #calculate the inverse
  makeCacheList$setinverse(inv) #set the inverse into cache to check if called again.
  inv # return the inverse just calculated
}
