## makeCacheMatrix: creates a matrix object 'x' that can cache its inverse.
## It has four internal functions:
  ## set: to set the values of 'x'
  ## get: to get the values of 'x'
  ## set_inverse: to set the inverse of 'x'
  ## get_inverse: to get the inverse of 'x'

## Arguments:
## x -> a square invertible matrix 
makeCacheMatrix <- function(x = matrix())
{
  ## the inverse of the object starts as NULL
  inverse <- NULL     
  
  set <- function(y) 
  {
    ## When the matrix values are set, the inverse of the object is ste to NULL
    x <<- y                   
    inverse <<- NULL
  }
  
  ## function to get the values of 'x'
  get <- function() x        
  
  ## fucntion to set the inverse of 'x'
  set_inverse <- function(solve) inverse <<- solve
  
  ## fucntion to get the inverse of 'x'
  get_inverse <- function() inverse                
  
  list(set = set, get = get,   
       set_inverse = set_inverse,
       get_inverse = get_inverse)

}

## cacheSolve: This function computes the inverse of the special matrix 'x' 
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache.

## Arguments:
## x -> an instance of makeCacheMatrix

cacheSolve <- function(x, ...) 
{
  ## caling get_inverse to see if the inverse of x has already been solved
  inverse <- x$get_inverse()
  
  if(!is.null(inverse)) 
  {
    ## If the inverse of x has already been solved then 
    ## it should retrieve the inverse from the cache:
    message("getting cached data")
    return(inverse)
  }
  
  ## If the inverse of x has not been solved yet 
  ## then I calculate it :
  data <- x$get()
  inverse <- solve(data, ...)
  x$set_inverse(inverse)
  
  ## I return a matrix that is the inverse of 'x'
  inverse
}
