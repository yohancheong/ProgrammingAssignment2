
## Creat a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {  
  
  ## Initialise i to store the inverse of a matrix
  i <- NULL
  
  ## Function to set vectors in special matrix object
  set <- function(y){
    
    ## x, i will be cached within the object once assigned
    x <<- y     
    i <<- NULL  
  }
  
  ## Get a matrix passed as an argument
  get <- function() x         
  
  ## Set i (inverse of a matrix) and Cache
  setInverse <- function(inverse) i <<- inverse
  
  ## Get i cached
  getInverse <- function() i     
  
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}


## Compute the inverse of a matrix, or retrieve the inverse from the cache in object x
cacheSolve <- function(x) {
  
  ## Retrieve the inverse from the cache in object x
  i <- x$getInverse()
  
  ## If the value retrieved from the cache is not null, then return it
  if(!is.null(i)){  
    message("getting cached data")
    return(i)
  }
  
  ## If the value retrieved from the cache is null, then start computing
  data <- x$get()
  
  ## Compute the inverse of square matrix
  i <- solve(data)  
  
  ## Assign the inverse to the cache in object x
  x$setInverse(i)
  
  ## Return a matrix that is the inverse
  i   
  
}
