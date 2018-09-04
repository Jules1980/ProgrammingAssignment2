## makeCacheMatric - Julia Hamilton - R Programming - Week 3 Assignment

#1. set the value of the maxtrix
#2. get the value of the matrix
#3. set the inverse of the matrix
#4. get the get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  invs <- NULL
  
  set <- function(y) {
    
    x <<- y
    invs <<- NULL
    
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() invs
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}


## computes the inverse of the matric returned by makeCacheMatrix 
## if the inverse has already been calculated then the cachesolve should retrieve the inverse 
  ## from the cache

cacheSolve <- function(x, ...) {
  
    invs <- x$getinverse()
    
    if(!is.null(invs)){
    message("getting cache data.")
      return(invs)
      
    }
    
    data <- x$get()
    
    invs <- solve(data)
    
    x$setinverse(invs)
    
    invs
}
