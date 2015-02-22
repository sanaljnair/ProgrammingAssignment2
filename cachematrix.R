## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. Below functions can be used to cache the inverse of a matrix in to a list.

## This fucntion will take the input matrix and create a list whcich can be input to cacheSolve function. 
## asumption: input matrix will be invertible. no special checks to validate the same is being done below.  

makeCacheMatrix <- function(x = matrix()) {
  
  mInv <- NULL
  
  ## set function: Set the value of the Matrix in parent environment. used only in makeCacheMatrix fcntion to set the output list
  set <- function(y){
      x    <<- y
      mInv <<- NULL
  }
  
  ## get function: Get the input metrix from the object. In cacheSolve fucntion, this fucntion is used to get the value of 
  ## input matrix for caclulating the inverse of the matrix for the first time.
  
  get <- function() x
  
  
  ## setInv function: In cacheSolve function, this function is used to set the inverse of the matrix in cache for the first time. 
  ## there after the cacheSolve function will use the getInv function to fetch the inverse of the matrix from the cache. 
  
  setInv <- function(inv) {
    mInv <<- inv
  }
  
  ## getInv function: this function will be used in cachesolve function to get the inverse matrix if already present in the cache.
  
  getInv <- function() mInv
  
  
  ## matrix cannot be passed directly to cacheSolve fucntion. the makeCacheMatrix fucntion has to be called to prepare the input for 
  ## for cacheSolve which is a list
  
  list(set=set,get=get,setInv=setInv,getInv=getInv)
  
}


## The below function will calculate the inverse of a matrix. if the inverse is already calculated for the matrix, then the 
## inverse will be returned from cache 

cacheSolve <- function(x, ...) {
  ## use getInv function to get the value of Inverse matrix from cache. if the value was never calculated before it will be null. 
      
    cInv <- x$getInv()
    
  ## if the value for  cInv is not null (i.e. if value was calculated before), read from cache.
  if (!is.null(cInv)){
    message("getting inverse matrix from cached data")
    return(cInv)
  }
  
  ## if inverse for the matrix was never calculated before, get the orignal input matrix anf find the inverse of matrix. 
  data <- x$get()
  cInv <- solve(data,...)
  
  ## set the inverse matrix to parent environment
  x$setInv(cInv)
  cInv
  
  ## Return a matrix that is the inverse of 'x'
        
        
}
