## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##makeCacheMatrix function makes a matrix object which can store
##the cache of the matrix inverse(mtxinv). The function sets the 
## matrix value to null, sets the inverse and gets the inverse 
##of the matrix
makeCacheMatrix <- function(x = matrix()) {
  mtxinv <- NULL
  set <- function(y){
    x <<- y
    mtxinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mtxinv <<- inverse
  getinverse <- function() mtxinv
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
  

}


## Write a short comment describing this function
##cacheSolve checks if the inverse of the matrix already exists in cache
## if it exists it, it gets the data from cache, else,
##it calculates the inverse and sets this as the new inverse
##in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mtxinv <- x$getinverse()
  if(!is.null(mtxinv)){
    message("Getting cached data")
    return(mtxinv)
  }
  data <- x$get()
  mtxinv <- solve(data, ...)
  x$setinverse(mtxinv)
  mtxinv
}

##Create a 2x2 test matrix to test above function
mymatrix <- matrix(c(1:4),2,2)
mymatrix
testcache <- makeCacheMatrix(mymatrix)
testinv <- cacheSolve(testcache)
testinv