## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}  ##get the matrix
  setInverse<-function(inverse) inv<<-inverse  ##sets the inverse of the matrix
  getInverse<-function() inv  ##gets the inverse of the matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()   
  if(!is.null(inv)){                ##if the inverse value is not NULL then it returns the cached data
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()                   ##if the inverse value is NULL then it finds the inverse of the matrix
  inv<-solve(matrix,...)
  x$setInverse(inv)
  inv
}
