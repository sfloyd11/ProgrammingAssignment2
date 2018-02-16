## In these functions, I am hoping to save some time computing the inverse of a matrix by first caching the inverse with the makeCacheMatrix function, 
## then the second function, cacheSolve calculates the inverse and if already stored from the first function, retrieves it and uses it. 

## creates a matrix that caches its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(y){
      x<<-y
      inv<<-NULL
    }
    get<- function() x
    setinv<-function(inverse) inv <<-inverse
    getinv<-function() inv
    list(set=set, get=get,
         setinv=setinv, getinv=getinv)
}


## looks for the cached inverse, if it finds it, uses it to calculate the inverse

cacheSolve <- function(x, ...) {
  inv<- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data<- x$get()
  inv<- solve(data, ...)
  x$setinv(inv)
  inv
}



