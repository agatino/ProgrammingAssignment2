## created by Agatino Grillo 
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
  x<<-y
  m<<-NULL
}
get<-function() x
setmatrix<-function(solve) m<<- solve
getmatrix<-function() m
list(set=set, get=get,
   setmatrix=setmatrix,
   getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
    m<-x$getmatrix()
    if(!is.null(m)){
      message("getting cached data")
      return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
cacheSolve <- function (x=matrix(), ...) {
# Need to compare matrix to what was there before!
m <- x$getinverse() # if an inverse has already been calculated this gets it
if(!is.null(m)){ # check to see if cacheSolve has been run before
    if(x$setmatrix() == x$getmatrix()) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix
        #parts removed
    return(m)
    }
# otherwise 
y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
x$setmatrix(y) # run the setmatrix function on the input matrix to cache it
m <- solve(y, ...) # compute the value of the inverse of the input matrix
x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
m # return the inverse

