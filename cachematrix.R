## Two functions are in this files. One can creat a special Matrix,
## and cache it. The other one can compute the inverse of the matrix
## ,if the inverse has already been calculated, then the function
## should retrieve the inverse from the cache

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
             x<<-y
             m<<-NULL
        }
        get<-function() x
        setMatrix<-function(solve) m<<-solve
        getMatrix<-function()m
        list(set=set,get=get,setMatrix=setMatrix,getMatrix=getMatrix)
}


## compute the inverse of the matrix
## ,if the inverse has already been calculated, then the function
## should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m<-x$getMatrix() 
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setMatrix(m)
  m
   ## Return a matrix that is the inverse of 'x'
}
