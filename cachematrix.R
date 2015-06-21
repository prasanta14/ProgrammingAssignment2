##Functions that can be used to cache the inverse of a matrix


##Create a matrix list containing a function to
  ##set the value of the matrix
  ##get the value of the matrix
  ##set the value of the inverse matrix
  ##get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse property
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


## Compute the inverse of the unique matrix

cacheSolve <- function(x=matrix(), ...) {
  ## Return a matrix that is the inverse
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  ## Compute the inverse via matrix multiplication
  m<-solve(matrix, ...)
  ## Set the inverse to the object
  x$setmatrix(m)
  ## Coming back to the matrix
  m
}

