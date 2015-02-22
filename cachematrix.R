## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## The first function, makeVector creates a special "vector", which is really a list containing a function to

## setmatrix the value for a matrix
## getmatrix the value for a matrix
## setinverse set the inverse of a matrix
## getinverse get the inverse of a matrix

# SAMPLE RUN FOR THE BELOW CODE
# > mat1<-matrix(data=1:4,nrow=2,ncol=2)
# > mat1
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > mat2<-makeCacheMatrix(mat1)
# > mat2$getmatrix()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# > cacheSolve(mat2)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# > cacheSolve(mat2)
# Getting Pre-computed Cached Data.....
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  SetMatrix <- function(y) {        
    x <<- y 
    m <<- NULL
  }
  
  GetMatrix <- function() x 
  ## Computing the inverse of a square matrix can be done with the solve function in R. 
  ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
  SetInverse <- function(solve) m <<- solve
  GetInverse <- function() m
  
  list(setmatrix = SetMatrix, getmatrix = GetMatrix, setinverse = SetInverse, getinverse = GetInverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  ## Return a matrix that is the inverse of 'x'
  if(nrow(x$getmatrix())==ncol(x$getmatrix())) { # Find out if the matrix is a square matrix
    m <- x$getinverse() 
    if(!is.null(m)){ ## if matrix is not null then just get the pre computed cached data
      message("Getting Pre-computed Cached Data.....")
      return(m)
    }
    y <- x$getmatrix() 
    m <- solve(y)
    x$setinverse(m) 
    m 
  }
  else 
    print("Make sure the matrix provided is invertible, so supply a square matrix")
}



