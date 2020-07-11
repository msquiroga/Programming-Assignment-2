# Miller Santiago Quiroga
# Programming Assignment 2 
# Coursera R Programming
# Johns Hopkins University

# Function 1 --------------------------------------------------------------

# This function creates a matrix called mymatrix. Mymatrix is an object that
# can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # I start assumming that the matrix provided is invertible
  mymatrix <- NULL
  # Here I set the value of the matrix using another function
  set <- function(y){
    # The <<- allows us to have two levels of parameters for 2 different
    # functions
    x <<- y
    mymatrix <<- NULL
  }
  # Now I get the value of the matrix provided within the function
  get <- function() x
  # Here I set the value of the inverse of the matrix is set
  setinverse <- function(inverse) mymatrix <<- inverse
  # Now I get the value of the inverse and put it into the object created 
  # before "mymatrix" which was set as NULL
  getinverse <- function() mymatrix
  # Here I create a list to store the values set before
  list (set = set, get = get, setinverse = setinverse, getinverse=getinverse)
}


# Function 2 --------------------------------------------------------------

# This is a function that calculates the inverse of the matrix generated
# by makeCacheMatrix function

cacheSolve <- function(x, ...) {
  # Here I calculate the inverse of the matrix x and assign it to the
  # object "mymatrx"
  mymatrix <- x$getinverse()
  # Here we verify if the inverse was calculated before. If so, the 
  # process of calculating the inverse can be skipped
  if (!is.null(mymatrix)){
    message("getting cached data")
    return(mymatrix)
  }
  # If the inverse wasn't calculated before we have to calculate it
  mat <- x$get()
  mymatrix <- solve(mat, ...)
  # Now I set the inverse of the matrix ussing the setinverse function
  x$setinverse(mymatrix)
  # Finally the inverse is displayed in the console
  mymatrix
}


# Checking ----------------------------------------------------------------

mymatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(mymatrix)
cacheSolve(mymatrix)
