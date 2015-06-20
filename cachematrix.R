## There are two functions in this program that compute the inverse of a matrix. 
## If the inverse has already been calculated, then the program goes to the cache memory 
## and reads the value stored in the cache.


## Function 1: makeCacheMatrix
## This is a function that contains 4 functions in it and creates a list of 4 functions as its output. 
## It takes an invertible matrix as the input

makeCacheMatrix <- function(x = matrix()) 
  {
    m <- NULL
    set <- function(y) 
      {
        x <<- y
        m <<- NULL
      }
    
    get <- function() 
      x
  
    setinverse <- function(inverse) 
      m <<- inverse
    getinverse <- function() 
      m
    
## Output is a list of 4 functions that can be used by another function    
    
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  }

## Function 2: This function takes the output of the first function as its input and 
## calculates the inverse of the matrix. 

cacheinverse <- function(x, ...) 
  {

## Checks if the inverse of the matrix has been calculated. If yes, uses information already stored.
  m <- x$getinverse()
    if(!is.null(m)) 
      {
      message("getting cached data")
      return(m)
      }
## If there is no value in the cache, then it caculates the value and uses the first function to 
## assign this to the appropriate variable
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
  }

## Define a matrix and assign it to the variable assign_matrix 
assign_matrix = matrix(1:4, nrow = 2, ncol = 2)

## Assign the output of the first function to the variable a
a <- makeCacheMatrix(assign_matrix)

## Calculate the inverse of the matrix using the function below
cacheinverse (a)



