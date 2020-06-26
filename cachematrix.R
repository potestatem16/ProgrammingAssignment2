## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


#############################################
#    The next function can generate a 
#    special matrix, which is the 
#    object to take the inverse from.
#############################################
makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     declare_function <- function(y) {
          x <<- y
          inv <<- NULL
          }
     get_matrix <- function() x
     set_inverse_matrix <- function(inverse) inverse <<- inverse
     get_inverse_matrix <- function() inverse
     list(declare_function = declare_function,
          get_matrix = get_matrix,
          set_inverse_matrix = set_inverse_matrix,
          get_inverse_matrix = get_inverse_matrix)
}



## Write a short comment describing this function
#################################################
#    From the function above, which generates 
#    the special matrix object to treat, 
#    the function below returns the inverse 
#    matrix from the original one.
#################################################

cacheSolve <- function(x, ...) {
     inverse<-x$get_inverse_matrix()
     if(!is.null(inverse)){
          return(inverse)
     }
     compute<-x$get_matrix()
     inverse<-solve(compute, ...)
     x$set_inverse_matrix(inverse)
     list_to_return<-list(cat("This is the inverse matrix"),inverse)
     return(list_to_return)
      ## Return a matrix that is the inverse of 'x'
}

#################################################
#    Now, with the fuctions done, it's time to 
#    test them.
#################################################

m<-makeCacheMatrix(matrix(sample(1:9), 3,3, byrow = T))
m$get_matrix()
cacheSolve(m)
