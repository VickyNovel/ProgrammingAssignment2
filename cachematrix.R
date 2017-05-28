## produces inverse of the input matrix and caches for further use.

##  creates out of a matrix a cacheable object (a list),
##  so that the inverse does not have to be recomputed every time.
##  setinverse stores the inverse of a matrix 
##  getinverse retrives the stored inverse of a matrix.


makeCacheMatrix <- function(x = matrix()){
     inv <- NULL
     set <- function(y){
              x <<- y
              inv <<- NULL
               }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list(set = set, get = get, 
                   setinverse = setinverse,
                   getinverse = getinverse)
   }


##  if the inverse of a matrix is already computed, retrive the
##  stored solution, otherwise apply the function solve on the
##  matrix. Cache the inverse of the matrix and return the solution.

cacheSolve <- function(x) {
  inv <- x$getinverse()
  if(!is.null(inv)){
        message("getting cached data")
        return(inv)
  }
  data <- x$get()
  inv <- solve(data) 
  x$setinverse(inv)
  inv
  ## Return a matrix that is the inverse of 'x'
}

#tests
#test_m <- matrix(9:12, nrow = 2, ncol = 2)
#cached_matrix <- makeCacheMatrix(test_m)
#cacheSolve(cached_matrix)

#Rerun
cacheSolve(cached_matrix)

