## Put comments here that give an overall description of what your
## functions do

##  makeCacheMatrix(): given a matrix as its argument, this function stores the matrix
##    and its inverse (once this is calculated by the cacheSolve() function) and 
##    returns a list of functions that store and retrieve the original matrix and its inverse.
##  cachSolve() takes as its argument the list returned from makeCacheMatrix, and uses the functions
##    of its argument to check to see if the inverse is already available.  If so it calls the
##    getInverse function of the argument to return the inverse.  If the inverse is not avaiable
##    cacheSolve calculates it and calls the setInverse function of its argument to store the
##    inverse.

##  NOTE:  solve() only works with a square matrix, and it fails if the matrix is singular.
##    When I tested it with some rectangular matrices or even some 3 x 3 matrices solve() threw
##    errors.
##  In order to use a non-square matrix must use ginv() instead of solve(). ginv() is a generalized
##    inversion routine that works with non-squar matrices.
##  Uncomment these lines to load the MASS package and have access to the ginv()
##     function in the MASS libary:...
#install.packages ("MASS")
#library(MASS)

##  Write a short comment describing this function
##  This function stores a matrix (supplied as the argument) and it's
##    inverse into the enclosing environment using the superassignment
##    operator <<-, and returns a list of function calls
##    to store and return the original object, and store and return the inverse.
##  The inverse of the matrix is calculated in the cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


##  Write a short comment describing this function
##    cacheSolve takes as its argument an object outputted by the makeCacheMatrix function.
##    cacheSolve first tries to retreive the calculated inverse
##      from the enclosing environment via the getInverse() function of
##      the supplied argument x (a list returned by the makeCacheMatrix()
##      function ... and if it's null then it calculates the inverse,
##      uses the setInverse() function of its argument to store it in the global
##      environment, and finally returns the inverse to the caller.  

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'

      m <- x$getInverse()
      if(!is.null(m)) {
        message("getting cached data")
        return(m)
      }
      data <- x$get()
      # solve() won't work unless its a square matrix and the matrix has a determinant
      # use ginv() instead...
      #m <- solve(data, ...)
      m <- ginv(data, ...)
      x$setInverse(m)
      m
}

#Sample output:
# 
# > m1<-matrix(1:9, nrow=3, ncol=3)
# > m1
# [,1] [,2] [,3]
# [1,]    1    4    7
# [2,]    2    5    8
# [3,]    3    6    9
# > ginv(m1)
# [,1]          [,2]       [,3]
# [1,] -0.6388889 -5.555556e-02  0.5277778
# [2,] -0.1666667 -5.551115e-17  0.1666667
# [3,]  0.3055556  5.555556e-02 -0.1944444
# > testM1<-makeCacheMatrix(m1)
# > #get the inverse, which rignt now should be NULL
#   > testM1$getInverse()
# NULL
# > #use cacheSolve to calcuate and store the inverse
#   > cacheSolve(testM1)
# [,1]          [,2]       [,3]
# [1,] -0.6388889 -5.555556e-02  0.5277778
# [2,] -0.1666667 -5.551115e-17  0.1666667
# [3,]  0.3055556  5.555556e-02 -0.1944444
# > #call it again, to show that its pulling from the cache
#   > cacheSolve(testM1)
# getting cached data
# [,1]          [,2]       [,3]
# [1,] -0.6388889 -5.555556e-02  0.5277778
# [2,] -0.1666667 -5.551115e-17  0.1666667
# [3,]  0.3055556  5.555556e-02 -0.1944444
# > #now compare the cached inverse to a directly cacl'd inverse to show its the same
#   > identical(ginv(m1), testM1$getInverse())
# [1] TRUE


