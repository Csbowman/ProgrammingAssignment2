makeCacheMatrix <- function(x = matrix()) { # open prime 1

      xinv <- NULL # for storing the inversion
      set <- function(y) { # nested 1
          x <<- y
          xinv <<- NULL #initialize xinv to null
      }   #close nested 1

      get <- function() x # return the input matrix
      setInv <- function(inv) xinv <<- inv # set the inversed matrix
      getInv <- function() xinv # gets the inversed matrix
      list(set = set, get = get, setInv = setInv, getInv = getInv)
  } # close prime 1

 cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
      # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
      if(!is.null(m)) { # A check to be sure the matrix worked
          message("Returning Inverted Matrix")
          return(m) # return the calculated inversion
      }
      data <- x$get() # otherwise we do x$get to get the matrix object
      m <- solve(data) # solves
      x$setInv(m) # storing
      m # printing
  }
