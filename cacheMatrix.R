Programming assignment 2:

## John Helm ProgrammingAssignment2
## makeCacheMatrix = the name of the first function 
## makeCacheMatrix: create matrix object to cache it's inverse
## cacheSolve compute inverse of matrix returned by makeCacheMatrix
## ----------

# Here starts makeCacheMatrix
makeCacheMatrix <- function(x = matrix()) { #start makeCacheMatrix
         inverse   <- NULL # inverse set to NULL
         set <- function(y) # function assigns X the arguments from the function 
         { 
                 x <<- y # assign data to matrix 
                 inverse <<- NULL # inverse set to NULL in parent / global
         } 
          
         get <- function() x # function returns x 
         setinverse <- function(inv) inverse <<- inv # function assigns invers 
         getinverse <- function() inverse # function returns i 
          
         list(set = set, get = get, setinv = setinverse, getinv = getinverse) # list function returns list of functions 
  
 } 

#ends makeCacheMatrix

# cacheSolve = inverse of the matrix returned by makeCacheMatrix
#  Begins cacheSolve
cacheSolve <- function(x, ...) { #start cacheSolve
         inv <- x$getinv() # attempt cache pull
         if (!is.null(inv)) # begin if to check cache 
         { #start if (!is.null(inv))
                 message("Cached Data Pull") # send Cache Message
                 return(inv) # return cached inverse 
         } #end if (!is.null(inv))
          
         data <- x$get() # get cached data 
         inv <- solve(data, ...) # compute inverse 
         x$setinv(inv) # cache inverse  
         inv # return inverse 

 }  #ends cacheSolve
