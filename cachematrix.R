#Assuming all matrix can be inversed:
  #The function "makeCacheMatrix" creates a special matrix object which
  #can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL 
set <- function(y){ 
        x <<- y   
        inv <<- NULL 
}
get <- function() x 

setInv <- function(inverse) inv <<- inverse

list(set = set, get = get, setInv = setInv, getInv = getInv)
}


#"cacheSolve" is a function which gives us the inverse of the special matrix
#obtained with "makeCacheMatrix". Although, if the inverse has already been
#calculated, it returns the original.

cacheSolve <- function(x, ...) {
        inv <- x$getInv()  
        
        if (!is.null(inv)) {
                message("Obtaining cached result")
                return(inv)
        }
        
        mat <- x$get()  
        inv <- solve(mat, ...)  
        x$setInv(inv)  
        inv 
}
