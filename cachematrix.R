## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        #create the set function
        set <- function(y)
        {
                x <<- y
                inv <<- NULL     
        }
        #create the get function 
        get<- function() x
        
        #create the set Inverse function
        setInverse <- function(invMatrix) inv <<- invMatrix
        
        #create the get Inverse function
        getInverse <- function() inv
        
        #return object as encapsulation of methods
        list(set=set,get=get,setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        
        inv <- x$getInverse()
        #check if inverse is null or not
        if(!is.null(inv))
        {
                #if inverse not null return the cached copy of the inverse 
                message("Getting Cached Data")
                return(inv)
        }
        
        #else calculate inverse
        data <- x$get()
        inv <- solve(data)
        
        #set the inverse to the object member that will act as cache
        x$setInverse(data)
        data
}
