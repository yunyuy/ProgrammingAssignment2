## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL                  # initiate I as NULL      
        
        set <- function(y){     
                x <<- y            # assign the input to the x object in the parent environment
                I <<- NULL         # assign NULL to I in the parent environment
        }
        
        get <- function() x        # define getter for matrix x, retrives x from parent enviroment
        
        setInverse <- function(inverse) I <<- inverse
        
        getInverse <- function()I
        
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        
        # Check if I is pre-exists, return cached data
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        I <- solve(data, ...)   # calculate inversed matrix
        x$setInverse(I)
        I
        
}
