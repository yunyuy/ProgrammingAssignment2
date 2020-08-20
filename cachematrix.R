# This pair of functions calculate and cache the inverse of a matrix


# makeCacheMatrix creates a spcial matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        I <- NULL                  # initiate I as NULL      
        
        set <- function(y){     
                x <<- y            # assign the input to the x object in the parent environment
                I <<- NULL         # assign NULL to I in the parent environment
        }
        
        get <- function() x        # define getter for matrix x, retrives x from parent enviroment
        
        setInverse <- function(inverse) I <<- inverse       # set the value of inverse of a matrix
        
        getInverse <- function()I                           # get the value of the inverse of a matrix
        
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)

}



# cacheSolve calculates the inverse of the matrix created by the above function
# If the inverse has already been calculated (and the matrix has not changed), 
# then retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        
        # Check if I is pre-exists, return cached data
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        
        ## Return a matrix that is the inverse of 'x'
        data <- x$get()
        I <- solve(data, ...)   # calculate inverse matrix
        x$setInverse(I)
        I
        
}


##  test functions
m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)

temp <- makeCacheMatrix(m1)
cacheSolve(temp)
cacheSolve(temp)
