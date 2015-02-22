# File with functions for inverse matrix and caching the result
####################################################################
# Computing the inverse of a square matrix can be done with the solve function in R. For example, 
# if X is a square invertible matrix, then solve(X) returns its inverse.
# For this assignment, assume that the matrix supplied is always invertible.
####################################################################


# makeCacheMatrix function creates a special "matrix" object that keeps matrix data and can cache its inverse
# function is used for storage data, not processing it
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # set new matrix and NULL for inverted matrix
    set <- function(y) {
        message("Setting new Matrix")
        x <<- y
        inv <<- NULL # preventing keeping old inverted for new matrix!
    }
    get <- function() x # take our matrix data
    setSolve <- function(inv) inv <<- inv # add inverted matrix to cache
    getSolve <- function() inv  # take inverted matrix from cache (or NULL)
    
    #our function returns list of 'methods' with their values
    list(set = set, get = get,
         setSolve = setSolve,
         getSolve = getSolve)       
}


# cacheSolve: This function computes, but don't store the inverse of the special "matrix" object
# Special "matrix" object must be created with makeCacheMatrix befor 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
# Function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    inv <- x$getSolve() # check if such matrix is in cache
    
    # such matrix is in cache - load it from there
    if(!is.null(inv)) { 
        message("Getting cached Inversed Matrix")
        return(inv)
    } 
    
    # NO such matrix in cache - create it and add to cache
    else{
        message("Making Inversed Matrix & add it to cache")
        data <- x$get()
        inv <- solve(data, ...) # create inversed matrix - massive counting here
        x$setSolve(inv) # write created inversed matrix to cache
    }
    inv # return inverted matrix
}


####### Working examples #######
# Create testing matrix
size <- 10 # size of the matrix edge, don't make this too big
x <- matrix(rnorm(size^2), nrow=size, ncol=size)
x_inv <- solve(x)           # this is result of solve function for compare with our results
# Here the magic begins:
xx <- makeCacheMatrix(x)    # create special object
xx_inv <- cacheSolve(xx)    # FIRST CALL - create inverted matrix
xx_inv2 <- cacheSolve(xx)    # SECOND CALL- call inverted matrix from cache
################################