
# makeCacheMatrix
# Creates an object that can cache the inverse of the matrix passed in  
# 
# Args:
#   x: A square invertible matrix
#      Example: x <- matrix(c(1,1,1,3,4,3,3,3,4),3,3)
#      Invert:  invertx <- matrix(c(7,-1,-1,-3,1,0,-3,0,1),3,3)
# 
# Returns:
#   An list object containing function names  
# 
# Error handling:
#   message if object x passed is not a matrix

makeCacheMatrix <- function(x = matrix()) {
    
    # check the object type of x
    if (!is.matrix(x)) {
        message("makeCacheMatrix function recieved wrong object class.")
        return
    }
    
    inverted.matrix <- NULL
    
    # returns non-inverted matrix
    get <- function() x
    
    # sets non-inverted matrix  
    set <- function(y) {
        x <<- y
        inverted.matrix <<- NULL
    }
    
    # returns the inverted cached matrix
    getInvertedMatrix <- function() inverted.matrix
    
    # sets the inverted cached matrix
    setInvertedMatrix <- function(m) inverted.matrix <<- m
    
    
    list (get = get, 
          set = set,
          getInvertedMatrix = getInvertedMatrix,
          setInvertedMatrix = setInvertedMatrix)
}


# cacheSolve
# This function computes the inverse of the special "matrix" returned 
# by makeCacheMatrix. If the inverse has already been calculated
# then the cachesolve retrieves the inverse from the cache of x
# 
# Args:
#   x: A list item created as a result of a call to makeCacheMatrix()
#      
# Returns:
#   A pre-calculated inverted matrix from cache of object x
#   or a newly created matrix that is the inverse of 'x

cacheSolve <- function(x, ...) {
    
    # First check to see if x$inverted.matrix exists
    inverted.matrix <- x$getInvertedMatrix()
    if(!is.null(inverted.matrix)) {
        message("Returning cached inverted matrix")
        return(inverted.matrix)
    }
    
    # solve and cache
    m <- solve(x$get(), ...)
    x$setInvertedMatrix(m) 
    
    #return the inverted matrix
    return(m)
}