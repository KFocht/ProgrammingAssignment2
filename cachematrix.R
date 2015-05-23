## These two functions compute and store the inverse of a given matrix

## makeCacheMatrix does the following:
## set changes the matrix stored in the main function
## get returns the matrix stored in the main function
## setinverse stores an inputted matrix for the inverse
## getinverse returns the stored inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {   
        x <<- y
        i <<- NULL
    }
    get <- function() x  
    setinverse <- function(inverse) i <<- inverse  
    getinverse <- function() i 
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve does the following:
## First, checks to see if the inverse is already stored in makeCacheMatrix
## If it is, the function will print "return cached inverse" and the inverse matrix
## If it is not, the function will find the inverse matrix, print the inverse
    ## matrix, and store the inverse matrix into the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()  
    if(!is.null(i)) {  
        message("return cached inverse")
        return(i)
    }
    data <- x$get()  
    i <- solve(data, ...)  
    x$setinverse(i)  
    i  
}
