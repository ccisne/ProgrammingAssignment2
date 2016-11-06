## Get and Set a square matrix
## I initialize i matrix with dimensions of x and elements NA
## I use <<- operator to put objects on parent enviroment and access to it
## The functions get and set for matrix to inverse
## The setsolve function to obtain the inverse and getsolve to determine if i matrix has changed

makeCacheMatrix <- function(x = matrix()) {

    dimx <- dim(x)
    i <- matrix(nrow = dimx[1], ncol = dimx[2])
    set <- function(y = matrix()) {
        x <<- y
        i <<- matrix(nrow = dimx[1], ncol = dimx[2])
    }
    get <- function() x
    setsolve <- function(solve) i <<- solve
    getsolve <- function() i
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
    
}


## Return the inverse of a square matrix from the cache or calculate it
## If Inverse of a square matrix has changed then print message and return it
## else calculate inverse of the matrix, use setsolve function to save it and return the inverse

cacheSolve <- function(x, ...) {
        
    i <- x$getsolve()
    if(!is.na(i[1,1])) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
    
}
