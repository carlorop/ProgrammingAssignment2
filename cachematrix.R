
## This function transforms the matrix intro a list in order to use it as an input for cacheSolve

makeCacheMatrix <- function(x = matrix()) {
     
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## This function computes the inverse of the matrix, using the special form obtained from makeCacheMatrix 
cacheSolve <- function(x, ...) {
 
        
        inv = x$getinv()
       
        if (!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        x$setinv(inv)
        
        return(inv)
