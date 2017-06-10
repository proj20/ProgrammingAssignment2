## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.


makeCacheMatrix <- function(x = matrix()) {
        # x = an invertible square matrix
        # Returns a list that contains functions that can:
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse
        # 4. get the inverse
        # and is used as input to cacheSolve()
        
        inv = NULL
        set = function(y) {
                # <<- is used to assign a value to an object in an environment that is different from the current environment 
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
        # x = output from makeCacheMatrix()
        # Computes the inverse of the special "matrix" returned by makeCacheMatrix
        
        inv = x$getinv()
        
        if (!is.null(inv)){
                # retrieve from the cache, skip calculation 
                message("getting cached data")
                return(inv)
        }
        
        mat.data = x$get()
        inv = solve(mat.data, ...)
        x$setinv(inv)
        return(inv)
}


## Below is a test/simulation to demonstrate difference between
## time it takes for the invert calculation to take place vs
## time it takes to retrieve data from cache.


# Create a 1000 by 1000 matrix:
rvec <- rnorm(1000000)
mat1 <- matrix(rvec,1000,1000)
mat2 <- makeCacheMatrix(mat1)


# First time calculate the inverse of mat2:
timeA <- Sys.time()
solve1 <- cacheSolve(mat2)
timeB <- Sys.time()
# Instead of calculating, data will be retrieved from cache:
solve2 <- cacheSolve(mat2)
timeC <- Sys.time()

# Calculate Duration:
timeB - timeA

# Cache Duration:
timeC - timeB
