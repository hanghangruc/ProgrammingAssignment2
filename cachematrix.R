## Comments: These two function together are able to do
## potentially time-consuming computations.

## This function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) i <<- inverse #store the value of the input as "i"
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse) 
}
## "makeCacheMatrix" function returns the results of four sub-functions to us


## This function computes the inverse of the special 
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse() 
        # the "if" function check if the inverse has been calucalted before
        if(!is.null(i)){
                message("getting cached data") #if the same inverse has been calculated
                #the "if" function gets the inverse from the cache and skips the computation
                return(i)
        }
        data <- x$get()
        i <- solve(data,...) #if the inverse has not been calculated,
        #it is calculated here
        x$setinverse(i) #store the value of inverse in the cache 
        i
}
