## These functions are used together to create a special "matrix" object and cache its inverse
## in the global enviornment. By caching the matrix as an in memory object, the function can call
## upon the cached inverse rather than spending time and memory running the same computation.

## Note: These functions assume the matrix supplied is a square, invertable matrix and therefore use the
## solve function in R. 


## The "makeCacheMatrix function creates a special "matrix" object that is then used as an input for the 
## "cacheSolve" function which computes the inverse of the origianl  matrix that was an input in the 
## "makeCacheMatrix" function. 

makeCacheMatrix <- function(x = matrix()) {                     # Creates an object "makeCacheMatrix" which allows the user to pass an argument "x" as an matrix. The matrix "x" is empty by default. It is assumed "x" is a square, invertable matrix.
        inv <- NULL                                             # Initialize an object "inv" to be used later in the function.
        set <- function(y) {                                    # Initialize the "set" function which will be used to maniplate the data in the "makeCacheMatrix" object.
                x <<- y                                         # Assign the input argument "y" to the "x" object in the parent environment ("makeCacheMatrix")
                inv <<- NULL                                    # Assign the value of NULL to the "inv" object in the parent environment. This line of code clears any value of "inv" that had been cached by a prior execution of cachemean().
        }
        get <- function() x                                     # The "get" function retrives the value of "x" from the parent environment.
        setinverse <- function(inverse) inv <<- inverse             # Defines how the object "inv" will be manipluated and assigns the input argument to the "inv" object in the parent enviornment.
        getinverse <- function() inv                            # Retrieves the value of "inv".
        list(set = set, 
             get = get,                              # Creates a list of the functions created up to this point which will allow the "cacheSolve" function to extract elements of this list by name.
             setinverse = setinverse,
             getinverse = getinverse)
}


## The "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {                                # Creates an object "cacheSolve" which allows the user to pass an argument "x" and any additional arguments as indicated by the ellipsis. 
        inv <- x$getinverse()                                   # The fuction attempts to retrieve a stored value of the inverse matrix by calling the "getinverse" object created in the "makeCacheMatrix" function.
        if(!is.null(inv)) {                                     # Then it checks to see whether the result is NULL. 
                message("getting cached data")
                return(inv)                                     # If the value here is not equal to NULL, the value is passed to the parent environment.
        }
        data <- x$get()                                         # If the value is equal to NULL, the function gets the input value from the beginning of the function.
        inv <- solve(data, ...)                                 # The function computes the inverse of the matrix using the solve function
        x$setinverse(inv)                                       # The "cacheSolve" function uses the "setinverse" function to set the inverse on the input object "x".
        inv                                                     # The function prints the inverse of the input object "x".
}
