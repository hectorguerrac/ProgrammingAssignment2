
# This assignment is abaout a pair of functions that cache the inverse of a matrix

# makeCacheMatrix() builds a set of functions and returns the functions within a list to the parent environment.

# Initialize objects

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL
        
        # Define the "behaviors" or functions for objects of type makeCacheMatrix()
        
        set <- function(y) {
                
                x <<- y
                m <<- NULL
        }
        
        get <- function() x
        
        setinv <- function(inv) m <<- inv
        
        getinv <- function() m
        
        # Create a new object by returning a list()
        
        list(set = set, # gives the name 'set' to the set() function defined above
             get = get, # gives the name 'get' to the get() function defined above
             setinv = setinv,# gives the name 'setmean' to the setmean() function defined above
             getinv = getinv)# gives the name 'getmean' to the getmean() function defined above
}

## cacheSolve() is required to return a matrix that is the inverse of 'x' - object of type makeCacheMatrix().

cacheSolve <- function(x,...) {
        
        # Attempt to retrieve an inverse from the object passed in as the argument (cache)
        
        m <- x$getinv()
        
        # If m is not null then return the value
        
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # If m is null then calculates the inverse with the function solve()
        
        data <- x$get()
        m <- solve(data,...)
        
        # and uses the setinv() funtion to set the inverse into the input object
        
        x$setinv(m)
        m
}
