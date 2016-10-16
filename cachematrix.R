## Two functions to work on a generic matrix
## makeCacheMatrix and cacheSolve

## Two functions to make matrix functions and the solve the matrix inverse
## use this as:
## my_matrix <- matrix(numbers, size, size)
## newname <- makeCacheMatrix(my_matrix)
## cachesolve(newname)


##create a function which accepts our matrix as an argument
makeCacheMatrix <- function(x = matrix()) { 
        ## define the inverse matrix as matrixinverse and to be NULL
        matrixinverse <- NULL                     
        ## make another function "set" where the value will be cached an
        ## available outside the environment (<< call)
        set <- function(y) {                      
                x <<- y
                ## if the matrix was changed make sure it is still NULL
                matrixinverse <<- NULL              
        }
        ## define get function
        get <- function() x                           
        #define setinverse function and calculate inv matrix via "solve"
        setinverse <- function(solve) matrixinverse <<- solve 
        # defines the getinverse function     
        getinverse <- function() matrixinverse        
        ## passes the value of the function makeCacheMatrix        
        list(set = set,
             get = get,                    
             setinverse = setinverse,
             getinverse = getinverse)
}

# used to get the cache of the matrix
cacheSolve<- function(x, ...) {                 
        matrixinverse <- x$getinverse()
        #check and retrieve existing matrix inverse.
        if(!is.null(matrixinverse)) {                 
                message("getting the cached inverse matrix")
                return(matrixinverse)
        }
        #if the inverse is absent, calculated and display
        data <- x$get()                      #get matrix         
        matrixinverse <- solve(data, ...)    #solve to calculate inverse
        x$setinverse(matrixinverse)          #as it wasn't there we put it there
        matrixinverse
}