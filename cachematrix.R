## Lauren Kazak

## Write a short comment describing this function
## These functions work together to employ the use of lexical scoping to solve the inverse of a matrix. 
## The makeCacheMatrix is essentially the hub of placing functions and values where needed, where the 
## cacheSolve solves the matrix given to the makeCacheMatrix function.  

## The matrixCacheMatrix takes the input argument given by user and stores it under the variable x, 
## additionally, then the variable mat_rix, which will later store the inverse of the matrix, 
## is set to a null vector. The set function will allow the user to set a new matrix in the future,
## will not be used the first time around. This can be done using the $ operator (ex. x$set()). 
## The get function will retrieve the matrix stored under the variable x. The setinverse function 
## initiates the variable inverse, then the inverse variable will be globally assigned to the mat_rix variable. 
## The getinverse function will retrieve the global mat_rix variable. The list allows us to index for the functions using the 
## $ operator.

makeCacheMatrix <- function(x = matrix()) {
        mat_rix <- NULL 
        set <- function(y){
                x <<- y
                mat_rix <<- NULL
        }
        get <- function() x 
        setinverse <- function(inverse) mat_rix <<- inverse 
        getinverse <- function() mat_rix 
        list(set = set, get = get ,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## The CacheSolve function takes the matrixCacheMatrix as an argument. In the beginning of the function,
## it tries to discover if the matrix you are looking for has already been cached. Then the function checks 
## if the mat_rix vector is null by employing the use of an ‘if’ function. If the function true, meaning 
## the empty vector is not false, the message prints and the mat_rix is returned. If this is not the case, 
## the code continues. We index the list in x (makecachematrix) to retrieve the get() function, which will 
## get the matrix the user inputted in the first function. Using the setinverse() function allows us to set 
## the inverse variable to the mat_rix from the cachesolve, then allows us to assign it globally . Finally mat_rix will be returned.

cacheSolve <- function(x, ...) {
        mat_rix <- x$getinverse()
        if(!is.null(mat_rix)){
                message("getting cached data")
                return(mat_rix)
        }
        data <- x$get()
        mat_rix <- solve(data,..)
        x$setinverse(mat_rix)
        mat_rix
}
