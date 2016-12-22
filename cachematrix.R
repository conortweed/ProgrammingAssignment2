## makeCacheMatrix will create a matrix object, apply the solve function to the matrix, and then cache the 
## inverse of the matrix for use later
## cacheSolve will then check to see if there is a cached inverse of the matrix already
## available and return the value, or if there is no cached inverse then it will go on
## to calculate this itself and return the result

## makeCacheMatrix will:
## 1. Set the values of the matrix using the object x
## 2. Get the value of the matrix
## 3. Set the result of the inverse
## 4. Get the result of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Start by creating a null vector for the inverse result
        inv <- NULL
        
        set <- function(y) {     ## Set the values of the matrix
                x <<- y
                inv <- NULL
                }
        
        get <- function() x   ## Get the values of the matrix
       
        setInv <- function(solve) inv <<- solve  ## Set the result of the inverse
        
        getInv <- function() inv   ## Get the result of the inverse
        
        list(set = set, get = get, setInv = setInv, getInv = getInv)   ## "Vector"/list of objects from the makeCacheMatrix function
}


## cacheSolve will first check to see if the result for the inverse of the matrix created
## as part of the makeCacheMatrix function has already been calculated. If it has, then 
## cacheSolve will return the result. If it has not already been calcualted, then cacheSolve
## will calculate the inverse and set the result in the cache using the setInv function

cacheSolve <- function(x, ...) {
        
        inv <- x$getInv()
        
        if(!is.null(inv)) {   ## Checking to see if the result for getInv contains values
                message("getting cached data")
                return(inv)   ## Will provide the result for the inverse from makeCacheMatrix, does not calculate the result anew
                }
        
        ## If getInv showed NULL then the function will not return a cached result, but will 
        ## go on the calcuate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        
        x$setInv(inv)  ## Having calculated, the function will cache the inverse for use later if needed
        
        inv   ## And display the result
}
