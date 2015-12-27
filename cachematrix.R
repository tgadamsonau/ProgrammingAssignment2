## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

##This function contains subfunctions to:
##set the value of the matrix
##get the value of the matrix
##set the value of the inv
##get the value of the inv


makeCacheMatrix <- function(x = matrix()) {
        
        ##set inverse (inv) matrix to null
        inv <- NULL     
        
        
        ##set the value of the matix 
        ##set the inverse 
        set <- function(y) {
              x <<- y
              inv <<- NULL
        }
        
        
        ##get the value of the matrix (M)
        get <- function() x
        
        ##Setter for the inverse
        setInv <- function(solve) inv <<- solve
        
        ##Getter for the inverse
        getInv <- function() inv
        
        ##List the sub functions
        list(set = set, 
             get = get,
             setInv = setInv,
             getInv = getInv)

}


## Write a short comment describing this function

##The function below
##Checks if the inverse of the above function has already been calculated
##If it has gets the inverse from the cache and skips computation
##Otherwise it calculates the inverse of the matrix and sets the value in 
##cache via the setInv function. 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        
        ## If this value is not null return the value
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        ##Otherwise get the value of the matrix
        data <- x$get()
        
        ##Calculate the inverse, save it to cache and return it
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
  
}
