## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#################################################
##
##  The function, makeVector creates a special "vector", which is really a 
##  list containing a function to:
##
##  * set the value of the vector
##  * get the value of the vector
##  * set the value of the mean
##  * get the value of the mean
##
#################################################
makeVector <- function(x = numeric()) {
        ## initialize m variable to empty set
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## Write a short comment describing this function               
#################################################
##
## The following function calculates the mean of the special "vector" created with the above function. 
## However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the 
## cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the 
## mean in the cache via the setmean function.  
## 
#################################################
cachemean <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getmean()
        
        ## if the inverse has already been calculated
        if(!is.null(m)) {
                
                ## get it from the cache and skips the computation.
                message("getting cached data")
                return(m)
        }
        ## otherwise, calculates the inverse
        data <- x$get()
        m <- mean(data, ...)
        
        ## sets the value of the inverse in the cache via the setinv function.
        x$setmean(m)
        m
}
