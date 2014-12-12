##  pair of functions that cache the inverse of a matrix
## if the contents of the matrix is not changed it caches
## the previously computed inverse matrix
 

## This function creates a special "matrix" object that can cache its inverse
## it returns a list of functions as output.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<-function(y) {
                x<<-y
                inv<<-NULL
                
        }
        get<- function() x
        setsolve <- function(msolve) inv<<-msolve
        getsolv <- function () inv
        list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


##This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. if the inverse is already 
##computed it gets it from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getsolve()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        msolve<-solve(data,...)
        x$setsolve(msolve)
        msolve
}
