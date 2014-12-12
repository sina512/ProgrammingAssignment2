##  pair of functions that cache the inverse of a matrix


## This function creates a special "matrix" object that can cache its inverse

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


##his function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above.
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
