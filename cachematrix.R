## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mtrx<-NULL
        set<-function(y) {
                x<<-y
                mtrx<<-NULL
        }
        
        get<-function() x
        set.inverse<-function(invs) mtrx<<-mean
        get.inverse<-function() mtrx
        list(set=set, get=get, set.inverse=set.inverse, get.inverse=get.inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mtrx<-x$get.inverse()
        if(!is.null(mtrx)) {
                return(mtrx)
        }
        
        data<-x$get()
        mtrx<-solve(data, ...)
        x$set.inverse(mtrx)
        mtrx
}
