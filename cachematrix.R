
##combination of functions calculates the inverse of a 2x2 matrix if the initial matrix has changed,
##if matrix did not change, cached inverse matrix will be returned
##function makeCacheMatrix sets 'm' to Null everytime set function is called (that is if the matrix changes)

makeCacheMatrix <- function(x = invmatrix()) {
        m <- NULL
        set <- function(y){
                x<<-y
                m<<-NULL
        }
        get <- function() x #returns initial matrix
        setmatrix<-function(invmatrix) m<<-invmatrix
        getmatrix<-function() m
        list(set=set, ##sets functions in list to be called in cacheSolve
             get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)

}


## function below executes 'solve' (the actual function needed to be performed);
## function only executes solve() if m is NULL; m is the output variable

cacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setmatrix(m)
        m
}
