## this function creates a cache of inverse of a matrix
## for finding the inverse i am using the solve() function
## for using this functin first create a matrix m
## then create another special matrix m1<- makeCacheMatrix(m)
makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<- function(y){
          x<<- y
          m<<- NULL
        }
        get<- function() x
        setsolve<- function(solve) m<<- solve
        getsolve<- function() m
        list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}
##this function solves the inverse of the created matrix m
## if the matrix inverse is already solved using this function then cache 
## matrix is created and after calling the inverse everytime the function will
## return the cached matrix
## to solve inverse use cacheSolve(m1)
## here m1 is the special matrix created using the makeCacheMatrix() function
cacheSolve <- function(x, ...) {
        m<- x$getsolve()
        if(!is.null(m)){
          message("getting cached inverse")
          return(m)
        }
        matrix<- x$get()
        m<- solve(matrix, ...)
        x$setsolve(m)
        m
        
        ## Return a matrix that is the inverse of 'x'
}
