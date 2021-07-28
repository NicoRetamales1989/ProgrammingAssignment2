install.packages(matlib)#install packages with mathematical functions that calculate inverse of matrix
library(matlib)

makeCacheMatrix <- function(x = matrix()) {
        Ivn <- NULL
        set1 <- function(y) {
                x <<- y
                m <<- NULL
        }
        get1 <- function() x
        setivn <- function(invr) Ivn <<- invr
        getivn <- function() Ivn
        list(set1 = set1, get1 = get1,
             setivn = setivn,
             getivn = getivn)
        
}

cacheSolve <- function(x, ...) {
        Ivn <- x$getivn()
        if(!is.null(Ivn)) {
                message("getting cached data")
                return(Ivn)
        }
        mtx <- x$get1()
        Ivn <- inv(mtx, ...)
        x$setivn(Ivn)
        Ivn
}