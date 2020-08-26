##creating the makeCacheMatrix

makeCacheMatrix <- function(x = matrix()) {
                    i<- NULL
                    set<-function(y) {
                          x<<- y
                          m<<- NULL
                    }
                    get<- function() x
                    setinverse<- function(inverse) i<<-inverse
                    getinverse<- function() i
                    list(set = set,
                         get = get,
                         setinverse = setinverse,
                         getinverse = getinverse)
}


##this function uses the saved cache for the inverse matrix 

cacheSolve <- function(x, ...) {
        i<-x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data<-x$get()
        i<-solve(data, ...)
        x$setinverse(i)
        i
}

