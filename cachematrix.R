## Here you'll find two excellent functions for your perusal and use at will.
## These fine functions relate to matrices, and in particular to the sometimes
## tricky business of inverting such beasts. They will surely aid you in your travels!


## This wonderful function creates a very special, nigh-on magical
## matrix-like object able to cache its very inverse!

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) inv <<- inverse
     getinverse <- function() inv
     list (set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}


## And this grand function then, oh boy! It is able to compute the inverse of 
## the magical matrix-like object returned by the above 
## function of excellence. But lo and behold! If such an inverse has already 
## been established, this clever chap of a function instead retrieves 
## this pre-determined inverse from the depths of the cache itself!

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
             message("fetching cached inverse")
             return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

