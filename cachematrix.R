## Brian Vives
## R Programming
## Assignment 2

## This function via 2 parts will calculate the inverse of e matrix, or pull from cache if rerun and no change
## made to the matrix given


##  makeCacheMatrix will create a "special" matrix and can also store cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        im <- NULL  ## im short for inverse matrix,  set NULL for clean first run
        
        ## will set based on argument given at command line calling of var$set(arg)
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x  ##seems to call the environment's memory addressing
        
        
        setim <- function(solve) im <<-solve
        getim <- function() im
        list(set = set, get = get,
             setim = setim,
             getim = getim)
}

## cacheSolve will calculate inverser of matrix using solve().  If it has been run before and the matrix
## the same,  it will call up the cached value for the inverted matrix
cacheSolve <- function(x, ...) {
        
        im <- x$getim()
        
        ## if not null,  an inverse has been done before and will be called up,  print message will indicate this
        if (!is.null(im)) {
                message("Has been cached,  retrieving that now")
                return(im)
                ## if im is NULL,  solve is run directly as never run before        
        } else {
                im <- solve(x$get())  ##solve based on get value from input
                x$setim(im)  ##sets im to not NULL after first run
                return(im)
        }
}