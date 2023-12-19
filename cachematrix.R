
# Creates a matrix object that can cache its inverse
makeCacheMatrix <- function(X = matrix()) {
    X_ <- NULL
    set <- function(Y) {
            X <<- Y
            X_ <<- NULL
    }
    get <- function() X
    
    set_inverse <- function(N) X_ <<- N
    get_inverse <- function() X_
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


# Get a matrix inverse through cache
cacheSolve <- function(X, ...) {
    X_ <- X$get_inverse()
    if(!is.null(X_)) {
        message("getting cached data")
        return(X_)
    }
    data <- X$get()
    N <- solve(data, ...)
    X$set_inverse(N)
    X$get_inverse()
}
