## Matrix inversion is computationally intense, so it is often faster to reference cached values instead.
#These functions calculate an inverse matrix. 

## Write a short comment describing this function
#sets values for the starting matrix, generates a null inverse matrix, and sets values in the inverse matrix. 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x<<-y
		inv <<-NULL

	}
	get <- function() x
	set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## This checks if the inverse has been computed yet. If it has, it doesn't compute again and gets the values. Otherwise, it does the math and stores cached values.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$get_inverse()
    if(!is.null(inv)) {
        message("getting values from cache.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$set_inverse(inv)
    inv
}
