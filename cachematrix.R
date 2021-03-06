## makeCacheMatrix() will return a closure containing a matrix ,its inverse
## and functions to 1) set the value of the matrix, 2) get the value of the matrix,
## 3) set the inverse of the matrix and 4) get the inverse of the matrix
## 
## cacheSolve() will check if the inv(x) exists and then decide whether to calculate
## the inv(x) and cache it.

## 1) set the value of the matrix, 2) get the value of the matrix, 3) set the inverse of the matrix and 4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    
    set_mat <- function(y) {
        inv_x <<- y
        inv_x <<- NULL
    }
    get_mat <- function() x
    
    set_inv_matrix <- function(inv_matrix) inv_x <<- inv_matrix
    get_inv_matrix <- function() inv_x
    
    # return the closure
    list(set_mat = set_mat, 
         get_mat = get_mat, 
         set_inv_matrix = set_inv_matrix,
         get_inv_matrix = get_inv_matrix)
}

## test the above makeCacheMatrix()
test_mat <- matrix(data = 1:4, nrow = 2, byrow = TRUE)
cached_mat <- makeCacheMatrix(test_mat)
# cached_mat$get_mat()
# cached_mat$get_inv_matrix() # seems all right

## cacheSolve() only check or set the inverse of the cached matrix

cacheSolve <- function(x, ...) {
        inv_x <- x$get_inv_matrix()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        temp_mat <- x$get_mat()
        inv_mat <- solve(temp_mat, ...)
        
        x$set_inv_matrix(inv_mat)
        
        inv_mat
}

# test the cacheSolve()
cacheSolve(cached_mat)
cached_mat$get_inv_matrix()
