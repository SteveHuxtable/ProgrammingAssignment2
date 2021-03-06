makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...) 
        x$setmean(m)
        m
}

# test the cachemean
# this is actually a closure: function + values
test_vec <- makeVector(c(1, 2, 3))
test_vec$getmean() # return NULL

cachemean(test_vec)
test_vec$getmean()   # return cached mean
