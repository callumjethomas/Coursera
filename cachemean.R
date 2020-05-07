cachemean <- function(x, ...) {
        m <- x$getmean()
        # try and get the mean from getmean() function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
                # since makeVector() sets the cached mean to NULL whenever a new
                # vector is set into the object, if the value is NOT NULL then we
                # have a cached mean we can return
        }
        data <- x$get()
        # if there is no cached mean, get the vector from input
        m <- mean(data, ...)
        # calculate a new mean from input
        x$setmean(m)
        # set the new mean as m
        m
        # return m
}