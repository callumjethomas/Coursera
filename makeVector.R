makeVector <- function(x = numeric()) {
        # initialise x in the function (as argument, empty numeric vector)
        m <- NULL
        # initialise m in the function
        set <- function(y) {
                x <<- y
                # assigns value (y) to value x in the parent environment
                m <<- NULL
                # initialises m to NULL in the parent environment (clears any value stored in m)
                # therefore, if there is already a mean cached  in m, when x is reset, m will be cleared 
        }
        get <- function() x
        # get the value of the vector from the parent environment
        setmean <- function(mean) m <<- mean
        # assignment the value of the mean to the value m in the parent environment
        getmean <- function() m
        # get the value of the mean m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

# results in a vector of four functions: set, get, setmean, getmean
# contains two object (x and m)

# Due to lexical scoping, myVector contains a complete copy of the 
# environment for makeVector(), including any objects that are defined 
# within makeVector() at design time