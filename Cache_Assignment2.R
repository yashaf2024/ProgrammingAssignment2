# Create a Matrix Object (x) and caches its Inverse (m)
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse_matrix) m <<- inverse_matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Calculates the inverse of the matrix returned by makeCacheMatrix
# If the inverse is already calculated and stored, retrieves it from cache
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

# Testing
# Define a test matrix
test_matrix <- matrix(c(2, 1, 1, 3), nrow = 2, ncol = 2)

# Create the special "matrix" object
cacheMatrix <- makeCacheMatrix(test_matrix)

# Compute the inverse and cache it
inverse1 <- cacheSolve(cacheMatrix)
print(inverse1)

# Test caching by calling cacheSolve again
inverse2 <- cacheSolve(cacheMatrix)
print(inverse2)

# Validate the inverse by multiplying the matrix by its inverse
identity_matrix <- test_matrix %*% inverse1
print(identity_matrix)  # Should be close to the identity matrix
