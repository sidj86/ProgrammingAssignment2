## R programming: Assignment 2
## Caching the inverse of a matrix
## This code enables the use to create a 'special' matrix which has the ability to cache its own invese.
## This is implemented through two functions which are described below

## This function returns a special matrix (essentially a list of 4 functions):
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
## Inputs: an invertible matrix

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y)
        {
                x <<-y
                inv <<- NULL
        }
        get <- function() x
        
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## This function calculates the inverse of a special matrix created by makeCachematrix. 
## If the inverse is already available in the cache, it returns that, thus saving computation time.
## If not, it uses solve() to calculate the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv<-solve(data,...)
        x$setinv(inv)
        inv
}

## This is just a simple test function to see if the code works
testInverse <-function()
{
        mat1 <- matrix(data = c(4,3,3,2), nrow=2, ncol=2, byrow = 2)
        x<- makeCacheMatrix(mat1)
        for (c in 1:10)
        {
                y<-cacheSolve(x)   
                print(y)
        }
        
}