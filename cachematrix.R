## Put comments here that give an overall description of what your
## functions do

## Return the list of function 
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	invmatrix <- NULL
    set <- function(y) {
        x <<- y
        invmatrix <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) invmatrix <<- inverse
    getinverse <- function() invmatrix
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		invmatrix <- x$getinverse()
		if(!is.null(invmatrix)) {
			message("getting cached data.")
			return(invmatrix)
		}
		data <- x$get()
		##  inverse the matrix
		invmatrix <- solve(data)
		x$setinverse(invmatrix)
		invmatrix
}


# > A = matrix( 
# +     c(2, 4, 3, 1, 5, 7,4,5,6), # the data elements 
# +     nrow=3,              # number of rows 
# +     ncol=3,              # number of columns 
# +     byrow = TRUE)
# > A
     # [,1] [,2] [,3]
# [1,]    2    4    3
# [2,]    1    5    7
# [3,]    4    5    6
# > my_matrix <- makeCacheMatrix(A)
# > cacheSolve(my_matrix)
           # [,1]       [,2]       [,3]
# [1,] -0.1515152 -0.2727273  0.3939394
# [2,]  0.6666667  0.0000000 -0.3333333
# [3,] -0.4545455  0.1818182  0.1818182
# > cacheSolve(my_matrix)
# getting cached data.
           # [,1]       [,2]       [,3]
# [1,] -0.1515152 -0.2727273  0.3939394
# [2,]  0.6666667  0.0000000 -0.3333333
# [3,] -0.4545455  0.1818182  0.1818182