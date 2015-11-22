## These set of functions allow user to cache the inverse of a matrix.
## In this case, future use of the inverse of the same matrix (base on the name
## of the matrix) can be recalled from cache rather than recalculate which 
## will potentially take more time.
## If the data is changed under the same name, 'makeCacheMatrix' need to be 
## re-run for that name

## This function define a temperary matrix containing Na's called 
## inverse.matrix to store the inverse of the matrix entered. Plus it store
## all the functions needed to performed when 'cacheSolve' function is called.
## 'set.matrix' will replace existing matrix will a new one and clear
## the inverse.matrix variable
## 'get.matrix' gets the matrix the user want to calculate the inverse.
## 'set.inverse' assigned the calculated inverse matrix to the object called 
## 'inverse.matrix'
## 'get.inverse' gets the 'inverse.matrix' that is previously defined. If
## this is the first time to run the function the that matrix, 'inverse.matrix'
## will be defined to be null in this function.

makeCacheMatrix <- function(x = matrix()) {
	inverse.matrix <- matrix(, nrow(x), ncol(x))
	set.matrix <- function (y) {
		x <<- y
		inverse.matrix <<- matrix(, nrow(x), ncol(x))
	}
	get.matrix <- function() x
	set.inverse <- function(solve) inverse.matrix <<- solve
	get.inverse <- function() inverse.matrix
	list(
		##set = set,
		get.matrix = get.matrix,
		set.inverse = set.inverse,
		get.inverse = get.inverse
	)
}

## This function will perform the functions defined in 'makeCacheMatrix' function
## in order.
## First it will check if the entered matrix has been created and try to find the inverse
## of the matrix 
## Then if check if the inverse matrix exist, it will display "getting cached data" and 
## return the inverse matrix then this is the end of the function
## If the inverse matrix does not exist, it will store the new entered matrix into
## 'New.Matrix' variable, then using 'solve' function to get inverse and store it. Lastly,
## return the inverse matrix

cacheSolve <- function(x, ...) {
	inverse.matrix <- x$get.inverse()
	if(prod(!is.na(inverse.matrix))) {
		message("getting cached data")
		return(inverse.matrix)
	}
	New.Matrix <- x$get.matrix()
	inverse.matrix <- solve(New.Matrix, ...)
	x$set.inverse(inverse.matrix)
	inverse.matrix
}
