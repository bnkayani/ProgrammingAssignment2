## This is R Programming assignment Number 2 
## which will cache the inverse of a matrix to make instead of computing it repeatedly to make it cost effective.

## This is the function "makeCacheMatrix" which will cache the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	inv <- NULL
	set <-function(y) {
	x   <<-y
	inv <<-NULL
        }

get <-function() x
setinv <-function(inverse) 
inv <<-inverse
getinv <-function() inv
list(set=set, get=get, setinv=setinv, getinv=getinv)
}

## Function cacheSolve will compute the inverse of the matrix. If inverse is already computed then it will be retrived 
## using this function from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv <-inv$getinv()
	if(!is.nul(inv)){
	message("getting cached data")
	return(inv)
        }
	mat.data=x$get()
	inv=solve(mat.data,...)
	x$setinv(inv)
	return(inv)
)
