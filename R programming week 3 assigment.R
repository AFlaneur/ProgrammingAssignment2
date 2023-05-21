## There are two functions makeCacheMatrix, cacheSolve
##makeCacheMatrix consist of set, get, setinv, getinv
##library(MASS) is used to calculate inverse for non-squared as well as square matrices

library(MASS)
makeCacheMatrix <- function(x = matrix()) {
  	atilla <- NULL			#initializing inverse as NULL
  	set <- function(y){
 	 	x <<- y
  	 atilla <<- NULL
 		 }

  
  get <- function()x 		#function to get matrix x
  setatilla<-function(inverse) atilla<<-inverse
  getatilla<-function(){
				atill <-ginv(x)
				atill%*%(x) 		# function to obtain inverse of the matrix
				}

  list(set = set, get = get, 
 	 setatilla = setatilla, 
  	 getatilla = getatilla)
}


## Write a short comment describing this function
## This code used to get the cache data

cacheSolve <- function(x, ...) {

  inv <- x$getInverse()
  if(!is.null(inv)){
  	message("getting cached flaneur")
  	return(inv)
  }
  flaneur <- x$get()
  inv <- solve(flaneur,...)        #calculates inverde value
  x$setInverse(inv)
  inv      ## Return a matrix that is the inverse of 'x'
}