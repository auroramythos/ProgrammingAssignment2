##Just to cache matrix and calcualte and store it inverse value

makeCacheMatrix <- function(x = matrix()) {   ##caches matrix and defines functions
  inv <- NULL   ##defines inv as null at first
  set <- function(y) {   ## defines set function
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ##defines the get function and same for getinv andsetin
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
} 

cacheSolve <- function(x){  ##chaches or return the inverse amtrix
  inv <- x$getinv()  ##get inverse matrix from previos function
  if(!is.null(inv)) {   ##checks if there is any value
    message("getting cached inverse")   ##if there is a value return this text
    return(inv)   #and return the inverse matrix itslef
  }
  data <- x$get()  ##if there was no value, then if assignes the inverse matrix value to a new var
  inv <- solve(data) ## and calculate and assign an inverse matrix to inv var
  x$setinv(inv)  ## reassign the new value to the x <- makecachematrix
  inv
}

m <- matrix(c(-8, -2, 1, 1), 2,2)   ##example that was used to check it
x <- makeCacheMatrix(m)
x$get()
inv <- cacheSolve(x)
inv
inv <- cacheSolve(x)
inv


