## These two functions illustrate how lexical scoping is used in R
## The first function consists of four functions which are assigned to a
#new variable(e.g.my_vector). They make a cache in which a matrix can be stored 
##and the calculated inverse matrix can be stored. Furthermore, 
##it names the ouputs(list)from the four functions. 
##The second function calculates the inverse of a matrix
##using the variables which were defined in the MakeCacheMatrix parent 
##environment

## Write a short comment describing this function
##This first function defines the environment of a new variable
##(e.g. my_matrix<-makeCacheMatrix(x).....) when assigned.
##The first steps in the makeCacheMatrix function ensure that m and x are 
##defined.Tjerefore the set function actually does the same here as
##the argument in the makeCachematrix function and the m<-NULL line of code.
##The get function is assigned the value x. so when the data is 
##called in the CacheSolve, x will be used
##setinverse is defined by the solve defined in another environment
##(in this case in the Cacheinverse function, if it has already been calculated,
##otherwise m will be NULL)
## the get inverse provides the inverse (m) which was set in the Cacheinverse 
##function. Finally the outcome is a list with the 4 named functions used in the
#makeCachematrix. Because they're name we can easily call them in CacheInverse
## using the x$..

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL 
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
#This function first determines what the value is of m
## due to lexical scoping it defines m as x$getinverse() from the 
##environment of my_matrix defined earlier. If m has already been calculated 
##for the data used, m is not NULL and it will print you a message telling 
##you that a value is already available.
##after the message it prints the available inverse which is already available 
##in the environment of the variable defined by makeCacheMatrix (my_matrix)
##When this variable is deleted or you want to calculate the inverse of
## a new matrix, my_matrix is defined again using the MakeCacheMatrix
#function.Thus m is null and the new inverse will be calculated 
##using the data (x) as defined in the makeCacheMatrix function
# Then it asigns m to x$setinverse in the parent environment of the 
##MakeMAtrix function by means of the <<-. 
## Finally it returns a matrix that is the inverse of 'x.
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m     
}


my_matrix <- matrix(c(10, 12, 10, 20), nrow = 2, ncol = 2) 
my_inverse<-makeCacheMatrix(my_matrix)
cacheSolve(my_inverse)