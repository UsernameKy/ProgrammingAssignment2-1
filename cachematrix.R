## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##a pair of functions that cache the invese of a matri
##creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ##initialize the inverse property
        i<-NULL
        
        ##method to set the matrix
        set<-function(matrix){
                x<<-matrix
                i<<-NULL
        }
        ##METHOD the get the matrix
        get<-function(){
                ##return the matrix
                x
        }
        ##method to set the inverse of the matrix
        setInverse<-function(inverse){
                i<<-inverse
        }
        ##method to get the inverse of the matrix
        getInverse<-function(){
                ##return the inverse property
                i
        }
        ##return a list of the methods
        list(set=set, 
             get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}



##compute the inverse of the special matrix returned by ""makeCacheMatrix" above.
##if the inverse has already been calculated (and the matrix has not changed), 
##then the "cachesolve" should retrieve the inverse from the cache.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x<-x$getInverse()
        
        ##return the inverse if its already set
        if(!is.null(x)){
                message("getting cached data")
                return(x)
        }
        
        ##get the matrix from the object
        data<-x$get()
        
        ##calculate the inverse using matrix multiplication
        x<-solve(data) %*%data
        
        ##set the inverse to the object
        x$setInverse(x)
        
        ##return the matrix
        x
}






