## Date: June 19, 2015
## Programmer: Philly Pat
## Title: cachematrix.R
## Purpose: Fulfilling Assignment 2 for R-Programming
##
## Functions:
##  makeCacheMatrix
##  cacheSolve

## These functions will take an invertible matrix and store it in 
## memory so that both it and its inverse may be retrieved rapidly.

## This function takes in an invertible matrix.  
## It can return it via get()
## It can set an inverse via setinv(<some matrix>)
## It can get the inverse via getinv()

makeCacheMatrix <- function(x = matrix()) 
{

 xinv <- NULL
 set <- function(y) 
 {
    x <<- y
    xinv <<- NULL
 }
 get <- function() x
 setinv <- function(inv) xinv <<- inv
 getinv <- function() xinv
 list(set = set, get = get, setinv = setinv,getinv = getinv)

}

## This function will return the inverse of a matrix for
## a cached matrix if that inverse has already been calculated.
## Else, it will caclulate that inverse and return it after
## caching.

cacheSolve <- function(x, ...) 
{

   ## Return a matrix that is the inverse of 'x'
   inv <- x$getinv()
   if(!is.null(inv)) 
   {
     message("getting cached inverse")
     return(inv)
   }
   data <- x$get()
   xinv <- solve(data)
   x$setinv(xinv)
   xinv
   
}
