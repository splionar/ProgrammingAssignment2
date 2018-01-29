## Put comments here that give an overall description of what your
## functions do
'
This file contains two functions, makeCacheMatrix and cacheSolve. Using the concept of lexical scoping, we will be able to find 
inverse of a matrix, and store its value in a cache. When a particular matrix has been stored in the cache, we can return its inverse
directly from its cache, without calculating again. This can provide efficiency in higher level program, especially if it involves
repeated inverses of identical big dimensions matrices.

Credits to lgreski for wonderful explanation on 
https://github.com/lgreski/datasciencectacontent/blob/master/markdown/rprog-breakingDownMakeVector.md
'
## Input matrix is always invertible in this set of problem, hence we can use R built-in function to find matrix:
## Inverse of matrix A <- solve(A) (Only work with nxn matrix) 

## Write a short comment describing this function
'
The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to:
1. set the value of the matrix
2. get the value of the matrix
3. set the value of the inverse
4. get the value of the inverse
'
makeCacheMatrix <- function(x = matrix()) { #initialize value of x (data type is matrix) upon function makeCacheMatrix is called
        inv <- NULL #initialize inverse (inv) to null upon makeCacheMatrix is called
        set <- function(y) { #reset value of x to y, and inverse to NULL
                x <<- y
                inv <<- NULL
        }
        get <- function() x #return value of matrix x
        setinverse <- function(inverse) inv <<- inverse #set inv to inverse, '<<-' indicates change to value in parent environtment
        getinverse <- function() inv #return value of inverse
        list(set = set, get = get, #set names of list and its values after makeCacheMatrix is called
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
'
The following function calculates the inverse of the special "matrix" created with the above function. 
However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips 
the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
the cache via the setinverse function.
'
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse() #get inverse matrix from previous makeCacheMatrix call
        if(!is.null(inv)) { #if there is stored inverse matrix, return its value directly
                message("getting cached data")
                return(inv)
        }
        #if inverse is NULL, get matrix obtained from makeCacheMatrix call, and compute its inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv) #update inv value
        inv
}