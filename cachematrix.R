## Put comments here that give an overall description of what
## your functions do
## Write a short comment describing this function
### Assignment: Caching the Inverse of a Matrix
##
## This programming assignment writes an R function that is
## able to cache potentially time-consuming computations.
## Matrix inversion is usually a costly computation and there ## may be some benefit to caching the inverse of a matrix,
## rather than computing it repeatedly (e.g. in a loop) so
## that when we need it again, it can be looked up in the
## cache rather than be recomputed.  This assignment writes
## a pair of functions that cache the inverse of a matrix.
##
## In this Programming Assignment functions take advantage of ## the scoping rules of the R language and how they can be
## manipulated to preserve state inside of an R object.
## Functions use the `<<-` operator which can be used to
## assign a value to an object in an environment that is
## different from the current environment.
##
## Below are two functions that are used to create a
## special object that stores a matrix and caches its inverse. ## If the contents of a matrix are not changing, it may make
## sense to cache the inverse of the matrix.
##
## The first function, `makeCacheMatrix` creates a special
## "matrix", which is really a data frame containing a
## function to
##
## 1.  set the value of the matrix
## 2.  get the value of the matrix
## 3.  set the value of the inverse of the matrix
## 4.  get the value of the inverse of the matrix
##
## This program writes the following functions:
##
## 1.  `makeCacheMatrix`: This function creates a special
## "matrix" object that can cache its inverse.
##
## 2.  `cacheSolve`: This function computes the inverse of the ## special "matrix" returned by `makeCacheMatrix` above. If
## the inverse has already been calculated (and the matrix
## has not changed), then `cacheSolve` should retrieve the
## inverse from the cache; it first checks to see if the
## inverse has already been calculated. If so, it `get`s the
## inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets ## the value of the inverse in the cache via the `cachesolve` ## function.
##
## Computing the inverse of a square matrix can be done with ## the `solve` function in R. For example, if `X` is a square ## invertible matrix, then solve(X)` returns its inverse.
##
## For this assignment, it can be assumeed that the matrix
## supplied is always invertible.
##



makeCacheMatrix <- function(x = matrix(nrow, ncol)) {
                        m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setsolve <- function(solve) m <<- solve
                getsolve <- function() m
                list(set = set, get = get,
                     setsolve = setsolve,
                     getsolve = getsolve)
}

##
## Write a short comment describing this function
## `makeCacheMatrix`: This function creates a special
## "matrix" object that can cache its inverse.
##The function creates a square matrix of dimension nrows and ## ncols, and it creates a function called 'set' which uses
## the `<<-` operator to assign a value to matrix y  and m
## that is in an environment that is different from the
## environment inside of the '{function}'.
## it also defines a function 'get', which is a function of
## the matrix x. It defines a function 'setsolve' which uses
## the << operator to find the inverse of the empty matrix m
## it also defines a function getsolve that will apply the
## solve function to whatever matrix is substitutes in
## place of m.
##
##
cacheSolve <- function(x, ...) {
                solve(makeCacheMatrix)
                m <- x$getsolve()
                if(!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
                data <- x$get()
                m<- solve(data,...)
                x$setsolve(m)
                m
}
##
## Return a matrix that is the inverse of 'makeCacheMatrix'
## `cacheSolve`: This function computes the inverse of the
## special "matrix" returned by `makeCacheMatrix` above. If
## the inverse has already been calculated (and the matrix
## has not changed), then `cacheSolve` should retrieve the
## inverse from the cache; it first checks to see if the
## inverse has already been calculated. If so, it `get`s the ## inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets ## the value of the inverse in the cache via the `cacheSolve` ## function.
##
