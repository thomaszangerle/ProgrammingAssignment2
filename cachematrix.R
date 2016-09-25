## These are the two functions for the R-Programming week 2 assignment
## on lexical scoping

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(m_Matrix=numeric()){
    m_InvMatrix <- NULL
    set <- function(matrixToSet){
        m_Matrix <<- matrixToSet
        m_InvMatrix <<- NULL
    }
    get <- function() m_Matrix
    setInverse <- function(invMatrix) m_InvMatrix <<- invMatrix
    getInverse <- function() m_InvMatrix
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x,...){
    m_InvMatrix <- x$getInverse()
    if(!is.null(m_InvMatrix)){
        message('getting cached data')
        return(m_InvMatrix)
    }
    m_Matrix <- x$get()
    m_InvMatrix <- solve(m_Matrix,...)
    x$setInverse(m_InvMatrix)
    m_InvMatrix
}
