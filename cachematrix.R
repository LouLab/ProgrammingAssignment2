## makeCacheMatrix() is a function that creates a matrix object capable of cacheing its inverse
## cacheSolve() then returns the inverse matrix calculation of the matrix object previously created by the 
## makeCacheMatrix() function

# makeCacheMatrix() creates a matrix object, stores it and allows for data to be written into it
# then returns a list of functions as named elements
makeCacheMatrix <- function(x = matrix()) {
        matCache<- NULL
        setMat<- function(z)    {
                x <<- z
                matCache<<-NULL
        }
        getMat<- function() x   
        invCache<- function(solve) matCache<<- solve       
        getinvCache<- function() matCache
        list(setMat=setMat, getMat=getMat, invCache=invCache, getinvCache=getinvCache) 
}        

# cacheSolve() calculates the inverse of a matrix object created by makeCacheMatrix
# if there is already a cached value then the function returns it
# else it calculates the inverse of the matrix given in the formal argument and stores it in the cache
# then returns the inverse matrix value
cacheSolve <- function(x=matrix(), ...) {
 
        matInv<- x$getinvCache()
        if(!is.null(matInv))    {
        message("retrieving cache, please wait...")
        return(matInv)
}
        mat<- x$getMat()
        matInv<- solve(mat, ...)
        x$invCache(matInv)
        matInv
}        
