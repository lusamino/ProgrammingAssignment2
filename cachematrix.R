## Essentially the first function define three functions that are later used
## to: 1. recover the cached data, 2. recover the cached inverse, or 3. cache
## the inverse. Then, function cacheSolve first checks if the inverse has been
## already cached, and if not, recover the data, compute the inverse and store
## it in cache.

## Definition of the three functions used

makeCacheMatrix <- function(x = matrix()) {
	
		inverseMat <- NULL

        getCache <- function() x
        setInv <- function(invMat) inverseMat <<- invMat
        getInv <- function() inverseMat
        list(getCache = getCache,
             setInv = setInv,
             getInv = getInv)
	        

}


## Calculation of the inverse

cacheSolve <- function(x, ...) {
	
       ## Return a matrix that is the inverse of 'x'
        
        inverseMat <- x$getInv()
        if(!is.null(inverseMat)) {
                message("getting cached data")
                return(inverseMat)
        }
        dataMat <- x$getCache()
        inverseMat <- solve(dataMat)
        x$setmean(inverseMat)
        inverseMat
        
}