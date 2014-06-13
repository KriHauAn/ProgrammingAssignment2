## This R source file provides functionality to:
## - Make an object that both holds a matrix variable
##   and has the ability to hold a cached version of
##   e.g. the inverse of this matrix.
## - Obtain the inverse of this matrix (presuming it
##   has one) utilising this cache -- i.e. the
##   calculation will only be done once as long
##   as the matrix is unchanged.

## The first function makeCacheMatrix will return
## this generalized matrix object with cache.
## It can be viewed as an object with four methods:
## SetMatrix, GetMatrix, SetCache & GetCache
## Though formally it is a list of four functions:

makeCacheMatrix <- function(Mx = matrix()) {
  # The cache starts out empty:
  Cache <- NULL
  # The first "method" SetMatrix assigns (new) content
  # to the variable Mx (i.e. the matrix) in the parent
  # environment, and it also clears Cache (i.e. resets
  # the cache):
  SetMatrix <- function(My) {
    Mx <<- My
    Cache <<- NULL
  }
  # The second "method" GetMatrix simply returns the
  # content of Mx -- i.e. returns the matrix:
  GetMatrix <- function() Mx
  # The third #method" SetCache assigns an
  # externally calculated value to the variable
  # Cache (i.e. the cache) in the parent environment:
  SetCache <- function(CalcValue) Cache <<- CalcValue
  # The final "method" GetCache simply returns the
  # content of Cache -- i.e. returns the cache:
  GetCache <- function() Cache
  # The function returns a list of these four functions:
  list(SetMatrix = SetMatrix,
       GetMatrix = GetMatrix,
       SetCache = SetCache,
       GetCache = GetCache)
}


## The second function cacheSolve takes as argument
## an object created by makeCacheMatrix and returns
## the inverse of it. If the cache is empty the inverse
## is calculated using the solve() function, and the
## result is saved to the cache before being returned.
## cacheSolve allows additional arguments, which if
## given will simply be passed on to the solve() function.

cacheSolve <- function(CacheMatrix, ...) {
  # First check the cache:
  InverseM <- CacheMatrix$GetCache()
  # If the cache is not empty (not null)
  # we presume that the value there is a
  # prior computation of the matrix inverse,
  # and we simply return this value:
  if(!is.null(InverseM)) {
    message("getting cached data")
    return(InverseM)
  }
  # Else calculate the inverse:
  Mx <- CacheMatrix$GetMatrix()
  InverseM <- solve(Mx, ...)
  # Save the result to the cache:
  CacheMatrix$SetCache(InverseM)
  # And return the obtained inverse matrix:
  InverseM
}

## Postscript:
## It would seem smarter (or at least safer) to
## build the function cacheSolve into the Matrix
## object such that this cache could not otherwise
## be manipulated.
## The object should thus only have the "methods"
## SetMatrix, GetMatrix and GetInverse (i.e. cacheSolve)

