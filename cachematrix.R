# Copyright (C) 2015 Khairul Azhar Kasmiran. All rights reserved.
#
# Standard disclaimer applies:
#
# THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS CODE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
# This code was produced using the facilities of Universiti Putra Malaysia,
# Malaysia.

# ------------------------------------------------------------------------------

## The 2 functions below work together to provide caching of inverse matrices.
## Caching here means that for a particular matrix x, its inverse should only be
## computed once even though many requests for the inverse are made.

## Example usage:
##
## > matrix_a <- makeCacheMatrix(matrix(c(4, 3, 3, 2), 2, 2))
## > cacheSolve(matrix_a)
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
## > cacheSolve(matrix_a)
## Getting cached inverse matrix.
##      [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4

## Note that you are NOT allowed to run code during the assessment, unless you
## really know what you're doing. One reason is that someone might accidentally
## write R code that deletes important files on your computer (doesn't seem
## likely but better to be safe than sorry).

## -------------

## makeCacheMatrix effectively bundles together a matrix with its inverse. The
## inverse should be computed using cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes and caches the inverse of a matrix if it hasn't yet been
## computed; otherwise it retrieves the cached inverse matrix of the matrix. The
## output from makeCacheMatrix should be the input to this function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'.

    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached inverse matrix.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
