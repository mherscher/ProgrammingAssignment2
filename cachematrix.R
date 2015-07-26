## "The Matrix is a system, Neo. That system is our enemy."
##        -Morpheus, The Matrix (1999)
## So we solve it. :)
## These functions can be used to create a cache library of matrices and thier inverses to lessen the amount of redunant
## computation that could be done.
## Two functions are needed to be able to create and acess this matrices cache:
##              1. makeCacheMatrix:: make a list of usable functions that allows the storage and access of a given matrix
##                 and it's inverse
##              2. cacheSolve:: either pulls or caches the inverse of a given matrix



### makeCacheMatrix basically lines up a list of functions that can be used to cache information about a matrix and it's
### inverse.
### There are 4 specific functions in each list:
###       1. set:: will set the value of the matrix
###       2. get:: will get the value of the matrix
###       3. setInverse:: will set the value of the inverse matrix
###       4. getInverse:: will get the value of the inverse matrix
### Pretty strait-forward. say what you wanna do and if you wanna do it to the inverse, specify.

makeCacheMatrix <- function(x = matrix()) {
  xInverse <- NULL
  set <- function(y){
    x<<-y
    xInverse <<- NULL
  }
  get <- function() x
  setInverse <-function(inverse) xInverse <<-inverse
  getInverse <-function() xInverse
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


### cacheSolve will look through the cache to figure out if the makeCacheMatrix matrix (say that 5 times fast) has an inverse
### already calculated.
### If it can find the matrix inverse in the cache (i.e. xIverse for x is not NULL), it will pull the inverse from the cache
### and let you know it had been cached.
### If, however, there isn't a matrix inversed cached, It will use solve() to find said inverse and then cache said inverse
### into the cache so that it can be called from the cache in the future.

cacheSolve <- function(x, ...) {
  xInverse <- x$getInverse()
  if(!is.null(xInverse)){
    message("getting cached data")
    return(xInverse)
  }
  theMatrix <-x$get()
  theInverse<-solve(theMatrix,...) #bend the spoon, Neo.
  x$setInverse(theInverse)
  theInverse
}



##### Example:: To understand how these functions will work, let's take a 3 by 3 matrix and call it "exampleMatrix".
#
# > exampleMatrix <-rbind(c(1,2,0),c(-1,1,1),c(1,2,3))
# > exampleMatrix
#      [,1] [,2] [,3]
# [1,]    1    2    0
# [2,]   -1    1    1
# [3,]    1    2    3
#
#### Now, using linear algebra, we know that this matrix is, in fact, invertible with an inverse matrix of:
####          1/9   -6/9    2/9
####          4/9    3/9   -1/9
####         -3/9     0     3/9
#### As a result, it fits the criteria given for the matrices expected for this problem and we have a known value
#### to test against.
#### Now, let's set our cached version of exampleMatrix to "exampleStep1" using makeCacheMatrix & try and pull the
#### inverse of the function using cacheSolve.
#
# > exampleStep1= makeCacheMatrix(exampleMatrix)
# > cacheSolve(exampleStep1)
#            [,1]       [,2]       [,3]
# [1,]  0.1111111 -0.6666667  0.2222222
# [2,]  0.4444444  0.3333333 -0.1111111
# [3,] -0.3333333  0.0000000  0.3333333
#
#### This first time round, the inverse hadn't been stored in the cache, so when it went to pull it from the
#### cached information, it found nothing there. As a result, it computed the inverse (correctly matching)
#### and adapted the cached information to record the inverse with it's matrix.
#### Now, let's do the same again.
#
# > cacheSolve(exampleStep1)
# getting cached data
#            [,1]       [,2]       [,3]
# [1,]  0.1111111 -0.6666667  0.2222222
# [2,]  0.4444444  0.3333333 -0.1111111
# [3,] -0.3333333  0.0000000  0.3333333
#
#### This time, the function was able to find the inverse cached. So, instead of doing the calculation out,
#### It just pulled the inverse from the cache archive.
#### Thanks for taking the time to read and critique all of this. I hope the example will help troubleshoot.
