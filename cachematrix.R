## caching the inverse of a matrix

## function for caching the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
            inv=NULL
            set<-function(y){
              x<<-y
              inv<<-NULL
            }
            get<-function() x
            setinv<-function(inverse) inv<<-inverse
            getinv<-function()inv
            list(set=set,get=get,
                 setinv=setinv,getinv=getinv)
}



## function to compute the inverse of a matrix

cacheSolve <- function(x, ...) {
          inv<-x$getinv()
          if(!is.null(inv)){
            message("getting cached data")
            return (inv)
          }
          matrix.data<-x$get()
          inv<-solve(matrix.data,...)
          x$setinv(inv)
          return(inv)
          
       
}
