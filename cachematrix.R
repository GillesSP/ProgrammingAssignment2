##Creates a matrix object that caches its inverse
##Use set() and get() to set and get the value of the matrix
##Use setinverse() and getinverse() to set and get the cached inverse
##inverse is set to NULL if it has not been set yet

makeCacheMatrix<-function(m=matrix()){
    inv<-NULL
    ## sets the matrix
    set<-function(m2){
        m<<-m2
        inv<<-NULL
    }
    ## returns the matrix
    get<-function() m
    ## sets the cached inverse
    setinverse<-function(inv2) inv<<- inv2
    ##returns the cached inverse. inverse is NULL if it has not yet been set
    getinverse<-function() inv
    list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


##takes a CacheMatrix and returns its inverse
##returns from Cache if already computed
##otherwise: computes the inverse, sets the cached inverse and returns the inverse
cacheSolve<-function(m,...){
    ## return from cache if possible
    inv<-m$getinverse()
    if(!is.null(inv)){
        message("Getting inverse from cache")   ##display a message when using the cache in case this was not desired
        return(inv)
    }
    ## compute inverse and set cache
    inv<-solve(m$get(),...)
    m$setinverse(inv)
    inv
}