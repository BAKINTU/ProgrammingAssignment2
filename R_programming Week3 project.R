#Create a special matrix object called x

makeCacheMatrix<-function(x=matrix()){
  i<-NULL
  #set the value of the matrix
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  
  #get the value of the matrix
  get<-function() x
  
  #set the value of the matrix inverse
  setinverse<-function(inverse)i<<-inverse
  
  #get the value of the matrix inverse
  getinverse<-function()i
  
  #return a list of the function
  list(set=set, get=get,
       setinverse=setinverse, getinverse=getinverse)
}

#To return the inverse of matrix as computed by 'makeCacheMatrix'
cacheSolve<-function(x, ...){
  i<-x$getinverse()
  
  #Check if the inverse of the special matrix has been calculated
  if(!is.null(i)){
    message("getting cached data")
    
    #ger the inverse of the matrix and skip computation
    return(i)
  }
  
  #get the matrix object
  data<-x$get()
  
  #Solve for the inverse of the matrix and set
  i<-solve(data, ...)
  x$setinverse(i)
  i
}