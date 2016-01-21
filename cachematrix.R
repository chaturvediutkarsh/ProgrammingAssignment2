## These functions are used to calculate and display (if,already calculated ) 
## the cached Inverse of a matrix. The first function takes all the inputs 
## and second function first checks if the inverse has already been calulated,
## displays the same inverse in this case by taking input from the first function.
## If the matrix is new then the inverse is calculated again and cached for future 
## by using first function.
 

## This function takes a matrix as an input 
## and stores it to be used in next function. 
## It has function that both assigns and retrieve values of the underlying variables.
## All the functions are stored in the list so that they can be invoked from 
## other function .

makeCacheMatrix <- function(x = matrix()) {
  Inverse=NULL  							##varaible initiales to NULL
  
  get=function() mat  							## function that returns the matrix
  
  set=function(newMat) {
    
    mat<<-newMat               						 ## function that re-assigns/sets the matrix to 
    Inverse<<-NULL            						  ## variable mat to be used later.
  }
  getInverse=function() Inverse  					   ## return the matrix inverse whenever called. 
  
  setInverse=function(newInverse) Inverse<<-newInverse  	            ## assigns the value of the argument to the Inverse Variable.
  
  list(get=get, set=set, getInverse=getInverse, setInverse=setInverse)   	## all the functiona are stored in the list to be used later.
  

}


## This function takes the variable as input. 
## This variable has the output of the previous function.
## It first checks the matrix, if it matches then it returns the cached inverse value
## else move out of the if block and recalculate the Inverse and displays it.

cacheSolve <- function(x, ...) {
  Inv=y$getInverse() 			      ## gets the value of matrix Inverse by calling first functions's subfunction.
  if(!is.null(Inv)){
    print("Cached Inverese fetched")  		## if block checks for the matrix and inverse. 
    return (Inv)                     		 ## If match found then displayed along with the print message.
  }
  data=y$get()                			## In case of no match, move out of if bloack and get the value of matrix.
 
   data1=solve(data, ...)      			## matrix inverse calculated using solve function.
 		
    ##  data1=ginv(data, ...)  			 ## additionally ginv() func of MASS library cal also be used to calucalte. 
                             			 ## ginv() can handle bigger matrices as compared to solve function. 
                             			 ## use library(MASS) and then use ginv() on the matrix.
  
    y$setInverse(data1)         
                         			 ## setting or caching the newly calculated value 
                         			 ## of matrix-inverse.
                         			 ## using setInverse() function to do so.
  
   data1                 			 ## displays the Inverse of matrix.
  
}
