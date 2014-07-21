## makeCacheMatrix accepts a matrix and will output a 'special matrix' (which is a list of functions, set, get, setInv and getInv)
##
## 'set' allows to change the matrix 
## 'get' will retrieve the stored matrix
## 'setInv' is a function that gives the ability to a set the value of the inv variable
## 'getInv' will retrieve the stored value for the inv variable

## cacheSolve will accept a 'makeCacheMatrix' matrix 
##
## this function will return the inverse of our 'special matrix'.
## If the inv variable (retrieved through the getInv function) is not NULL, it's value it returned
## If inv is NULL, the inverse matrix is found using solve(), that value is set to inv of our 'special matrix' 
## and stored for safe keeping.


#############FUNCTION ONE#############

#makeCacheMatrix will accept a matrix and output a 'special matrix' which is a list of functions.
makeCacheMatrix <- function (x=matrix()){
	
	#Initial calls to makeCacheMatrix will set our private object to NULL 
	#because we haven't had a chance to create it's cached value
	inv <- NULL
	
	#create the set function, this function gives our special matrix the ability
	#to give us a new matrix, this also wipes our any stores inv values.	
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	
	#create the get function, this allows you to access the value associate 
	#to your special matrix
	get <- function() x
	
	#create the setInv function, this gives our special matrix the ability 
	#to set the 'inv' variable so that we may save repeated computation
	setInv <- function(x) inv<<- x
	
	#create the getInv function, this gives our special matrix the ability 
	#to retrive the value stored in the inv variable
	getInv <- function() inv
	
	#return the output which is a list of functions as defined above
	list (set=set,get=get,setInv=setInv,getInv=getInv)
}



#############FUNCTION TWO#############

# cacheSolve will accept a 'makeCacheMatrix' matrix and output the inverse of that function.
cacheSolve <- function(x){
	#set the inv variable to be the value of executing the 'getInv' function 
	#which is defined through initializing our special matrix in the above function
	inv <- x$getInv()

	#Check the value obtained from our function call above ** x$getInv() **
	if(!is.null(inv)){
		# If there is a value, we know we've processed this special matrix before 
		#and so we can return the stores inv without doing the computation
		message('Getting Cache Data...')
		return (inv)
	}

	#############################################################
	# At this point, we know the value of 'inv' in our special 	#
	# matrix is NULL since we've failed our test above. 		#
	# We want to fix this to save computation time. This means:	#
	# A. we haven't found the inverse for this matrix and 		#
	# B. we need to set the inv to prevent repeated computation #
	#############################################################

	#find the inverse of our matrix by executing the solve() function 
	#on the output of our get() function defined in the function above
	
	matrix <- x$get()

	inv <- solve(matrix)

	#Set the inverse found in the step above using our 'setInv()' function
	x$setInv(inv)

	#return the inverve matrix
	return(inv)

}
