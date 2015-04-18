# ------------------------------------------------------------------------------
# rprog-013 - R Programming
# Programming Assignment 2: Lexical Scoping 
# Week 3
# Student: Bruce Bond-Myatt (6606297)
# ------------------------------------------------------------------------------ 

makeCacheMatrix <- function(x = matrix()) {
# ------------------------------------------------------------------------------
# This function creates a special "matrix" object that can cache its inverse.
#
# Args: 
#       x: Square invertible matrix that should have its inverse cached
#       
# Returns: Special object - that stores the original square matrix and its 
#          inverse 
# ------------------------------------------------------------------------------ 
        # Check to see if arguement is a matrix
        if(!is.matrix(x)) {
                message("Error: [makeCacheMatrix] Arguement must be a matrix")
                return()
        }

        # Check for squareness
        xdim <- dim(x)
        if(!(xdim[1] == xdim[2])){
                message("Error: [makeCacheMatrix] Matrix is not square - this function will not solve")
                return()
        }
        
        # Check to see if the arguement is an empty matrix (NA)
        if(anyNA(x)) {
                message("Warning: [makeCacheMatrix] Passed parameter is NA - Inversion will also be NA")
                return(x)
        }
        
        # OK lets get to work - initialise this enclosure
        invM = matrix()
        
        # Returns the original matrix
        get <- function() {
                x
        } 
        
        # Stores the inverse of the input matrix in the enclosure
        setinvM <- function(im){
                invM <<- im 
        } 

        # Returns the stored inverted matrix
        getinvM <- function(){
                invM
        }
        
        list(get = get,
             setinvM = setinvM, 
             getinvM = getinvM) 
}



cacheSolve <- function(x, ...) {
# ------------------------------------------------------------------------------
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve should retrieve the 
# inverse from the cache.
#
# Args:
#       x: Special object created by the function makeCacheMatrix
#       
# Returns:
#       Inverse of matrix used as an arguement to makeCacheMatrix
# ------------------------------------------------------------------------------        
        # Lets check if this is the special matrix. 
        # If it is a normal matrix calculate the inverse and return it
        if(is.matrix(x)){
                message("Warning: [cacheSolve] Parameter is not the special matrix -- just solving")
                        inv_matrix <- solve(x)
                        return(inv_matrix)
        }
        if(!is.list(x)) {
                message("Error: [cacheSolve] Arguement is not matrix or special matrix ")
                return(x)        
        }
        # Now lets check if we have cached the inversion of the matrix
        inv_mat <- x$getinvM()
        
        # if its NOT got any NA's then we have cached it. 
        # This test uses the fact that matrix() is used to initialise the variable inv_mat. 
        # Matrix() creates a 1 by 1 matrix with NA in the cell
        if(!anyNA(inv_mat)) {
                message("Info: Using Cached Inversion")
                return(inv_mat)
        }
        # Ok we did not cache it -- lets get the matrix and invert it
        matrix <- x$get()
        inv_data <- solve(matrix)
        
        # Set the cache with the inverted matrix
        x$setinvM(inv_data)
        
        # Return the inverted matrix
        return(inv_data)
}


testHarness <- function(){
# ------------------------------------------------------------------------------
# This function runs a series of tests on makeCacheMatrix and cacheSolve
# Args:
#       None
#       
# Returns:
#       Test outcomes for each test run
# ------------------------------------------------------------------------------        
        # Setup some data 
        xSquare <- matrix(c(-1, -2, 1, 1), 2,2)
        xNotSquare <- rbind(c(1,-1/4, 2), c(-1/4, 1, 3))
        xEmpty <- matrix()
        xNotMatrix <- c(1, 2, 3)
        
        # Start the tests - ouput to the console
        message("Starting makeCacheMatrix tests....", "\n")
        message("### Test1: input is a Square Matrix", "\n")
        t1 <- makeCacheMatrix(xSquare)
        print(t1)
        
        message("\n", "Test1 Try solve run 1")
        print(cacheSolve(t1))
        
        message("\n", "Test1 Try solve run 2")
        print(cacheSolve(t1))
        
        message("Test1 Checking inversion - inverting the inversion")
        t1a <- solve(cacheSolve(t1))
        print(t1a)
        message("Test 1 Check if the Inversion of the inversion is equal to the original")
        print(identical(xSquare, t1a))

        
        message("\n","###Test2: Input is Non square Matrix", "\n")
        t2 <- makeCacheMatrix(xNotSquare)
        print(t2)
        
        message("\n", "###Test3: input is an Empty Matrix", "\n")
        t3 <- makeCacheMatrix(xEmpty)
        print(t3)
        
        message("\n", "###Test4: Input is not a matrix", "\n")
        t4 <- makeCacheMatrix(xNotMatrix)
        print(t4)
        
        message("\n", "Testing cacheSolve....", "\n")
        message("###Test5: cacheSolve - Use special matrix", "\n")
        t5 <- cacheSolve(t1)
        print(t5)
        
        message("\n", "###Test6: cacheSolve - use Square matrix ", "\n")
        t6 <- cacheSolve(xSquare)
        print(t6)
        
        message("\n", "###Test7: cacheSolve - use Vector", "\n")
        t7 <- cacheSolve(xNotMatrix)
        print(t7)

        # Following thanks to Karl Schultz
        message("\n", "###Test8: Checking Cache...", "\n")
        n <- 3
        mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
        matCached <- makeCacheMatrix(mat)
        matSolved1 <- cacheSolve(matCached)
        matSolved2 <- cacheSolve(matCached)
        if (!identical(matSolved1, matSolved2)) {
                message("Fail: Cached version does not match solved version")
        } else {
                message("OK: Cached version MATCHES solved version")
        }

        # # Following thanks to Karl Schultz
        message("\n", "###Test9: Time Test", "\n")
        n <- 128
        mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
        matCached <- makeCacheMatrix(mat)
        time1 <- system.time(matSolved1 <- cacheSolve(matCached))
        time2 <- system.time(matSolved2 <- cacheSolve(matCached))
        if (time1["user.self"] < time2["user.self"]) {
                message("Solve time is less than cache time")
        } else {
                message("OK: Solve time is GREATER than cache time")
        }
        
}
