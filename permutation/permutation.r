# homework 2 - problem 3 part 1
permn <- function(x, m, FUN=NULL) {
    if (is.null(FUN)) { 
        FUN <- function(x){
            x
        }
    } 
    # Enumerate The Permutations Of A Vector
    # Given an integer n, return a matrix whose 
    # columns enumerate various permutations of 1:n.
    # Function perms() returns all permutations in lexicographic order
    p <- perms(m)
    # The default method combines its arguments to form a vector. 
    # All arguments are coerced to a common type which is the type of 
    # the returned value, and all attributes except names are removed.
    answer <- c()
    # Generate all combinations of the elements of x taken m at a time.
    # If argument FUN is not NULL, applies a function given by the 
    # argument to each point.
    combn(x, m, function(cmb) {
        # Returns a vector or array or list of values obtained by 
        # applying a function to margins of an array or matrix.
        apply(p, 2, function(j) {
            # Take a sequence of vector, matrix or data-frame 
            # arguments and combine by columns or rows, respectively.
            res <<- cbind(res, FUN(cmb[j]))
        }) 
    })
    return(answer) 
}

# homework 2 - problem 3 part 2
expectedVal <- function(){
    val <- permn(12, 8, function(y){
                            sum(abs(diff(y)))
                        }
                )
    return (mean(val))
}
