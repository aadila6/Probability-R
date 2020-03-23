sim1 <- function(nreps){
    numStops <- 2
    count <- 0
    L1 <- 1:nreps
    L2 <- 1:nreps
    for (i in 1:nreps) {
        passengers <- 0
        newPassengers <- sample(0:2,1,prob = c(0.5,0.4,0.1))
        passengers <- passengers + newPassengers
        L1[i] = passengers
        if (passengers > 0) {
            passengers <- passengers - rbinom (1 , passengers ,0.2)
        }
        newPassengers <- sample(0:2,1,prob = c(0.5,0.4,0.1))
        passengers <- passengers + newPassengers
        L2[i] = passengers
    }
    L2L1Diff = L2 - L1
    
    # cat("L1Mean:  ", mean(c(L1)),"\n")
    # cat("L1Var:   ", var(c(L1)),"\n")
    # cat("L2Mean:  ", mean(c(L2)),"\n")
    # cat("L2Var:   ", var(c(L2)),"\n")
    cat("Mean(L2-L1): ", mean(c(L2L1Diff)),"\n")
    cat("Var(L2-L1):  ", var(c(L2 - L1)),"\n")
    cat("Var(L2) + Var(L1):", var(c(L1))+var(c(L2)),"\n")
    cat("Cov(L1,L2):", cov(c(L1),c(L2)))
}
sim1(1000)
