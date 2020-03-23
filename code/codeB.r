simB <- function(nreps){
    N = rgeom(nreps, 0.15) + 1
    D = abs(N - 11)
    ND = N*D 
    cat("Cov2[N,D] = ", cov(N,D))
}
simB(10000000)