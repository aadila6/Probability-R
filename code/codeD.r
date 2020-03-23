daccum <- function(i,k) {
    p <- c(0, 1/36, 2/36, 3/36, 4/36, 5/36, 6/36, 5/36, 4/36, 3/36, 2/36, 1/36)
    if(i > 0 && k <= 0) return(0)
    if(i == 0) {
        if(k > 0) return(0)
        else return(1)
    }
    total <- 0
    for (j in 2:12) {
        total <- total + p[j] * daccum(i-1, k-j)
    }
    return(total)
}

paccum <- function(i,k) { 
    prob <- 0
    for (index in 1:i) {
        prob <- prob + daccum(index, k)
    }
    return (prob)
}

qaccum <- function(q, k){
    num <- ceiling(k/12)
    while (paccum(num, k) < q && num <= ceiling(k/2)) {
        num <- num + 1 
    }
    return (num)
}

raccum <- function(nreps, k) {
  rolls <- rep(0, nreps)
  for (i in 1:nreps) {
    dots <- 0
    while(dots < k) {
      dots <- dots + sum(sample(1:6, 2, replace = T))
      rolls[i] <- rolls[i] + 1
    }
  }
  return (rolls)
}

