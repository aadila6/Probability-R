pik <- function(i,k) {
  if(k==1){
      prob <- 0
      if (i < 2) prob <- 0 # because max dice value is 6 (1 to 6)
      if (i == 2) prob <- 1/6
      if (i == 3) prob <- 2/6
      if (i == 4) prob <- 3/6
      if (i == 5) prob <- 4/6
      if (i == 6) prob <- 5/6
      if (i == 7) prob <- 1
      return (prob)
  }
  tot <- 0
  for(j in 1:6)
      if(i+j < 8) tot <- tot + 1/6 * pik(i+j,k-1)
  return (tot)
}

pik(3,2)
pik(4,4)
pik(1,2)
pik(0,2)
pik(7,1)