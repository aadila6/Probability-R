# use simulation to find P(2 aces) when deal a 5-card hand from a
# standard deck
# think of the 52 cards as being labeled 1-52, with the 4 aces having
# numbers 1-4
sim_aces <- function(nreps) {
   count2aces <- 0  # count of number of hands with 2 aces
   for (rep in 1:nreps) {
      # replace=FALSE means unique cards    
      hand <- sample(1:52, size=5,replace=FALSE)  # deal hand; 
      aces <- intersect(1:4,hand)  # find which aces, if any, are in hand
      if (length(aces) == 2) count2aces <- count2aces + 1
}
   cat("The probability of 2 aces in a hand of 5 cards is", 
   count2aces/nreps, "\n")
}

# use simulation to find P(3 diamonds)
sim_diamonds <- function(nreps) {
   count3diamonds <- 0  # count of number of hands with 3 diamonds
   for (rep in 1:nreps) {
      hand <- sample(1:52,size=5,replace=FALSE)  # deal hand
      diamonds <- intersect(1:13,hand)  # find which diamonds, if any, are in hand
      if (length(diamonds) == 3) count3diamonds <- count3diamonds + 1
}
   cat("The probability of 3 diamonds in a hand of 5 cards is", 
   count3diamonds/nreps, "\n")
}


sim_aces(1000)
sim_diamonds(1000)