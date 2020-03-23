# The game is to toss a coin until we get ​r ​consecutive heads or reach 
# a total of ​s ​tosses, whichever comes first.
# Let X denote the number of tosses we make. We win $X. Find the minimum 
# fee that should be charged for this game if r = 3 and s = 6. Confirm via simulation.

coin_sim <- function(r, s, nreps) {
    winnings <- 0 
    count <- 0
    for (rep in 1:nreps) {
        consecutiveHead <- 0
        flag <- 0
        for (i in 1:s) {
            toss <- sample(0:1, size = 1) # return a random 0 or 1
            if (toss) { # if it is 1
                consecutiveHead <- consecutiveHead + 1
                if (consecutiveHead == r) {            
                    winnings <- winnings + i
                    count <- count + 1
                    flag <- 1
                    break
                }
            } else {
                consecutiveHead <- 0
            }
        }
        if (flag) {
            winnings <- winnings + s
            count <- count + 1
        }
        flag <- 0
    }
    return (winnings/count)
}

print(coin_sim(3,6,1000))