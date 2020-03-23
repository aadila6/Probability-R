aloha <- function(p, q, nreps){
  x1_notEqual_x2 <- 0
  zero_collision <- 0
  one_collision <- 0
  two_collision <- 0
  for (i in 1:nreps) {
    # number of collisions
    temp_collision <- 0
    # Epoch 1:
    # starts off active or not active
    node_a <- runif(1) < p 
    node_b <- runif(1) < p
    if(node_a + node_b == 2) {
      # 1 collision
      temp_collision <- temp_collision + 1
    }
    if (node_a + node_b == 1) {
        # 1 node sends
        X1 <- 1 
    } else {
        # neither node sends
        X1 <- 2 
    }
    # Epoch 2:
    # number of active nodes
    active_node <- X1 
    # Try generating a new message
    if (X1 == 1 && runif(1) < q) {
      active_node <- 2
    }
    if(active_node == 1) {
    # Try sending message
        if (runif(1) < p){
            X2 <- 0 
        } else {
            X2 <- 1 
        }
    } else {
        node_a <- runif(1) < p
        node_b <- runif(1) < p
        if(node_a + node_b == 2) {
            # 1 collision happended
            temp_collision <- temp_collision + 1
            X2 <- active_node
        } else if (node_a + node_b == 1) {
            # 1 node is sent
            X2 <- active_node - 1
        } else {
            # neither node are sent
            X2 <- active_node
        } 
    }
    # if X1 != X2
    if(X1 != X2){
      x1_notEqual_x2 <- x1_notEqual_x2 + 1
      #Count the number of collisions
      if (temp_collision == 0) {
        zero_collision <- zero_collision + 1
      }
      if (temp_collision == 1) {
        one_collision <- one_collision + 1
      }
      if(temp_collision == 2) {
        two_collision <- two_collision + 1
      }
    } 
  }
  # print results:
  cat("P(C=0|(X1 != X2)) = ", zero_collision / x1_notEqual_x2, 
   "\nP(C=1|(X1 != X2)) = ",one_collision / x1_notEqual_x2, 
   "\nP(C=2|(X1 != X2)) = ",two_collision / x1_notEqual_x2 )
}

aloha(0.4,0.8,1000)
# one of the outputs:
# P(C=0|(X1 != X2)) =  0.7022587 
# P(C=1|(X1 != X2)) =  0.2977413 
# P(C=2|(X1 != X2)) =  0