## Log-Metropolis Function

l_met <- function(n.iter, theta.start, t.fun, t.fun.arg, j.fun, j.fun.arg){
  
  # Initialize Vectors
  theta.vec <- numeric(n.iter)
  r.vals <- numeric(n.iter - 1)
  jump.vec  <- numeric(n.iter - 1)
  theta.vec[1] <- theta.start
  
  # Pass in Target and Jumping Functions
  TFUN <- match.fun(t.fun)
  JFUN <- match.fun(j.fun)
  
  # Metropolis Loop
  for( i in 2:n.iter){
    theta.curr <- theta.vec[i-1]
    theta.cand <- do.call(JFUN, c(list(theta.curr), j.fun.arg)) 
    
    t.curr <- do.call(t.fun, c(theta.curr, t.fun.arg))
    t.cand <- do.call(t.fun, c(theta.cand, t.fun.arg))
    
    log.r <- t.cand - t.curr
    p.accept <- min(1, exp(log.r))
    
    u <- runif(1)
    
    ifelse(u <= p.accept, theta.vec[i] <- theta.cand, theta.vec[i] <- theta.curr)
    jump.vec[i-1] <- ifelse(u <= p.accept, 1, 0)
    r.vals[i-1] <- round(exp(log.r), digits = 3)
  }
  return(list(theta.vec = theta.vec, r.vals = r.vals, efficiency = mean(jump.vec)))
} 