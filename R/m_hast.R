m_hast <- function(n.iter, theta.start, t.fun, t.arg, j.s.fun, j.s.arg, j.d.fun, 
                   j.d.arg1, j.d.arg2, center){
  
  # Initialize Vectors
  theta.vec <- numeric(n.iter)
  r.vals <- numeric(n.iter - 1)
  jump.vec <- numeric(n.iter - 1)
  theta.vec[1] <- theta.start
  
  # Pass in Target and Jumping Functions
  TFUN <- match.fun(t.fun)
  JSAMP <- match.fun(j.s.fun)
  JDIST <- match.fun(j.d.fun)
  
  # Metropolis-Hastings Loop
  for(i in 2:n.iter){
    theta.curr <- theta.vec[i - 1]
    j.s.arg[center] <- theta.curr
    j.d.arg2[center] <- theta.curr
    theta.cand <- do.call(JSAMP, c(list(1),j.s.arg))
    j.d.arg1[center] <- theta.cand
    r <- (do.call(TFUN, c(theta.cand, t.arg)) / do.call(TFUN, c(theta.curr, t.arg))) *
      (do.call(JDIST, c(theta.curr, j.d.arg1)) / do.call(JDIST, c(theta.cand, j.d.arg2)))
    p.accept <- min(1,r)
    u <- runif(1)
    ifelse(u <= p.accept, theta.vec[i] <- theta.cand, theta.vec[i] <- theta.curr) 
    jump.vec[i-1] <- ifelse(u <= p.accept, 1, 0)
    r.vals[i-1] <- round(r, digits=3)
  }
  return(list(theta = theta.vec, r.vals = r.vals, efficiency = mean(jump.vec)))
}
