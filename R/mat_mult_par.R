mat_mult_par <- function(A, B){
  cl <- makeCluster(8, type = "SOCK")
  if (ncol(A) != nrow(B)) stop("Matrix dimensions do not agree")
  idx   <- splitIndices(nrow(A), length(cl))
  Alist <- lapply(idx, function(ii) A[ii,,drop=FALSE])
  ans   <- clusterApply(cl, Alist, get("%*%"), B)
  stopCluster(cl)
  do.call(rbind, ans)
}

