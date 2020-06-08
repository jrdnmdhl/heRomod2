#
#trace <- markov_trace(expand_init, eval_trans_mat)
#

markov_trace1 <- function(init, matrix) {
# ======================
n <- 360 + 1
s <- length(init)
x1 <- matrix(rep(init, each=n),nrow=n)
res_matrix <- array(0,c(n,s,s),dimnames = list(c(), c(colnames(init)),c(colnames(init))))
res_matrix[1,1,] <- init
for (i in 2:n) {
  x1[i,] <- x1[i-1,] %*% matrix[i-1,,]
  res_matrix[i,,] <- x1[i-1,] * matrix[i-1,,]
  #x1[i,] <- colSums((res_matrix[i,,]))
}
colnames(x1) <- colnames(init)
return(list(x1,res_matrix))
} 

marmarkov_trace2 <- function(init, matrix) {
  # ======================
  n <- 360 + 1
  s <- length(init)
  x1 <- matrix(rep(init, each=n),nrow=n)
  res_matrix <- array(0,c(n,s,s),dimnames = list(c(), c(colnames(init)),c(colnames(init))))
  names(res_matrix[,,])
  res_matrix[1,1,] <- init
  for (i in 2:n) {
    #x1[i,] <- x1[i-1,] %*% matrix[i-1,,]
    res_matrix[i,,] <- x1[i-1,] * matrix[i-1,,]
    x1[i,] <- colSums((res_matrix[i,,]))
  }
  colnames(x1) <- colnames(init)
  return(list(x1,res_matrix))
}

#res1 <- markov_trace1(expand_init, eval_trans_mat)

#microbenchmark(markov_trace1(expand_init, eval_trans_mat),times = 50)
#microbenchmark(markov_trace2(expand_init, eval_trans_mat),times = 50)
