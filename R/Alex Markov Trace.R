#
#trace <- markov_trace(expand_init, eval_trans_mat)
#

# Versions 1 with for loop 6 ms 2 ms
i=0 
start_time <- Sys.time()
x1 <- expand_init
x2 <- cbind(x1,i)
for (i in 1:360) {
  x1 <- x1 %*% eval_trans_mat[i,,]
  x2 <- rbind(x2, cbind(x1,i) )
}
end_time <- Sys.time()
lasted1 <- end_time - start_time 


# Version 2 -- Vectorized
start_time <- Sys.time()
x1 <- matrix(rep(expand_init, each=360),nrow=360)
x1 <- cbind(x1, seq(1:360)) 

x0 <<- expand_init
matrix_mult <- function(x) {
  i <- x[11]
  z <- x0 %*% eval_trans_mat[i,,]
  x0 <<- z
  x <- cbind(z, i)
  }

x2 <- apply(x1,1, function(x) matrix_mult(x))
x2 <- t(x2)
colnames(x2) <- c(colnames(expand_init), 'i')
end_time <- Sys.time()
lasted2 <- end_time - start_time 




