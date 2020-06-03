#
#trace <- markov_trace(expand_init, eval_trans_mat)
#
markov_trace <- function(init, matrix, v=1) {

if (v==1) {
# Versions 1 with for loop 
i=0
start_time <- Sys.time()
x1 <- init
x2 <- cbind(x1,i)
for (i in 1:n_cycles) {
  x1 <- x1 %*% matrix[i,,]
  x2 <- rbind(x2, cbind(x1,i) )
}
end_time <- Sys.time()
lasted1 <- end_time - start_time 
print(paste("Trace Markov For-Loop Version Time",lasted1))
return(x2)
} 
else {  
# Version 2 -- Vectorized
start_time <- Sys.time()
x1 <- matrix(rep(init, each=n_cycles),nrow=n_cycles)
x1 <- cbind(x1, seq(1:n_cycles)) 

x0 <<- init
j <- length(init)+1
matrix_mult <- function(x) {
  i <- x[j]
  z <- x0 %*% matrix[i,,]
  x0 <<- z
  x <- cbind(z, i)
  }

x2 <- apply(x1,1, function(x) matrix_mult(x)) 
x2 <- rbind(init,t(x2[-j,]))

end_time <- Sys.time()
lasted2 <- end_time - start_time 
print(paste("Trace Markov Vectorized Version Time",lasted2))
return(x2)
}
}

res1 <- markov_trace(expand_init, eval_trans_mat,v=1)
