simulate_f_q3_2 <- function(sample_size, c_theta, c_beta){
  alpha_1 <- beta(c_theta,1)
  alpha_2 <- (sqrt(2))*beta(1, c_beta) + beta(2, c_beta)
  p_1 <- alpha_1/(alpha_1 + alpha_2)
  count <- 0 
  count_rejection <- 0
  simulate_f_q3_2 <- numeric(0)
  while(count < sample_size) {
    k <- rbinom(1, 1, p_1)
    if (k == 1) {
      X <- rbeta(1, c_theta, 1)
      q_1_X <- (X^(c_theta - 1))/(1 + X^2)
      alpha_g_1_X <-  (X^(c_theta - 1))
      ratio_X <-  q_1_X/alpha_g_1_X
    }
    else { 
      mix_prob <- sqrt(2)*beta(1, c_beta)/(alpha_2)
      mix_ind <- rbinom(1, 1, mix_prob)
      if(mix_ind == 1) {X <- rbeta(1,1,c_beta)}
      else {X <- rbeta(1, 1, c_beta + 1)}
      q_2_X <- (sqrt(2 + X^2))*((1-X)^(c_beta - 1))
      alpha_g_2_X <- sqrt(2)*((1-X)^(c_beta - 1)) + X*(1-X)^(c_beta - 1)
      ratio_X <- q_2_X/alpha_g_2_X
    }
    u <- runif(1, 0, 1)
    if(u <= ratio_X) {
      simulate_f_q3_2 <- append(simulate_f_q3_2, X)
      count <- count  + 1
    }
    else {count_rejection <- count_rejection + 1}
  }
  acceptance_rate <- count/(count+count_rejection)
  return(simulate_f_q3_2)
}