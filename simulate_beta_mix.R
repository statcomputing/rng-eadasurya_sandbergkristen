
simulate_beta_mix <- function(sample_size, c_theta, c_beta)  {
  alpha_0 <- beta(c_theta,1)/(beta(c_theta,1) + sqrt(2)*beta(1, c_beta) + beta(2,c_beta))
  alpha_1 <- sqrt(2)*beta(1, c_beta)/(beta(c_theta,1) + sqrt(2)*beta(1, c_beta) + beta(2,c_beta))
  indicator <- runif(sample_size, 0, 1)
  simulate_beta_mix <- numeric(sample_size)
  for (i in 1:sample_size) {
    if (indicator[i] <= alpha_0) {
      simulate_beta_mix[i] <- rbeta(1, shape1 = c_theta, shape2 = 1)
    } 
    else if (indicator[i] <= alpha_0 + alpha_1) { simulate_beta_mix[i] <- rbeta(1, shape1 = 1, shape2 = c_beta)}
    else {simulate_beta_mix[i] <- rbeta(1, shape1 = 2, shape2 = c_beta)}
  }
  return(simulate_beta_mix)
}

simulate_f_q3    <- function(sample_size, c_theta, c_beta) {
  count <- 0
  count_rejection <- 0
  simulate_f_q3 <- numeric(0)
  while(count < sample_size) {
    inst           <- simulate_beta_mix(1, c_theta, c_beta)
    q_inst         <- (inst^(c_theta - 1))/(1 + inst^2) + (sqrt(2 + inst^2))*((1 - inst)^(c_beta - 1))
    unif           <- runif(1,0,1)
    alpha_g_inst   <- inst^(c_theta - 1) + (sqrt(2))*(1 - inst)^(c_beta - 1) +  inst*((1 - inst)^(c_beta - 1))
    ratio_inst     <- q_inst/alpha_g_inst
    if(unif <= ratio_inst) {
      simulate_f_q3 <- append(simulate_f_q3, inst)
      count = count + 1
    }
    else {
      count_rejection = count_rejection + 1
    }
  }
  acceptance_rate = count/(count + count_rejection)
  return(simulate_f_q3)
}