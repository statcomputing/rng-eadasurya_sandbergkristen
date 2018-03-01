## Simulate from f(x)

simulate_g <- function(sample_size, theta)  {
  alpha_0 = 2*gamma(theta)/(2*gamma(theta) + gamma(theta + (1/2)))
  indicator = rbinom(sample_size, 1, alpha_0)
  simulate_g = numeric(sample_size)
  for (i in 1:sample_size) {
    if (indicator[i] == 1) {
      simulate_g[i] <- rgamma(1, shape = theta, rate = 1)
    } 
    else { simulate_g[i] <- rgamma(1, shape = theta + (1/2), rate = 1)}
  }
  return(simulate_g)
}

simulate_f <- function(sample_size, theta) {
  count = 0
  simulate_f <- numeric(0)
  while(count < sample_size) {
    g_sample <- simulate_g(1, theta)
    uniform  <- runif(1,0,1)
    q_x      <- sqrt(4 + g_sample)*((g_sample)^(theta-1))*exp(-1*g_sample)
    g_x      <- 2*((g_sample)^(theta-1))*exp(-1*g_sample) + ((g_sample)^(theta-(1/2)))*exp(-1*g_sample)
    ratio    <- (q_x)/(g_x)
    if (uniform > ratio) { 
    }
    else {
      simulate_f <- append(simulate_f, g_sample)
      count = count + 1
    }
  }
  return(simulate_f)
}