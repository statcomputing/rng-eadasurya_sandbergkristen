## simulation from a mixture of gamma density functions
## sample_size is the required sample size from the distribution and theta is a parameter of the 'g' density function
## alpha is the weight assigned to gamma distribution with shape parameter 'theta' as opposed to "1 - alpha" that will 
##         be assigned to gamma distribution with shape parameter 'theta + 1/2'.
## indicator is a simulation vector from bernoulli that implicates which gamma distribution to simulate from so that 
##         g is a mixed distribution of both gamma's
##         

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

