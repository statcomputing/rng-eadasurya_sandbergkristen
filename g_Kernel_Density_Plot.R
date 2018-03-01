sample_size_sol <- 10000
theta_sol <- 2
g_sample <- simulate_g(sample_size_sol, theta_sol)
alpha = 2*gamma(theta_sol)/(2*gamma(theta_sol) + gamma(theta_sol + (1/2)))

plot(density(g_sample), col = "red")
curve(alpha*dgamma(x,shape = 2, rate = 1) + (1-alpha)*dgamma(x, shape = 5/2, rate = 1), xlim = c(0, max(g_sample)), add = TRUE, col = "green")
legend("topright", legend = c("Kernel Density", "True Density"), col = c("red","green"), lty = c(1,1))