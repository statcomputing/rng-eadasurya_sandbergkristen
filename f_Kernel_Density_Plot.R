sample_size_sol <- 10000
theta_sol <- 2
f_sample <- simulate_f(sample_size_sol, theta_sol)

plot(density(f_sample), col = "red")
legend("topright", legend = c("Estimated Density of f(x)"), col = c("red"), lty = c(1))