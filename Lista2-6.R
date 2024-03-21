n <- 100
n1 <- 60
n0 <- 40
y0 <- sort(rexp(n), decreasing = TRUE)
tau <- 1
y1 <- y0 + tau

tau_hat_p <- c()
V_til_p <- c()
lim_sup <- c()
lim_inf <- c()
est <- c()
for (i in 1:10^4) {
  z_permut <- sample(z)
  tau_hat_p[i] <- mean(y1[z_permut == 1]) - mean(y0[z_permut == 0])
  V_til_p[i] <- (var(y1[z_permut == 1])*sqrt(n0/ n1) + var(y0[z_permut == 0])*sqrt(n1/n0)) / n
  lim_sup[i] <- tau_hat_p[i] + 1.96*sqrt(V_til_p[i])
  lim_inf[i] <- tau_hat_p[i] - 1.96*sqrt(V_til_p[i])
  est[i] <- (tau_hat_p[i] - tau) / sqrt(V_til_p[i])
}
mean(V_til_p)
hist(est)