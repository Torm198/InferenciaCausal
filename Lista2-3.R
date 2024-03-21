
data(lalonde,package="Matching")
Y_i <- lalonde$re78
R_i <- rank(lalonde$re78, ties.method = "average")
Z_i <- lalonde$treat

y_0 <-  mean(Y_i[Z_i==0])
s_0 <- var(Y_i[Z_i==0])
y_1 <-  mean(Y_i[Z_i==1])
s_1 <- var(Y_i[Z_i==1])

t <- (y_1-y_0)/sqrt((s_1/sum(Z_i))+(s_0/sum(Z_i==0)))
t_a <- t.test(Y_i[Z_i==0],Y_i[Z_i==1],var.equal = F)$statistic 
W <- sum(R_i*Z_i)-(sum(Z_i)*(sum(Z_i)+1)/2)
W_a <- wilcox.test(Y_i[Z_i==1],Y_i[Z_i==0])$statistic



{
  mc <- 10^4
  
  t_perm <- rep (0, mc)
  t_perm_a <- rep (0, mc)
  W_perm <- rep (0, mc)
  W_perm_a <- rep (0, mc)
  
  for(i in 1:mc){
    z_perm <- sample(Z_i)
    y_0 <-  mean(Y_i[z_perm==0])
    s_0 <- var(Y_i[z_perm==0])
    y_1 <-  mean(Y_i[z_perm==1])
    s_1 <- var(Y_i[z_perm==1])
    
    t_perm[i] <- (y_1-y_0)/sqrt((s_1/sum(z_perm))+(s_0/sum(z_perm==0)))
    t_perm_a[i] <- t.test(Y_i[z_perm==1],Y_i[z_perm==0],var.equal=F)$statistic
    
    W_perm[i] <- ( sum(R_i*z_perm)-(sum(z_perm)*(sum(z_perm)+1)/2))
    W_perm_a[i] <- wilcox.test(Y_i[z_perm==1],Y_i[z_perm==0])$statistic 
    
  }
  
}
c(mean(abs(W_perm)>=abs(W)),mean(abs(W_perm_a)>=abs(W_a)),wilcox.test(Y_i[Z_i==0],Y_i[Z_i==1])$p.value)
# 0.00530000 0.00530000 0.01094664
c(mean(abs(t_perm)>=abs(t)),mean(abs(t_perm_a)>=abs(t_a)),t.test(Y_i[Z_i==0],Y_i[Z_i==1])$p.value)
# 0.009000000 0.009000000 0.007892971

