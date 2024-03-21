require(tidyverse)
data(lalonde,package="Matching")


sub_lalonde <- lalonde[sample(1:445,size=445*.8),]
z <- lalonde$treat
y <- lalonde$re78
y_sub <- sub_lalonde$re78
z_sub <- sub_lalonde$treat
t <- t.test(y[z==0],y[z==1],var.equal = F)$statistic
t_sub <- t.test(y_sub[z_sub==0],y_sub[z_sub==1],var.equal = F)$statistic


R_size <- c(100,200,300,400,500,1000,2500,5000,7500,10000)
pfrt <- rep(0,length(R_size))
pfrt_hat <- rep(0,length(R_size))
pfrt_til <- rep(0,length(R_size))


for(it in 1:length(R_size)){
  mc <- R_size[it]
  t_nothing <- rep(0,mc)
  t_hat <- rep(0,mc)
  t_til <- rep(0,mc)
for(i in 1:mc){
  z_perm <- sample(z)
  z_sub_perm <- sample(z_sub)
  t_nothing[i] <- t.test(y[z_perm==0],y[z_perm==1],var.equal = F)$statistic
  t_hat[i] <- t.test(y_sub[z_sub_perm==0],y_sub[z_sub_perm==1],var.equal = F)$statistic
  
}
  pfrt[it] <- mean(abs(t_nothing)>=abs(t))
  pfrt_hat[it] <- mean(abs(t_hat)>=abs(t_sub)) 
  pfrt_til[it] <- sum(abs(t_hat)>=abs(t_sub))/(mc+1)
  
}


result <- data.frame(R_size,pfrt,pfrt_hat,pfrt_til)
result$assint <- t.test(y[z==0],y[z==1],var.equal = F)$p.value
ggplot(result |> pivot_longer(-R_size,names_to = 'estimator'),aes(x=R_size,y=value,col=estimator))+geom_line()

