library(tidyverse)


####1a ####

Y_fun <- function(rho,X,Z) {
  rho*X + sqrt(1-rho^2)*Z
}


T_fun <- function(Y){
  (990/(10*log(1-(Y))-11))+90
}

S_fun <-  function(Z){
  (90/(log(1-(Z))-1))+90
}

samples <- 10000
rho <- seq(-1,1, by = .025)

df_out <- data.frame()

for(i in rho) {
  df <- data.frame(x = rnorm(samples), z = rnorm(samples))
  
  df$Y <- mapply(Y_fun, i, df$x, df$z)
  df$U <- mapply(pnorm, df$Y)
  df$V <- mapply(pnorm, df$x)
  df$Tv <- mapply(T_fun, df$U)
  df$S <- mapply(S_fun, df$V)
  
  covdf <- cov(df$Tv,df$S)
  cov_T <- var((df$Tv))
  cov_S <- var((df$S))
  
  cor_df <- covdf/(sqrt(cov_S*cov_T))
  
  
  df_temp <- data.frame(rho=i,cor = cor_df)
  df_out <- bind_rows(df_out,df_temp)
}

ggplot(df_out, aes(x = rho, y = cor)) +
  geom_abline() +
  geom_path(color = "red")

#### 1b #####
pay_fun <- function(t,s){
  50000*exp(-.05*min(t,s))
}

err <- .05

burn <- 100
rho <- .2
N <- 100
df_main <- data.frame()

for(i in 1:burn) {
  df <- data.frame(x = rnorm(N), z = rnorm(N))
  
  df$Y <- mapply(Y_fun, rho, df$x, df$z)
  df$U <- mapply(pnorm, df$Y)
  df$V <- mapply(pnorm, df$x)
  df$Tv <- mapply(T_fun, df$U)
  df$S <- mapply(S_fun, df$V)
  df$val <-  mapply(pay_fun,df$Tv,df$S)
  df_temp <- data.frame(a = mean(df$val))
  df_main <- bind_rows(df_main,df_temp)
  
}

hist(df_main$a)

#data.frame(a = mean(df$val))

err <- mean(df_main$a)*.05
err_cal <- 1.96*sqrt(var(df_main$a))/sqrt(nrow(df_main))
iter <- burn
  
while(err_cal > err) {
  iter <- iter + 1
  df <- data.frame(x = rnorm(N), z = rnorm(N))
  
  df$Y <- mapply(Y_fun, rho, df$x, df$z)
  df$U <- mapply(pnorm, df$Y)
  df$V <- mapply(pnorm, df$x)
  df$Tv <- mapply(T_fun, df$U)
  df$S <- mapply(S_fun, df$V)
  df$val <- mapply(pay_fun,df$Tv,df$S)
  
  df_temp <- data.frame(a = mean(df$val))
  df_main <- bind_rows(df_main,df_temp)
  
  err <- 0.05*mean(df_main$a)
  err_cal <- 1.96*sqrt(var(df_main$a))/sqrt(nrow(df_main))
  
  if(iter %% 10 == 0) {
    print(paste("Iter:",iter,"Current error:",err_cal,"Target error:",err))
  }
}

mean(df_main$a)
hist(df_main$a)



#### Problem2 ####


elect <- read_csv("2016electoral.csv")

half <- sum(elect$Votes)/2

burn <- 10

N <- 10000

mat_votes <- matrix(rep(elect$Votes,N),nrow = N)
mat <- matrix(rep(elect$RandomProb,N),nrow = N)



df_main <- data.frame()

for(i in 1:burn) {
  

  #print("1")
  mat_p <- matrix(purrr::rbernoulli(N*52),nrow = N)
  
  mat_t3 <- mat_p* mat_votes
  
  df <- data.frame(votes=rowSums(mat_t3)) %>%
    mutate(val = ifelse(votes==half,1,0))
  #print("3")
  df_main<- bind_rows(df_main, data.frame(prob = 2^52*(sum(df$val)/N)))
  #print("4")
}

hist(df_main$prob)

err <- mean(df_main$prob)*.05
err_cal <- 1.96*sqrt(var(df_main$prob))/sqrt(nrow(df_main))
iter <- burn

while(err_cal > err) {
  iter <- iter + 1

  mat_p <- matrix(purrr::rbernoulli(N*52),nrow = N)
  
  mat_t3 <- mat_p* mat_votes
  
  df <- data.frame(mat_t2,votes=rowSums(mat_t3)) %>%
    mutate(val = ifelse(votes==half,1,0))
  #print("3")
  df_main<- bind_rows(df_main, data.frame(prob = 2^52*(sum(df$val)/N)))
  
  err <- 0.05*mean(df_main$prob)
  err_cal <- 1.96*(sqrt(var(df_main$prob))/sqrt(nrow(df_main)))
  
  if(iter %% 500 == 0) {
    print(paste("Iter:",iter,"Current error:",err_cal,"Target error:",err))
  }
}

hist(df_main$prob)

mean(df_main$prob)

##### 2b #####
burn <- 2

N <- 10000

mat <- matrix(rep(elect$PollProb,N),ncol = 52,byrow=TRUE)
mat_votes <- matrix(rep(elect$Votes,N),nrow = N)
for(i in 1:50){
df_main <- data.frame()

for(i in 1:burn) {
  
  
  #print("1")
  mat2 <- apply(mat,1:2,function(x){purrr::rbernoulli(1,x)})
  
  mat_t3 <- mat2 * mat_votes
  
  df <- data.frame(votes=rowSums(mat_t3)) %>%
    mutate(val = ifelse(votes>=half+15,1,0))
  #print("3")
  df_main<- bind_rows(df_main, data.frame(prob = (sum(df$val)/N)))
  #print("4")
}

#hist(df_main$prob)

err <- mean(df_main$prob)*.05
err_cal <- 1.96*sqrt(var(df_main$prob))/sqrt(nrow(df_main))
iter <- burn

while(err_cal > err) {
  iter <- iter + 1
  
  mat2 <- apply(mat,1:2,function(x){purrr::rbernoulli(1,x)})
  
  mat_t3 <- mat2 * mat_votes
  
  df <- data.frame(votes=rowSums(mat_t3)) %>%
    mutate(val = ifelse(votes>=half+15,1,0))
  #print("3")
  df_main<- bind_rows(df_main, data.frame(prob = (sum(df$val)/N)))
  
  if(iter %% 500 == 0) {
    print(paste("Iter:",iter,"Current error:",err_cal,"Target error:",err))
  }
  
  err <- 0.05*mean(df_main$prob)
  err_cal <- 1.96*(sqrt(var(df_main$prob))/sqrt(nrow(df_main)))
}


print(mean(df_main$prob))
}
#### Extra crap ####





ggplot(df_out) +
  geom_path(aes(x = rho, y = cor))

rho <- .9
df <- data.frame(x = rnorm(10000), z = rnorm(10000))

df$Y <- mapply(Y_fun, rho, df$x, df$z)
df$U <- mapply(pnorm, df$Y)
df$V <- mapply(pnorm, df$x)
df$Tv <- mapply(T_fun, df$U)
df$S <- mapply(S_fun, df$V)

covdf <- cov(df$Tv,df$S)
cov_T <- var((df$Tv))
cov_S <- var((df$S))
cor(df$Tv,df$S)
cor(df$x,df$Y)
cor(df$U,df$V)


pnorm(.5)
cor_df <- covdf/(sqrt(cov_S*cov_T))



ggplot(df, aes(x = U, y = V)) +
  geom_point()

hist(df$S)


hist(df$S)
hist(df$Tv)
log(.0001)-1.1
9.9/-10.31


df2 <- data.frame(x = seq(1:90)) %>%
  mutate(y=1-exp(-1.1/(1-x/90) +1.1))

ggplot(df2) + geom_point(aes(x,y))




samples <- 10000
rho <- seq(-1,1, by = .025)

df_outt <- data.frame()


for(i in rho) {
  df <- data.frame(x = rnorm(samples), z = rnorm(samples))
  
  df$Y <- mapply(Y_fun, i, df$x, df$z)
  df$U <- mapply(pnorm, df$Y)
  df$V <- mapply(pnorm, df$x)
  df$Tv <- -log(1-df$U)
  df$S <- -.5*log(1-df$V)
  
  covdf <- cov(df$Tv,df$S)
  cov_T <- var((df$Tv))
  cov_S <- var((df$S))
  
  cor_df <- covdf/(sqrt(cov_S*cov_T))
  
  
  df_temp <- data.frame(rho=i,cor = covdf)
  df_outt <- bind_rows(df_outt,df_temp)
}

ggplot(df_outt) +
  geom_path(aes(x = rho, y = cor))
