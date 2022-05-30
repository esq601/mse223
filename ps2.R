library(tidyverse)

#### Problem 1 ####


df_new = data.frame()

reps <- c(100,1000,10000,100000)

for(i in reps) {
  time_start <- Sys.time()
  t_vec <- rexp(i)
  
  
  df <- data.frame(t = t_vec) %>%
    mutate(t_1 = exp(t/2),
           t_2 = exp(t/3), t_3 = t_1*t_2)
  
  cov_est <- mean(df$t_3) - mean(df$t_1)*mean(df$t_2)
  time_end <- Sys.time()
  time_diff <- time_end-time_start
  print(paste(i,":",cov_est,time_diff))
  
  rm(df)

}
for(i in 1:100){
  
  t_vec <- rexp(100000)
  
  
  df_new <- rbind(df_new,x=cov(exp(t_vec/2),exp(t_vec/3)))
}

colnames(df_new) <- "x"

mean(df_new$x)

hist(df_new$x)




mean(df$t_1)
mean(df$t_2)
mean(df$t_3)
cov(df$t_1,df$t_2)


#### Problem 2 ####

x <- runif(100000,0,2)
y <- runif(100000,0,2)

df <- data.frame(x=x, y=y) %>%
  mutate(i = ifelse(y<=x,1,0) 
  ) %>%
  mutate(g = sin(x*y)*i)#%>%
  # mutate(z_alt = sin(x*y_alt)) %>%
  # mutate(dif = case_when(
  #   z <= z_alt ~ "a",
  #   z > z_alt ~ "r"
  # ))
4*mean(df$g)

# df1 <- df %>%
#   group_by(diff) %>%
#   summarise(num = n())
# 
# 4*df1[1,2]/(df1[1,2]+df1[2,2])

##### Problem 3 #####

exp_life <- rexp(10000)


T_life <- data.frame(u = runif(10000)) %>%
  mutate(t = sqrt(.25 + log(1/(1-u)))-.5)

summary(T_life$t)


ggplot(T_life) +
  geom_density(aes(x = t), color = "red") +
  geom_density(data = data.frame(x=life),aes(x=x))



##### Problem 4 #####

df_out <- data.frame()

for(i in 1:10000) {
  n <- -1
  p <- 1
  
  while(p > exp(-9)) {
    p <- p*runif(1)^3
    n <- n+1
  }
  
  df_out <- rbind(df_out,n)
}

colnames(df_out) <- "N"
mean(df_out$N^2)

for(i in 0:6) {
  df_out1 <- df_out %>%
    filter(N == i)
  
  print(paste(i,":",nrow(df_out1)/nrow(df_out)))
}

ggplot(df_out) +
  geom_bar(aes(x = N))
