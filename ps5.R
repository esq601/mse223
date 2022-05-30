library(tidyverse)


#### Problem 3 ####

#Naive MC

rv_x <- function(i){
  rexp(1,rate = 1/i)
}
reps <- 1000
val_total <- vector()
for(i in 1:reps) {
  
  x1 <- rv_x(1)
  x2 <- rv_x(2)
  x3 <- rv_x(3)
  x4 <- rv_x(4)
  
  val <- max(x1+x2,x3+x4,x1+x4,x3+x2)
  val_total <- append(val_total,val)
}

std_val <- sqrt(var(val_total))
err <- (1.96*std_val)/sqrt(reps)

err
mean(val_total)
mean(val_total) + err
mean(val_total) - err


#Control Variates
val_total_cv <- vector()
val_total <- vector()

for(i in 1:reps) {
  
  x1 <- rv_x(1)
  x2 <- rv_x(2)
  x3 <- rv_x(3)
  x4 <- rv_x(4)
  
  val <- max(x1+x2,x3+x4,x1+x4,x3+x2)
  val_total <- append(val_total,val)
  
  val_cv <- x1+x4+x2+x3 - 10
  val_total_cv <- append(val_total_cv,val_cv)
  
}
cor(val_total,val_total_cv)
mean(val_total_cv)
beta <- 1


est_cv <- val_total - beta*val_total_cv


std_val_cv <- sqrt(var(est_cv))
err_cv <- (1.96*std_val_cv)/sqrt(reps)

err_cv
mean(est_cv)
mean(est_cv) + err_cv
mean(est_cv) - err_cv


df <- data.frame(x = 1:reps, reg = val_total, cv = val_total_cv) %>%
  mutate(cvest = reg-cv*beta)



ggplot(df[1:50,], aes(x = x)) +
  geom_path(aes(y = reg),color = "red") +
  geom_path(aes(y = cv),color = "blue") +
  geom_path(aes(y = cvest), color = "green")




##### Problem 1 ####


reps <- 10000
val_total <- vector()
val_total_cv <- vector()
for(i in 1:reps) {
  
  x <- rweibull(1,shape =2, scale = 1)
  
  val <- x * exp(x/2)
  
  val_total <- append(val_total,val)
  
  val_cv <- 1+x+(x^2)/2 - .5*(3+sqrt(pi))
  val_total_cv <- append(val_total_cv,val_cv)
}

cor(val_total,val_total_cv)
mean(val_total_cv)

df1 <- data.frame(x = 1:reps, val =val_total, cv = val_total_cv)

ggplot(df1[1:50,],aes(x = x)) + 
  geom_path(aes(y = val),color = "red") +
  geom_path(aes(y = cv),color = "blue")



## part 2

reps <- 10000
val_total <- vector()
val_total_cv <- vector()
for(i in 1:reps) {
  
  #x <- rweibull(1,shape =2, scale = 1)
  #x <- rexp(1)
  x <- rweibull(1,shape =2, scale = 1)
  
  val <- x * exp(x/2)
  
  val_total <- append(val_total,val)
  
  val_cv <- x^2 - 1
  val_total_cv <- append(val_total_cv,val_cv)
}

cor(val_total,val_total_cv)
mean(val_total_cv)

df2 <- data.frame(x = 1:reps, val =val_total, cv = val_total_cv)


hist(df2$val)
summary(df2$val)
summary(df1$val)
ggplot(df2[1:50,],aes(x = x)) + 
  geom_path(aes(y = val),color = "red") +
  geom_path(aes(y = cv),color = "blue")




#### Problem 4 ####


prob_value <- function(b, k, p_rate, c_mean){
  
  val <- 0
  vec <- vector()
  for(i in 1:k){
    
    #pay_prob <- 0
    #print(i)
    #pay_probt <- 0
    # for(j in seq(0:i-1)){
    #   print(j)
    #   pay_probt <- 1-pgamma()
    #   print(pay_probt)
    #   pay_prob <- pay_prob + pay_probt
    #   #print(pay_prob)
    # }
    #print(i)
    #print(pay_prob)
    #print((exp(-(b/c_mean)) * pay_prob))
    valt <- dpois(i,p_rate)* (1-pgamma(b,i,1/c_mean))
    #valt <- (1-pgamma(b,i,1/c_mean))
    #print(valt)
    val <- val+valt
    vec <- append(vec, val)
  }
  vec
}




var(df_test$cost)


hist(df$cost)

rexp(1,1/100)
rgamma(1,10,rate = 1/100)
dgamma(1,100,100)
#pgamma(11960,2500,1/100)

vector <- prob_value(12500,300,100,100)

vector

((1/100)*11960)^149
(11960^200)/factorial(200)
dpois(50,100)
(100/(11960))^10

(100^10/factorial(10))*exp(-100)

exp(-(1/100)*11600)

for(i in 1:200) {
  
}


str1 <- ppois(90,100)
strmid <- dpois(91:109,100)
str2 <- 1-ppois(109,100)

sum(str1,strmid,str2)
total <- data.frame(strata= 90:110,val = c(str1,strmid,str2)) %>%
  mutate(reps = round(10000*val))

total[1,3] <- total[1,3] -3


number <- 10000
arrivals <- rpois(number,100)
cost <- rgamma(length(arrivals),shape = arrivals,rate = 1/100)

df_test <- data.frame(arrivals,cost)



df <- data.frame()

for(i in 1:nrow(total)) {
  print(i)
  for(j in 1:total[i,3]){
    if(i == 1){
      num <- 100
      while(num>90){
        num <- rpois(1,100)
      }
      val <- rgamma(1,shape = num,rate = 1/100)
    } else  if(i == nrow(total)){
      num <- 90
      while(num<100){
        num <- rpois(1,100)
      }
      val <- rgamma(1,shape = num,rate = 1/100)
    } else {
      num <- total[i,1]
      val <- rgamma(1,shape=num,rate = 1/100)
    }
    
    df <- bind_rows(df,data.frame(bin = i, arrivals = num, val = val))
    
  }
  
}


prob_vec <- vector()
for(i in 1:2000){
  sampledf <- sample_n(df,nrow(df),replace = TRUE)
  
  sampledf <- sampledf %>%
    mutate(ab = case_when(
      val  < 11960 ~ TRUE,
      TRUE ~ FALSE
    ))
  
  prob_vec <- append(prob_vec,prop.table(table(sampledf$ab))[[1]])
  
}

quantile(prob_vec,c(.025,0.975))

