library(tidyverse)
library(ggforce)

umin <- -2
umax <- 1
vmin <- -1
vmax <- 1






ggplot() +
  geom_rect(aes(xmin = umin, xmax = umax, ymin = vmin, ymax = vmax),fill = "grey80") +
  geom_circle(aes(x0 = 0, y0 = 0, r=1),fill = "darkgreen",color = "transparent",alpha = .2) +
  geom_rect(aes(xmin = -2, xmax = -1, ymin = -.5, ymax = .5),fill = "darkred",color = "transparent",alpha = .2)


u <- 3*runif(10000)-2
v <- 2*runif(10000)-1

df1 <- data.frame(cbind(u,v)) %>%
  mutate(type = case_when(
    u^2 +v^2 <= 1 ~ "c",
    u >= -2 & u <= -1 & v <= .5 & v >= -.5 ~ "s",
    T ~ "o"
  ))

ggplot() +
  geom_rect(aes(xmin = umin, xmax = umax, ymin = vmin, ymax = vmax),fill = "grey80") +
  geom_circle(aes(x0 = 0, y0 = 0, r=1),fill = "darkgreen",color = "transparent",alpha = .2) +
  geom_rect(aes(xmin = -2, xmax = -1, ymin = -.5, ymax = .5),fill = "darkred",color = "transparent",alpha = .2) +
  geom_point(data=df1,aes(x = u, y = v, color = type))


simulapi <- function(iter=10000) {
  
  u <- 3*runif(iter)-2
  v <- 2*runif(iter)-1
  
  df1 <- data.frame(cbind(u,v)) %>%
    mutate(type = case_when(
      u^2 +v^2 <= 1 ~ "c",
      u >= -2 & u <= -1 & v <= .5 & v >= -.5 ~ "s",
      T ~ "o"
    )) %>%
    group_by(type) %>%
    summarise(num = n())
  
   c_val <- filter(df1, type == 'c')[,2]/iter
   s_val <- filter(df1, type == 's')[,2]/iter
   
   c_val/s_val
  
}


df_iters <- data.frame()

for(i in 1:100){
  
  val <- simulapi()
  
  df_iters <- bind_rows(df_iters,val)
}

(max(df_iters$num) - min(df_iters$num))/ pi

(5-pi)/6

#### Question 3

u <- 3*runif(10000)-2
v <- 2*runif(10000)-1

df1a <- data.frame(cbind(u,v)) %>%
  mutate(type = case_when(
    u^2 +v^2 <= 1 ~ "c",
    u >= -2 & u <= -1 & v <= 1 & v >= -1 ~ "s",
    T ~ "o"
  ))

ggplot() +
  geom_rect(aes(xmin = umin, xmax = umax, ymin = vmin, ymax = vmax),fill = "grey80") +
  geom_circle(aes(x0 = 0, y0 = 0, r=1),fill = "darkgreen",color = "transparent",alpha = .2) +
  geom_rect(aes(xmin = -2, xmax = -1, ymin = -1, ymax = 1),fill = "darkred",color = "transparent",alpha = .2) +
  geom_point(data=df1a,aes(x = u, y = v, color = type))


simulapi2 <- function(iter=10000) {
  
  u <- 3*runif(iter)-2
  v <- 2*runif(iter)-1
  
  df1 <- data.frame(cbind(u,v)) %>%
    mutate(type = case_when(
      u^2 +v^2 <= 1 ~ "c",
      u >= -2 & u <= -1 & v <= 1 & v >= -1 ~ "s",
      T ~ "o"
    )) %>%
    group_by(type) %>%
    summarise(num = n())
  
  c_val <- filter(df1, type == 'c')[,2]/iter
  s_val <- filter(df1, type == 's')[,2]/iter
  
  2*c_val/s_val
  
}


df_iters2 <- data.frame()

for(i in 1:100){
  
  val <- simulapi2()
  
  df_iters2 <- bind_rows(df_iters2,val)
}

(max(df_iters2$num) - min(df_iters2$num))/ pi


#### Question 4


u <- 2*runif(10000)-1
v <- 2*runif(10000)-1

df1b <- data.frame(cbind(u,v)) %>%
  mutate(type = case_when(
    u^2 +v^2 <= 1 ~ "c",
    TRUE ~ "s"
  ))

ggplot() +
  #geom_rect(aes(xmin = umin, xmax = umax, ymin = vmin, ymax = vmax),fill = "grey80") +
  geom_circle(aes(x0 = 0, y0 = 0, r=1),fill = "darkgreen",color = "transparent",alpha = .2) +
  #geom_rect(aes(xmin = -2, xmax = -1, ymin = -1, ymax = 1),fill = "darkred",color = "transparent",alpha = .2) +
  geom_point(data=df1b,aes(x = u, y = v, color = type)) +
  coord_fixed()



simulapi3 <- function(iter=10000) {
  
  u <- 2*runif(iter)-1
  v <- 2*runif(iter)-1
  
  df1 <- data.frame(cbind(u,v)) %>%
    mutate(type = case_when(
      u^2 +v^2 <= 1 ~ "c",
      T ~ "s"
    )) %>%
    group_by(type) %>%
    summarise(num = n())
  
  c_val <- filter(df1, type == 'c')[,2]/iter
  s_val <- filter(df1, type == 's')[,2]/iter
  
  4*c_val/(c_val+s_val)
  
}


df_iters3 <- data.frame()

for(i in 1:100){
  
  val <- simulapi3()
  
  df_iters3 <- bind_rows(df_iters3,val)
}

(max(df_iters3$num) - min(df_iters3$num))/ pi

