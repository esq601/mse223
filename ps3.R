library(tidyverse)
library(plotly)
library(rayshader)

### Problem 1 ####

v <- runif(10000)

z <- v^(4/3)
y <- runif(10000)*.75*z^(-.25)

x_sign <- runif(10000)
y_sign <- runif(10000)

z_iden <- ifelse(x_sign < .5, -1,1)
y_iden <- ifelse(y_sign < .5, -1,1)

z_mir <- z*z_iden
y_mir <- y*y_iden

ggplot(data.frame(x=z_mir, y = y_mir),aes(x=z_mir, y = y_mir)) +
  geom_point(size = .5)


### Problem 2 ####

inv <- -log(1-runif(10000))



K <- 1
c <- 2

p2_func <- function(K=1,x=0,y=0){
  K*exp(-(x^2)-(y^2) + ((cos(x*y)*x)/2))
}

df <- data.frame()

for(i in seq(1,10000)){
  
  flag <- FALSE
  while(flag == FALSE){
    
    x_iden <- ifelse(runif(1) < .5, -1,1)
    y_iden <- ifelse(runif(1) < .5, -1,1)
    ival <- ival+1
    x <- -log(1-runif(1))*x_iden
    y <- -log(1-runif(1))*y_iden
    u <- runif(1)
    uhist <- append(uhist,u)
    g <- c*exp(-abs(x)-abs(y)+abs(x))
    f <- p2_func(K=K, x = x, y = y)
    if(f > g){
      print("oh no!")
    }
    ug <- u*g
    if(ug <= f) {
      df_temp <- data.frame(x = x, y = y,cg = g,u=u,ucg = u*g, f = f)
      df <- bind_rows(df,df_temp)
      break
    } 
    
    
    
  }
  
}

ggplot(df) +
  geom_density2d(aes(x=x,y=y)) + 
  coord_equal() +
  scale_x_continuous(limits = c(-2,2))

df1 <- df %>%
  mutate(xy = x*y)

mean(df1$xy)
hist(df$y)
ggplot(df) +
  geom_density(aes(x=x))


### Problem 3 ####



sample <- data.frame(x = rnorm(1000,0,1),y = rnorm(1000,0,1),z = rnorm(1000,0,1)) %>%
  mutate(norm = 1/sqrt(x^2+y^2+z^2))

norm <- 1/sqrt(sum(sample^2))

sample1 <- sample 

plot_ly(x =sample1$x, y= sample1$y, z = sample1$z)

exp(-(1^2))

exp(2)
df <- data.frame()

while(nrow(df) < 1000) {
  print(nrow(df))
  sample <- data.frame(x = rnorm(1000,0,1),y = rnorm(1000,0,1),z = rnorm(1000,0,1)) %>%
    mutate(norm = 1/sqrt(x^2+y^2+z^2)) %>%
    transmute(x = x*norm, y = y*norm, z = z*norm) %>%
    mutate(f = exp(2*z), g=exp(2), c=runif(1000),gc = g*c) %>%
    filter(f>=gc)
  
  df <- bind_rows(df,sample)
  
}

sample2 <- sample1 %>%
  mutate(f = exp(2*z), g=exp(2), c=runif(1000),gc = g*c)

sample3 <- df[1:1000,]

plot_ly(sample3,x =~x, y=~y, z = ~z, type = "scatter3d",
        mode = "markers",opacity = 1, marker = list(size = 5,color = ~z))



### Problem 4 ####



  