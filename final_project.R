library(tidyverse)

f_0 <- 1
s_0 <- 2
p_val <- .4
mu_0 <- .1
sigma_0 <- .15
mu_1 <- .25
sigma_1 <- .3

rho <- .01
r_val <- .03
b_val <- 200



s <- function(s,r) {
  s*r
}

r_fun <- function(mu_0,sigma_0,mu_1,sigma_1,p){
  
  z <- rnorm(1)
  j <- as.numeric(rbernoulli(1, p = p))
  # print(paste(j,1-j))
  # print(z)
  exp((mu_0+z*sigma_0)*j + (mu_1+z*sigma_1)*(1-j))
}


f <- function(f,r){
  exp(r)*f
}

b <- function(rs,nrs,f,s,squigly){
  nrs*f + rs*s +s^2*squigly*s
}


der_fun <- function(fval,sval) {
  #print(paste(fval,sval))
  rt1 <- uniroot(function(x){(sval + fval*(-2-.04*x) - .02 * sval * x) / (sval*x*(1-.01*x) + fval * (200 - 2*x - .02 *x^2))},
          interval = c(0,100))
  rt2 <- uniroot(function(x){(sval + fval*(-2-.04*x) - .02 * sval * x) / (sval*x*(1-.01*x) + fval * (200 - 2*x - .02 *x^2))},
             interval = c(-100,0))
  
  if(abs(rt1$f.root) > 100){
    val <- rt2$root
  } else {
    val <- rt1$root
  }
  #print(rt1$f.root)
  if(val < 0 ) {
    0
  } else {
    val
  }
}


der_fun <- function(fval,sval) {
  #print(paste(fval,sval))
  df <- data.frame()
  for(i in seq(from =-100, to =100, by = 10)){
    rt1 <- tryCatch(pracma::fzero(function(x){(sval + fval*(-2-.04*x) - .02 * sval * x) / (sval*x*(1-.01*x) + fval * (200 - 2*x - .02 *x^2))},
                         x = i), error = function(e) NULL)
    df <- bind_rows(df, data.frame(x=rt1$x,fval=rt1$fval))
    
  }

  df2 <- df %>%
    mutate(fval = abs(fval)) %>%
    filter(fval == min(fval,na.rm = TRUE))
  #print(df)
  df2[1,1]

}

der_fun(fval = 1.03,sval = 5)

fval <- 1.03
sval <- 3
uniroot(function(x){((fval*(-.04*x-2) )-.02*sval*x + sval)/(fval*(-.02*x^2-2*x+200)+x*(1-.01*x)*sval)},
        interval = c(-100,0),extendInt = "yes",maxiter = 10000)
uniroot(function(x){((fval*(-.04*x-2) )-.02*sval*x + sval)/(fval*(-.02*x^2-2*x+200)+x*(1-.01*x)*sval)},
        interval = c(-100,100),extendInt = "yes",maxiter = 10000)


test <- function(x){(sval + fval*(-2-.04*x) - .02 * sval * x) / (sval*x*(1-.01*x) + fval * (200 - 2*x - .02 *x^2))}
test(78)
fval
sval <- 4
test(9.2)
pracma::fzero(test,x = c(-100,0))
pracma::fzero(test,x = 20)

test <- function(x,fval,sval){(((-.04*x)-2) * fval + sval)/((((-.02*(x^2))-(2*x)+200)*fval)+x*sval)}

f_1 <- f(f_0,r_val)
test(61.8,f_1,2)
1.03*(-.004*61.8 - 2) + 2
1.03*(-.002*61.8^2-2*61.8+200)+2*50
-.3/170.8

applyfun <- function(f_1,s_1){
  val <- tryCatch(der_fun(f_1,s_1), error = function(e) 0)
  if(val < 0 ) {
    0 
  } else {
    val
  }
}
applyfun(1.03,1)
lowval <- function(s1,gz){
  if(s1 > 6 & gz <.1){
    gz <- 100
  } else {
    gz
  }
  gz
}

##### finding the optimal parameter theta #####


fin_func <- function(s_0,p,mu_0,sigma_0,mu_1,sigma_1,rho,r,b,gamma,delta,n_0,N_0) {
  
  mean <-0
  mean_ux <- 0
  mmt <- 0
  mmt_ux <- 0
  eps <- .05
  i <- 0
  max_phi <- 0
  f_1 <- f(1,r)
  err <- Inf
  out_val <- data.frame()
  
  while(i < N_0 | err >= eps){
    s1 <- s(s_0,r_fun(mu_0,sigma_0,mu_1,sigma_1,p))
    #print(s1)
    gz <- applyfun(f_1,s1)
    #print(paste(s1,gz))
    # if(s1 > 6 & gz <.1){
    #   gz <- 100
    # }
    
    ux <- log(gz*s1 + ((b-s1*s_0))*f_1)

    
    mean_ux <- (mean_ux * i + ux) / (i+1)
    mmt_ux <- (mmt_ux*i+ux^2)/(i+1)
    std_ux = sqrt(mmt_ux- mean_ux^2)
    err_ux <- qnorm(1-gamma/2)*std_ux/sqrt(i)
    
    mean <- (mean * i + gz) / (i+1)
    mmt <- (mmt*i+gz^2)/(i+1)
    std = sqrt(mmt- mean^2)
    
    i <- i+1
    out_val <- rbind(out_val,data.frame(i = i, g = mean,ux = mean_ux,max = max_phi,gz = gz,s = s1))
    eps <- delta*mean
    err <- qnorm(1-gamma/2)*std/sqrt(i)
  }
  
  list(data=out_val,theta_mean = mean, theta_error = err, u_mean = mean_ux, u_error = err_ux)
}


df<- fin_func(2,p_val,mu_0,sigma_0,mu_1,sigma_1,rho,r_val,b_val,.05,.05,10,100)

View(df$data)
ggplot(df$data) +
  geom_path(aes(x = i, y = g))

ggplot(df$data) +
  geom_path(aes(x = i, y = ux))

ggplot(df$data) +
  geom_histogram(aes(x = gz))

##### using bias elimination #####




mean <-0
mmt <- 0
eps <- .05
i <- 0
mean_ux <- 0
max_phi <- 0
err <- Inf
out_val <- data.frame()

while(i < 10 | err >= eps){
  
  n_0 <- 10
  pg <- 1-(.5)^(3/2)
  N <- rgeom(1,pg) + n_0
  print(i)
  print("here1")
  
  s_1 <- replicate(2^(N+1),s(2,r_fun(mu_0,sigma_0,mu_1,sigma_1,p_val)))
  print("here2")
  
  val <- sapply(s_1,applyfun,f_1 = f_1)
  print("here3")
  
  val <- mapply(lowval,s_1,val)
  val_even <- val[c(TRUE,FALSE)]
  val_odd <- val[c(FALSE,TRUE)]
  
  print("here4")
  
  
  gz <- (mean(val) - (mean(val_odd)+mean(val_even))/2)/(pg*(1-pg)^(N-n_0)) + mean(val[1:2^n_0])
  
  ux <- log(val*s_1 + ((200-s_1*2))*f_1)
  ux_even <- ux[c(TRUE,FALSE)]
  ux_odd <- ux[c(FALSE,TRUE)]
  
  ux <- (mean(ux) - (mean(ux_odd)+mean(ux_even))/2)/(pg*(1-pg)^(N-n_0)) + mean(ux[1:2^n_0])
  # max_phi <- max(gz,max_phi)
  mean_ux <- (mean_ux * i + ux) / (i+1)
  
  mean <- (mean * i + gz) / (i+1)
  mmt <- (mmt*i+gz^2)/(i+1)
  std = sqrt(mmt- mean^2)
  
  i <- i+1
  out_val <- rbind(out_val,data.frame(i = i, g = mean,gz = gz,ux_mean = mean_ux,ux = ux))
  eps <- .05*mean
  err <- 1.96*std/sqrt(i)
}


ggplot(out_val) +
  geom_path(aes(x = i, y = g))

ggplot(out_val) +
  geom_path(aes(x = i, y = ux_mean))

print(mean+eps)
print(mean-eps)
