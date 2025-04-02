library(tidyverse)
library(nleqslv)


#DATA CLEANING
data = read_csv("agacis.csv")

data.precip <- data |>    
  dplyr::select(-Annual) |>                   # Remove annual column 
  pivot_longer(cols = c(Jan, Feb, Mar, Apr,   # pivot the column data into one col
                        May, Jun, Jul, Aug, 
                        Sep, Oct, Nov, Dec), 
               values_to = "Precipitation",   # store the values in Precipitation
               names_to = "Month") |>         # store the months in Month
  mutate(Precipitation = case_when(Precipitation == "M" ~ NA_character_,
                                   TRUE                 ~ Precipitation))|>
  mutate(Precipitation = as.numeric(Precipitation)) |>
  filter(!is.na(Precipitation)) |>
  write_csv("data.csv")


#MLES for Gamma 
mlegamma <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dgamma(x=data, shape=alpha, rate=beta)))
  
  return(ifelse(neg, -loglik, loglik))
}

mles <- optim(par = c(1,1),
               fn = mlegamma,
               data=data.precip$Precipitation,
               neg=T)
(alpha.hat.mle <- mles$par[1])
(beta.hat.mle <- mles$par[2])


sum(log(dgamma(x=data.precip$Precipitation, shape = alpha.hat.mle, rate = beta.hat.mle)))


#MLES for Lognormal
mlelognormal <- function(data, par, neg=F){
  meanlog <- par[1]
  sdlog <- par[2]
  
  loglik <- sum(log(dlnorm(x=data, meanlog=meanlog, sdlog=sdlog)))
  
  return(ifelse(neg, -loglik, loglik))
}

mles <- optim(par = c(1,1),
              fn = mlelognormal,
              data=data.precip$Precipitation,
              neg=T)
meanlog <- mles$par[1]
sdlog <- mles$par[2]


sum(log(dlnorm(x=data.precip$Precipitation, meanlog=meanlog, sdlog=sdlog)))







#FINDING THE RATIOS 

#WEIBULL GAMMA 

-2166.496 - sum(log(dgamma(x=data.precip$Precipitation, shape = alpha.hat.mle, rate = beta.hat.mle)))

-15.34738


#WEIBULL LOGNORMAL

-2166.496 - sum(log(dlnorm(x=data.precip$Precipitation, meanlog=meanlog, sdlog=sdlog)))

37.70453


#GAMMA LOGNORMAL

sum(log(dgamma(x=data.precip$Precipitation, shape = alpha.hat.mle, rate = beta.hat.mle))) - sum(log(dlnorm(x=data.precip$Precipitation, meanlog=meanlog, sdlog=sdlog)))

53.05191



winter = data.precip |>
  filter(Month == "Dec" | Month == "Jan" | Month == "Feb")
spring = data.precip |>
  filter(Month == "Mar" | Month == "Apr" | Month == "May")
summer = data.precip |>
  filter(Month == "Jun" | Month == "Jul" | Month == "Aug")
fall = data.precip |>
  filter(Month == "Sep" | Month == "Oct" | Month == "Nov")



mlegamma <- function(data, par, neg=F){
  alpha <- par[1]
  beta <- par[2]
  
  loglik <- sum(log(dgamma(x=data, shape=alpha, rate=beta)))
  
  return(ifelse(neg, -loglik, loglik))
}

mles <- optim(par = c(1,1),
              fn = mlegamma,
              data=winter$Precipitation,
              neg=T)
walpha = mles$par[1]
wbeta = mles$par[2]

mles <- optim(par = c(1,1),
              fn = mlegamma,
              data=fall$Precipitation,
              neg=T)
falpha = mles$par[1]
fbeta = mles$par[2]

mles <- optim(par = c(1,1),
              fn = mlegamma,
              data=spring$Precipitation,
              neg=T)
spalpha = mles$par[1]
spbeta = mles$par[2]

mles <- optim(par = c(1,1),
              fn = mlegamma,
              data=summer$Precipitation,
              neg=T)
sualpha = mles$par[1]
subeta = mles$par[2]



graph = tibble(x = seq(-0.25,13, length.out=10000)) |>
  mutate(wpdf = dgamma(x, shape = walpha, rate = wbeta))|>
  mutate(fpdf = dgamma(x, shape = falpha, rate = fbeta)) |>
  mutate(supdf = dgamma(x, shape = sualpha, rate = subeta)) |>
  mutate(sppdf = dgamma(x, shape = spalpha, rate = spbeta))

ggplot(data = graph)+
  geom_line(aes(x=x,y=wpdf), color = "cyan3")+
  geom_line(aes(x=x,y=fpdf), color = "chocolate3")+
  geom_line(aes(x=x,y=sppdf), color = "chartreuse3")+
  geom_line(aes(x=x,y=supdf),color = "red3")




