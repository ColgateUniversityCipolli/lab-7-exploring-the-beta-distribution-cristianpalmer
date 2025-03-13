########################################################################################################
#LAB 8
########################################################################################################

#Task 6: Collect and Clean Data:
library(tidyverse)

#Read in world bank data csv
world_bank_data <- read_csv("World_Bank_Data.csv") 

#Isolate 2022 Data
world_bank_data_2022 <- world_bank_data["2022"] |>
  #Divide by 1000 to Death rate per 1000 citizens
  mutate(`2022` = `2022`/1000) |>
  #Rename 2022 column
  rename("Percent_Died" = "2022")

#Task 7: What are alpha and beta:

###################
# MOM (Method of Moments)
# Copied and Adapted  From rcode-lecture14
###################
library(nleqslv)

MOM.beta<- function(data, par){
  alpha <- par[1]
  beta <- par[2]
    
  EX <- alpha/(alpha+beta)
  EX2 <- (((alpha+1)*alpha))/((alpha+beta+1)*(alpha+beta))
  m1 <- mean(data, na.rm=TRUE)
  m2 <- mean(data^2, na.rm=TRUE)
  
  return(c(EX-m1, EX2-m2)) 
}

nleqslv(x = c(1,1), 
        fn = MOM.beta,
        data= world_bank_data_2022$'Percent_Died')

###################
# MLE (Maximum Likelihood Estimator)
# Copied and Adapted From rcode-lecture14
###################
llbeta <- function(data, par, neg=FALSE){
  alpha <- par[1]
  beta <- par[2]
  loglik <- sum(log(dbeta(x=data, shape1 = alpha, shape2 = beta)), na.rm=TRUE)
  
  return(ifelse(neg, -loglik, loglik))
}

(mles <- optim(par = c(1,1),
               fn = llbeta,
               data= world_bank_data_2022$'Percent_Died',
               neg = TRUE))

alpha.hat.mle <- mles$par[1]
beta.hat.mle <- mles$par[2]

###################
# Plot Histogram of Data Superimposed MOM and MLE Distributions
###################

ggplot() +
  geom_histogram(data= world_bank_data_2022,
                 aes(x= Percent_Died,
                     y= after_stat(density)),
                 color="lightgrey") +
  xlab("Percent of Population Which Died in 2022 For Various Countries")

ggdat.exp <- tibble(x=seq(0,0.030,length.out=1000)) |>
  mutate(pdf = dexp(x, rate=1/mean(world_bank_data_2022$'Percent_Died')))
ggplot()+
  geom_histogram(data=world_bank_data_2022,
                 breaks = seq(0, 125, 25),
                 color="lightgrey")+
  geom_line(data=ggdat.exp)
