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

(MOMs <- nleqslv(x = c(1,1), 
        fn = MOM.beta,
        data= world_bank_data_2022$'Percent_Died'))

alpha.hat.mom <- MOMs$x[1]
beta.hat.mom <- MOMs$x[2]

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
#Graph histogram of Data
ggplot() +
  geom_histogram(data= world_bank_data_2022,
                 aes(x= Percent_Died,
                     y= after_stat(density)),
                 color="lightgrey") +
  xlab("Proportion of Population Which Died in 2022 For Various Countries")


#Graph Estimated Max Likelihood
Estimated.Max.Likelihood.Data <- tibble(x = seq(-0.0005, 0.025, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.hat.mle, beta.hat.mle)) # Compute Beta PDF

ggplot(data= Estimated.Max.Likelihood.Data)+                           # specify data
  geom_line(aes(x=x, y=beta.pdf, color="MLE Distribution")) +                      # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("blue"))+                          # change colors
  theme(legend.position = "bottom") +                                  # move legend to bottom
  ggtitle("Estimated Max Likelihood Distribution")                     # title graph  


#Graph Estimated MOM
Estimated.MOM.Data <- tibble(x = seq(-0.0005, 0.025, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.hat.mom, beta.hat.mom)) # Compute Beta PDF

ggplot(data= Estimated.MOM.Data)+                                      # specify data
  geom_line(aes(x=x, y=beta.pdf, color="MOM Distribution")) +                       # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("red"))+                           # change colors
  theme(legend.position = "bottom") +                                  # move legend to bottom
  ggtitle("Estimated Method of Moments Distribution")                     # title graph  



#Superimpose
ggplot() +
  geom_histogram(data= world_bank_data_2022,
                 aes(x= Percent_Died,
                     y= after_stat(density)),
                 color="lightgrey") +
  xlab("Proportion of Population Which Died in 2022 For Various Countries") +
  
  geom_line(aes(x=x, y=beta.pdf, color="MOM Distribution"),
            data = Estimated.MOM.Data) +                 
  geom_hline(yintercept=0)+                                        
  theme_bw() +
  
  geom_line(aes(x=x, y=beta.pdf, color="MLE Distribution"),
            data = Estimated.Max.Likelihood.Data) +         
  geom_hline(yintercept=0)+                                            
  theme_bw() +
  labs(color = "Distribution")

#Task 8: Which estimators should we use?
alpha8 = 8
beta8 = 950
sample.size <- 266 # Specify sample details


# Create empty tibble
results_tibble <- tibble(
  alpha_hat_mom = numeric(),
  beta_hat_mom = numeric(),
  alpha_hat_mle = numeric(),
  beta_hat_mle = numeric()
)

for (i in 1:1000){
  set.seed(7272+i) # Set seed so we all get the same results.
  beta.sample <- rbeta(n = sample.size,  # sample size
                       shape1 = alpha8,   # alpha parameter
                       shape2 = beta8)    # beta parameter
###################
# Calculate MOM
  
  (MOMs <- nleqslv(x = c(1,1), 
                   fn = MOM.beta,
                   data= beta.sample))
  
  alpha.hat.mom8 <- MOMs$x[1]
  beta.hat.mom8 <- MOMs$x[2]
  
###################
# Calculate MLE
  
  (mles <- optim(par = c(1,1),
                 fn = llbeta,
                 data= beta.sample,
                 neg = TRUE))
  
  alpha.hat.mle8 <- mles$par[1]
  beta.hat.mle8 <- mles$par[2]
  
###################
# Add results to tibble
  results_tibble <- results_tibble |>
    add_row(
      alpha_hat_mom = alpha.hat.mom8,
      beta_hat_mom = beta.hat.mom8,
      alpha_hat_mle = alpha.hat.mle8,
      beta_hat_mle = beta.hat.mle8
    )
}

#Plot estimated densities

#Plot estimated density for alpha for MOM

alpha_MOM <- ggplot(results_tibble) +
  geom_density(aes(x = alpha_hat_mom), fill = "blue", alpha = 0.5) +
  labs(
    title = "Density of Estimated Alpha (MOM)",
    x = "Estimated Alpha",
    y = "Density"
  ) +
  theme_minimal()

#Plot estimated density for beta for MOM

beta_MOM <- ggplot(results_tibble) +
  geom_density(aes(x = beta_hat_mom), fill = "red", alpha = 0.5) +
  labs(
    title = "Density of Estimated Beta (MOM)",
    x = "Estimated Beta",
    y = "Density"
  ) +
  theme_minimal()

#Plot estimated density for alpha for MLE

alpha_MLE <- ggplot(results_tibble) +
  geom_density(aes(x = alpha_hat_mle), fill = "pink", alpha = 0.5) +
  labs(
    title = "Density of Estimated Alpha (MLE)",
    x = "Estimated Alpha",
    y = "Density"
  ) +
  theme_minimal()

#Plot estimated density for beta for MLE

beta_MLE <- ggplot(results_tibble) +
  geom_density(aes(x = beta_hat_mle), fill = "purple", alpha = 0.5) +
  labs(
    title = "Density of Estimated Beta (MLE)",
    x = "Estimated Beta",
    y = "Density"
  ) +
  theme_minimal()

#Use Patchwork Package to put graphs into 1 2x2 Grid
library(patchwork)

# Create the combined plot
combined_plot <- (alpha_MOM + beta_MOM) / 
  (alpha_MLE + beta_MLE)
combined_plot

# Create tibble with results
library(xtable)
results_summary <- tibble(
  Parameter = c("Alpha MOM", "Beta MOM", "Alpha MLE", "Beta MLE"),
  Bias = c(
    mean(results_tibble[[1]]) - 8,
    mean(results_tibble[[2]]) - 950,
    mean(results_tibble[[3]]) - 8,
    mean(results_tibble[[4]]) - 950
  ),
  Precision = c(
    1/var(results_tibble[[1]]),
    1/var(results_tibble[[2]]),
    1/var(results_tibble[[3]]),
    1/var(results_tibble[[4]])
  ),
  MSE = c(
    var(results_tibble[[1]]) + (mean(results_tibble[[1]]) - 8)^2,
    var(results_tibble[[2]]) + (mean(results_tibble[[2]]) - 950)^2,
    var(results_tibble[[3]]) + (mean(results_tibble[[3]]) - 8)^2,
    var(results_tibble[[4]]) + (mean(results_tibble[[4]]) - 950)^2
  )
)

# Create xtable from tibble
xt <- xtable(results_summary, 
             caption = "Estimation Metrics",
             label = "tab:estimation_metrics")

print(xt, file = "estimation_metrics_table.tex", 
      include.rownames = FALSE, 
      caption.placement = "top")