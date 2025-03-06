########################################################################################################
#Task One: Describe the Population Distribution
#Load TidyVerse
library(tidyverse)

# Create a tibble with alpha and beta combinations
results <- tibble(
  Alpha = c(2, 5, 5, 0.5),
  Beta = c(5, 5, 2, 0.5)) |>
# Calculate all the statistics using mutate()
mutate(
  Mean = Alpha / (Alpha + Beta),
  Variance = (Alpha * Beta) / ((Alpha + Beta)^2 * (Alpha + Beta + 1)),
  Skewness = (2 * (Beta - Alpha)) * (sqrt(Alpha + Beta + 1)) / ((Alpha + Beta + 2) * sqrt(Alpha * Beta)),
  Excess_Kurtosis = ((6 * (((Alpha - Beta)^2) * (Alpha + Beta + 1) - 
  Alpha * Beta * (Alpha + Beta + 2))) / ((Alpha * Beta) * (Alpha + Beta + 2) * (Alpha + Beta + 3)))) |>
  mutate(Case = c("Case1","Case2","Case3","Case4")) |>
  select(Case, everything())

########################################################################################################
# Plot the 2,5 Distribution
########################################################################################################
alpha <- 2
beta <- 5
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom") +                                  # move legend to bottom
  ggtitle("Beta(a = 2, B = 5) Distribution")                           # title graph                          

########################################################################################################
# Plot the 5,5 Distribution
########################################################################################################
alpha <- 5
beta <- 5
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom") +                                  # move legend to bottom
  ggtitle("Beta(a = 5, B = 5) Distribution")                           # title graph      
########################################################################################################
# Plot the 5,2 Distribution
########################################################################################################
alpha <- 5
beta <- 2
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) +                 # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom") +                                  # move legend to bottom
  ggtitle("Beta(a = 5, B = 2) Distribution")                           # title graph      
########################################################################################################
# Plot the 0.5,0.5 Distribution
########################################################################################################
alpha <- 0.5
beta <- 0.5
q1.fig.dat <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha, beta),                      # compute the beta PDF
         norm.pdf = dnorm(x,                                    # Gaussian distribution with
                          mean = alpha/(alpha+beta),            # same mean and variance
                          sd = sqrt((alpha*beta)/((alpha+beta)^2*(alpha+beta+1)))))

ggplot(data= q1.fig.dat)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(0.5,0.5)")) +             # plot beta dist
  geom_line(aes(x=x, y=norm.pdf, color="Gaussian(0.2857, 0.0255)")) +  # plot guassian dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom") +                                  # move legend to bottom
  ggtitle("Beta(a = 0.5, B = 0.5) Distribution")                           # title graph      
########################################################################################################
#Task Two: Compute the Moments

beta.moment <- function(alpha, beta, k, centered = TRUE) {
  if(centered==FALSE){
    #Calculates mean when not centered
    mean <- function(x) {(x^k)*(dbeta(x,alpha,beta))}
    integrate(mean, lower = 0, upper = Inf)$value
  }else{
    #Calculates mean when centered
    mean <- function(x) {(x^1)*(dbeta(x,alpha,beta))}
    mean <- integrate(mean, lower = 0, upper = Inf)$value
    #Calculates variance
    var <- function(x) {((x-mean)^k)*(dbeta(x,alpha,beta))}
    integrate(var, lower = 0, upper = Inf)$value
    }
  }
#Can use this function to calculate skewness and kurtosis
########################################################################################################
#Task Three: Do Data Summaries Help?

##########################
# Generate a sample
##########################
set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details
alpha <- 2
beta <- 5
beta.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha,   # alpha parameter
                     shape2 = beta)    # beta parameter
  
#Histogram for Beta(a = 2, B = 5) Distribution
beta.sample <- data.frame(x = beta.sample)
ggplot(beta.sample, aes(x = x)) +  
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram for Beta(a = 2, B = 5) Distribution")
