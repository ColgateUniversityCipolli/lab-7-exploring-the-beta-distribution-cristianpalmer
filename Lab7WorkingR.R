########################################################################################################

#Task One: Describe the Population Distribution
#Load TidyVerse
library(tidyverse)
#Load e1071 for summarizing later
library(e1071)

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
  mutate(Value = c("Population","Population","Population","Population")) |>
  mutate(Distribution = c("2,5","5,5","5,2","0.5,0.5")) |>
  select(Value, Distribution, Mean, Variance, Skewness, Excess_Kurtosis) 

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

########################################################################################################
# Generate a sample for all Histograms

set.seed(7272) # Set seed so we all get the same results.
sample.size <- 500 # Specify sample details

########################################################################################################

#Data for Beta(a = 2, B = 5) Distribution
alpha_2_5 <- 2
beta_2_5 <- 5
beta.sample_2_5 <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha_2_5,   # alpha parameter
                     shape2 = beta_2_5)    # beta parameter

########################################################################################################

#Histogram for Beta(a = 2, B = 5) Distribution
beta.sample_2_5 <- data.frame(x = beta.sample_2_5)
ggplot(beta.sample_2_5, aes(x = x)) +  
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram for Beta(a = 2, B = 5) Distribution") 

#Summary for Beta(a = 2, B = 5) Distribution
beta_2_5_summary <- beta.sample_2_5 |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = skewness(x), 
    Excess_Kurtosis = kurtosis(x) 
  )

########################################################################################################

#Data for Beta(a = 5, B = 5) Distribution
alpha_5_5 <- 5
beta_5_5 <- 5
beta.sample_5_5 <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha_5_5,   # alpha parameter
                     shape2 = beta_5_5)    # beta parameter

########################################################################################################

#Histogram for Beta(a = 5, B = 5) Distribution
beta.sample_5_5 <- data.frame(x = beta.sample_5_5)
ggplot(beta.sample_5_5, aes(x = x)) +  
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram for Beta(a = 5, B = 5) Distribution")

#Summary for Beta(a = 5, B = 5) Distribution
beta_5_5_summary <- beta.sample_5_5 |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = skewness(x), 
    Excess_Kurtosis = kurtosis(x) 
  )

########################################################################################################

#Data for Beta(a = 5, B = 2) Distribution
alpha_5_2 <- 5
beta_5_2 <- 2
beta.sample_5_2 <- rbeta(n = sample.size,  # sample size
                         shape1 = alpha_5_2,   # alpha parameter
                         shape2 = beta_5_2)    # beta parameter

########################################################################################################

#Histogram for Beta(a = 5, B = 2) Distribution
beta.sample_5_2 <- data.frame(x = beta.sample_5_2)
ggplot(beta.sample_5_2, aes(x = x)) +  
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram for Beta(a = 5, B = 2) Distribution")

#Summary for Beta(a = 5, B = 2) Distribution
beta_5_2_summary <- beta.sample_5_2 |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = skewness(x), 
    Excess_Kurtosis = kurtosis(x) 
  )
########################################################################################################

#Data for Beta(a = 0.50, B = 0.50) Distribution
alpha_0.50_0.50 <- 0.50
beta_0.50_0.50 <- 0.50
beta.sample_0.50_0.50 <- rbeta(n = sample.size,  # sample size
                         shape1 = alpha_0.50_0.50,   # alpha parameter
                         shape2 = beta_0.50_0.50)    # beta parameter

########################################################################################################

#Histogram for Beta(a = 0.50, B = 0.50) Distribution
beta.sample_0.50_0.50 <- data.frame(x = beta.sample_0.50_0.50)
ggplot(beta.sample_0.50_0.50, aes(x = x)) +  
  geom_histogram(aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram for Beta(a = 0.50, B = 0.50) Distribution")


#Summary for Beta(a = 0.50, B = 0.50) Distribution
beta_0.50_0.50_summary <- beta.sample_0.50_0.50 |>
  summarize(
    Mean = mean(x),
    Variance = var(x),
    Skewness = skewness(x), 
    Excess_Kurtosis = kurtosis(x) 
  )

########################################################################################################

#Create Tibble of all Summary data

Summary_Data <- rbind(beta_2_5_summary, beta_5_5_summary, beta_5_2_summary, beta_0.50_0.50_summary) |>
  mutate(Distribution = c("2,5","5,5","5,2","0.5,0.5")) |>
  mutate(Value = c("Sample","Sample","Sample","Sample")) |>
  select(Value, Distribution, everything())

#Merge Tables

Merged_Data <- rbind(Summary_Data, results)

########################################################################################################

#Task Four: Is Sample Size Important?
#install.packages("cumstats")
#Load cumstats library
library(cumstats)

#Compute Cumulative Mean
beta.sample_2_5 <- beta.sample_2_5 |>
  mutate("Number_of_Observations" = 1:500)
data.2_5 <- beta.sample_2_5$x
#Compute Cumulative Mean
cumulative_mean = cummean(data.2_5)
#Compute Cumulative Variance
cumulative_variance = cumvar(data.2_5)
#Compute Cumulative Skewness
cumulative_skewness = cumskew(data.2_5)
#Compute Cumulative Excess_Kurtosis
cumulative_excess_kurtosis = cumkurt(data.2_5)-3

########################################################################################################

#Plot Cumulative Mean Data
Beta_2_5_Graph <- ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_mean)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 2, B = 5) Cumulative Mean Data") +
  xlab("X values") + 
  ylab("Cumulative Mean") +
  geom_hline(yintercept = 0.2857143, linetype = "solid", color = "blue", size = 0.5) + #Add Y Intercept
  theme_minimal()  

#For Loop Cumulative Mean
for (i in 2:50) {
  set.seed(7272+i)
  new_sample <- rbeta(n = 500, shape1 = 2, shape2 = 5)
  cum_mean <- cumsum(new_sample) / seq_along(new_sample)
  new_data <- data.frame(
  Number_of_Observations = 1:500,
  cumulative_mean = cum_mean
  )
  
#Add this line to the plot with color based on iteration number
  Beta_2_5_Graph <- Beta_2_5_Graph + 
    geom_line(data = new_data, aes(x = Number_of_Observations, y = cumulative_mean), 
                     color = i)
}

########################################################################################################

#Plot Cumulative Variance Data
ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_variance)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 5, B = 2) Cumulative Variance Data") +
  xlab("X values") + 
  ylab("Cumulative Variance") +
  geom_hline(yintercept = 0.02551020, linetype = "solid", color = "blue", size = 0.5) + #Add Y Intercept
  theme_minimal()  

########################################################################################################

#Plot Cumulative Skewness Data
ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_skewness)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 5, B = 5) Cumulative Skewness Data") +
  xlab("X values") + 
  ylab("Cumulative Skewness") +
  geom_hline(yintercept = 0.5962848, linetype = "solid", color = "blue", size = 0.5) + #Add Y Intercept
  theme_minimal()  

########################################################################################################

#Plot Cumulative Excess_Kurtosis Data
ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_excess_kurtosis)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 0.50, B = 0.50) Cumulative Excess-Kurotis Data") +
  xlab("X values") + 
  ylab("Cumulative Excess_Kurtosis") +
  geom_hline(yintercept = -0.1200000, linetype = "solid", color = "blue", size = 0.5) + #Add Y Intercept
  theme_minimal()  

