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
Beta_2_5_Mean_Graph <- ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_mean)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 2, B = 5) Cumulative Mean Data") +
  xlab("Number Of Observations") + 
  ylab("Cumulative Mean") +
  geom_hline(yintercept = 0.2857143, linetype = "solid", color = "blue", size = 1.5) + #Add Y Intercept
  theme_minimal()  

########################################################################################################

#Plot Cumulative Variance Data
Beta_2_5_Variance_Graph <- ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_variance)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 2, B = 5) Cumulative Variance Data") +
  xlab("Number Of Observations") + 
  ylab("Cumulative Variance") +
  geom_hline(yintercept = 0.02551020, linetype = "solid", color = "blue", size = 1.5) + #Add Y Intercept
  theme_minimal()  

########################################################################################################

#Plot Cumulative Skewness Data
Beta_2_5_Skewness_Graph <- ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_skewness)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 2, B = 5) Cumulative Skewness Data") +
  xlab("Number Of Observations") + 
  ylab("Cumulative Skewness") +
  geom_hline(yintercept = 0.5962848, linetype = "solid", color = "blue", size = 1.5) + #Add Y Intercept
  theme_minimal()  
  
########################################################################################################

#Plot Cumulative Excess_Kurtosis Data
Beta_2_5_Kurtosis_Graph <- ggplot(beta.sample_2_5, aes(x = Number_of_Observations, y = cumulative_excess_kurtosis)) +  
  geom_line() +
  ggtitle("Lineplot for Beta(a = 2, B = 5) Cumulative Excess-Kurtosis Data") +
  xlab("Number Of Observations") + 
  ylab("Cumulative Excess-Kurtosis") +
  geom_hline(yintercept = -0.1200000, linetype = "solid", color = "blue", size = 1.5) + #Add Y Intercept
  theme_minimal()  

########################################################################################################
#For() loop to simulate new data
#Create Sample
for (i in 2:50) {
  set.seed(7272+i)
  new_sample <- rbeta(n = 500, shape1 = 2, shape2 = 5)
#Calculate New Cumulative Statistics
  cum_mean <- cummean(new_sample)
  cum_var <- cumvar(new_sample)
  cum_skew <- cumskew(new_sample)
  cum_kurt <- cumkurt(new_sample)-3
#Create Tibble of New Data
  new_data <- tibble(
  Number_of_Observations = 1:500,
  cumulative_mean_loop = cum_mean,
  cumulative_variance_loop = cum_var,
  cumulative_skewness_loop = cum_skew,
  cumulative_excess_kurtosis_loop = cum_kurt
  )
  
#Add this line to the Mean Plot 
  Beta_2_5_Mean_Graph <- Beta_2_5_Mean_Graph + 
    geom_line(data = new_data, aes(x = Number_of_Observations, y = cumulative_mean_loop), 
              color = i) 
  
#Add this line to the Variance Plot 
  Beta_2_5_Variance_Graph <- Beta_2_5_Variance_Graph + 
    geom_line(data = new_data, aes(x = Number_of_Observations, y = cumulative_variance_loop), 
              color = i)
  
#Add this line to the Skewness Plot 
  Beta_2_5_Skewness_Graph <- Beta_2_5_Skewness_Graph + 
    geom_line(data = new_data, aes(x = Number_of_Observations, y = cumulative_skewness_loop), 
              color = i)
  
#Add this line to Excess_Kurtosis plot 
  Beta_2_5_Kurtosis_Graph <- Beta_2_5_Kurtosis_Graph + 
    geom_line(data = new_data, aes(x = Number_of_Observations, y = cumulative_excess_kurtosis_loop), 
              color = i)
}

#Use Patchwork Package to put graphs into 1 2x2 Grid
#install.packages("patchwork")
library(patchwork)

# Create the combined plot
combined_plot <- (Beta_2_5_Mean_Graph + Beta_2_5_Variance_Graph) / 
  (Beta_2_5_Skewness_Graph + Beta_2_5_Kurtosis_Graph)
combined_plot

########################################################################################################

#Task Five: How Can we Model the Variation?

# Create vectors to store the 1000 statistics
means <- numeric(1000)
variances <- numeric(1000)
skewnesses <- numeric(1000)
excess_kurtoses <- numeric(1000)

# Run 1000 iterations
for (i in 1:1000) {
  set.seed(7272+i)
  new_sample <- rbeta(n = 500, shape1 = 2, shape2 = 5)
  
  # Calculate statistics for this sample
  means[i] <- mean(new_sample)
  variances[i] <- var(new_sample)
  skewnesses[i] <- skewness(new_sample)  
  excess_kurtoses[i] <- kurtosis(new_sample) - 3
}

# Create the final tibble with all 1000 results
Tibble_task_5 <- tibble(
  mean = means,
  variance = variances,
  skewness = skewnesses,
  excess_kurtosis = excess_kurtoses
)

#Plot a Histogram for Means
means_histogram <- ggplot(Tibble_task_5, aes(x = mean)) +  
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram of Means")

#Plot a Histogram for Variances
variance_histogram <- ggplot(Tibble_task_5, aes(x = variance)) +  
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram of Variances")

#Plot a Histogram for skewnesses
skewness_histogram <- ggplot(Tibble_task_5, aes(x = skewness)) +  
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram of Skewnesses")

#Plot a Histogram for excess_kurtosis
excess_kurtosis_histogram <- ggplot(Tibble_task_5, aes(x = excess_kurtosis)) +  
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red") +
  ggtitle("Histogram of Excess_Kurtoses")

#Extra
# Combine the Histograms
combined_histogram <- (means_histogram + variance_histogram) / 
  (skewness_histogram + excess_kurtosis_histogram)
combined_histogram

########################################################################################################