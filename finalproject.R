#set.seed(123)
arrival_rate <- 5
num_days_list <- c(30, 180, 365)

results <- data.frame(num_days = num_days_list,
                      mean_payment = numeric(length(num_days_list)),
                      sd_payment = numeric(length(num_days_list)),
                      default_rate = numeric(length(num_days_list)),
                      revenue_loss = numeric(length(num_days_list)))

for (i in 1:length(num_days_list)) {
  num_days <- num_days_list[i]
  n_customers <- 0
# generate random costumers for 30,180 and 365 days that follows a poisson distribution   
  for (j in 1:num_days) {
    lambda <- arrival_rate
    u <- runif(1)
    k <- 0
    p <- exp(-lambda)
    
    while (u >= p) {
      k <- k + 1
      p <- p + (exp(-lambda) * lambda^k) / factorial(k)
    }
    n_customers <- n_customers + k
  }
  # Generate random payment amounts for each customer using an exponential distribution  
  mean_exp <- 500
  payment_amounts_exponential <- numeric(n_customers)
  
  for (j in 1:n_customers) {
    rate <- 1 / mean_exp
    u <- runif(1)
    payment_amount <- -log(1 - u) / rate
    payment_amounts_exponential[j] <- payment_amount
  }
  # Simulate credit card payments and defaults
  total_revenue <- 0
  n_defaults <- 0
  for (j in 1:n_customers) {
    if (rbinom(1, 1, 0.05)) {
      n_defaults <- n_defaults + 1
    } else {
      total_revenue <- total_revenue + payment_amounts_exponential[j]
    }
  }
  
# Calculate simulation results
  default_rate <- n_defaults / n_customers
  revenue_loss <- default_rate * total_revenue
  
  results[i, "mean_payment"] <- mean(payment_amounts_exponential)
  results[i, "sd_payment"] <- sd(payment_amounts_exponential)
  results[i, "default_rate"] <- default_rate
  results[i, "revenue_loss"] <- revenue_loss
}

print(results)

# Generate combined histogram to show frequency distribution for payment amounts

hist(payment_amounts_exponential[num_days_list == 30], col = "transparent", border = "blue", xlim = c(0, 4000),
     main = "Histogram of Payment Amounts", xlab = "Payment Amount", ylab = "Frequency")
hist(payment_amounts_exponential[num_days_list == 180], col = "transparent", border = "red", add = TRUE)
hist(payment_amounts_exponential[num_days_list == 365], col = "transparent", border = "green", add = TRUE)
legend("topright", c("30 days", "180 days", "365 days"), fill = c("blue", "red", "green"))



# Create a boxplot for num_days = 30
boxplot_30 <- boxplot(payment_amounts_exponential[num_days_list == 30],
                      main = "Boxplot for Number of Days = 30",
                      xlab = "num_days = 30",
                      ylab = "Payment Amount",
                      ylim = c(0, 2000))

mean_30 <- mean(payment_amounts_exponential[num_days_list == 30])
points(mean_30, col = "red", pch = 19)
legend("topright", legend = paste("Mean:", round(mean_30, 2)), col = "red", pch = 19)

# Create a boxplot for num_days = 180
boxplot_180 <- boxplot(payment_amounts_exponential[num_days_list == 180],
                       main = "Boxplot for Number of Days = 180",
                       xlab = "num_days = 180",
                       ylab = "Payment Amount",
                       ylim = c(0, 2000))

mean_180 <- mean(payment_amounts_exponential[num_days_list == 180])
points(mean_180, col = "red", pch = 19)
legend("topright", legend = paste("Mean:", round(mean_180, 2)), col = "red", pch = 19)

# Create a boxplot for num_days = 365
boxplot_365 <- boxplot(payment_amounts_exponential[num_days_list == 365],
                       main = "Boxplot for Number of Days = 365",
                       xlab = "num_days = 365",
                       ylab = "Payment Amount",
                       ylim = c(0, 2000))

mean_365 <- mean(payment_amounts_exponential[num_days_list == 365])
points(mean_365, col = "red", pch = 19)
legend("topright", legend = paste("Mean:", round(mean_365, 2)), col = "red", pch = 19)

# Bar plot of default rate
barplot(results$default_rate, names.arg = results$num_days,
        xlab = "Number of Days", ylab = "Default Rate", main = "Default Rate by Number of Days")


