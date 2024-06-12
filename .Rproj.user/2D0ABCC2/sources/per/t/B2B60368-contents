# Load necessary libraries
library(readxl)
library(neuralnet)
library(Metrics)
library(ggplot2)

# Load the dataset
data <- read_excel("ExchangeUSD.xlsx", col_types = c("date", "numeric", "numeric"))

# Select only the 3rd column (USD/EUR exchange rates)
exchange_rates <- data[[3]]

# Define the lagged input features up to t-4
lags <- 4  # Number of lags up to t-4

# Create input/output matrices
input_features <- data.frame(matrix(nrow = length(exchange_rates) - lags, ncol = lags))

output_rates <- exchange_rates[(lags + 1):length(exchange_rates)]

# Fill in the input features with lagged data
for (i in 1:lags) {
  input_features[, i] <- exchange_rates[(lags + 1 - i):(length(exchange_rates) - i)]
}

# Rename columns of the input features
colnames(input_features) <- paste0("Lag_", 1:lags)

# Combine the input and output data into a single data frame
data_combined <- cbind(input_features, Output = output_rates)

head(data_combined)

# Split the data into training and testing sets
train_size <- 400

train_data <- data_combined[1:train_size, ]
test_data <- data_combined[(train_size + 1):nrow(data_combined), ]

# Normalization function
normalize <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

# Normalize the data
train_data <- as.data.frame(lapply(train_data, normalize))

summary(train_data)

test_data <- as.data.frame(lapply(test_data, normalize))

summary(test_data)

nn_model_1 <- neuralnet(Output ~ Lag_1 + Lag_2 + Lag_3+Lag_4,
                        data = train_data, hidden = c(5),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_1)


nn_model_2 <- neuralnet(Output ~ Lag_1 ,
                        data = train_data, hidden = c(5),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_2)


nn_model_3 <- neuralnet(Output ~ Lag_1 + Lag_2, 
                        data = train_data, hidden = c(5),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_3)


nn_model_4 <- neuralnet(Output ~ Lag_1 + Lag_2 + Lag_3,
                        data = train_data, hidden = c(5),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_4)


nn_model_5 <- neuralnet(Output ~ Lag_1 + Lag_2 + Lag_3+Lag_4,
                        data = train_data, hidden = c(10),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_5)


nn_model_6 <- neuralnet(Output ~ Lag_1, 
                        data = train_data, hidden = c(10),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_6)


nn_model_7 <- neuralnet(Output ~ Lag_1 + Lag_2 ,
                        data = train_data, hidden = c(10),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_7)


nn_model_8 <- neuralnet(Output ~ Lag_1 + Lag_2 + Lag_3,
                        data = train_data, hidden = c(10),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_8)


nn_model_9 <- neuralnet(Output ~ Lag_1 + Lag_2 + Lag_3+Lag_4,
                        data = train_data, hidden = c(5,2),  
                        linear.output = TRUE, act.fct = "logistic")
plot(nn_model_9)


nn_model_10 <- neuralnet(Output ~ Lag_1,
                         data = train_data, hidden = c(5,2),  
                         linear.output = TRUE, act.fct = "logistic")
plot(nn_model_10)


nn_model_11 <- neuralnet(Output ~ Lag_1+ Lag_2,
                         data = train_data, hidden = c(5,2),  
                         linear.output = TRUE, act.fct = "logistic")
plot(nn_model_11)


nn_model_12 <- neuralnet(Output ~ Lag_1+ Lag_2+ Lag_3,
                         data = train_data, hidden = c(5,2),  
                         linear.output = TRUE, act.fct = "logistic")
plot(nn_model_12)


nn_model_13 <- neuralnet(Output ~ Lag_1+ Lag_2+ Lag_3+Lag_4,
                         data = train_data, hidden = c(5,4),  
                         linear.output = TRUE, act.fct = "logistic")
plot(nn_model_13)


nn_model_14 <- neuralnet(Output ~ Lag_1,
                         data = train_data, hidden = c(5,4),  
                         linear.output = TRUE, act.fct = "logistic")
plot(nn_model_14)


nn_model_15 <- neuralnet(Output ~ Lag_1+ Lag_2,
                         data = train_data, hidden = c(5,4),  
                         linear.output = TRUE, act.fct = "logistic")
plot(nn_model_15)


# Define a function to calculate confusion metrics
calculate_confusion_metrics <- function(model, test_data) {
  # Make predictions on the test data using the model
  predictions <- compute(model, test_data[, -ncol(test_data)])$net.result
  
  # Actual output values
  actual_values <- test_data$Output
  
  # Calculate confusion metrics
  rmse_value <- rmse(actual_values, predictions)
  mae_value <- mae(actual_values, predictions)
  mape_value <- mape(actual_values, predictions)
  smape_value <- smape(actual_values, predictions)
  
  # Return the metrics as a list
  return(list(rmse = rmse_value, mae = mae_value, mape = mape_value, smape = smape_value))
}

# List to store confusion metrics for each model
confusion_metrics_list <- list()

# Calculate confusion metrics for each model
for (i in 1:15) {
  # Get the model variable
  model_var <- paste0("nn_model_", i)
  model <- get(model_var)
  
  # Calculate confusion metrics
  confusion_metrics <- calculate_confusion_metrics(model, test_data)
  
  # Store error confusion in the list
  confusion_metrics_list[[model_var]] <- confusion_metrics
}

# Print confusion metrics for each model
for (model_var in names(confusion_metrics_list)) {
  cat("\nError metrics for", model_var, ":\n")
  cat("RMSE:", confusion_metrics_list[[model_var]]$rmse, "\n")
  cat("MAE:", confusion_metrics_list[[model_var]]$mae, "\n")
  cat("MAPE:", confusion_metrics_list[[model_var]]$mape, "\n")
  cat("SMAPE:", confusion_metrics_list[[model_var]]$smape, "\n")
}

# Identify the best model (e.g., based on lowest RMSE)
best_model_name <- names(confusion_metrics_list)[which.min(sapply(confusion_metrics_list, function(x) x$rmse))]

# Retrieve the best model
best_model <- get(best_model_name)

# Make predictions on the test data
predictions <- compute(best_model, test_data[, -ncol(test_data)])$net.result

# Actual output values
actual_values <- test_data$Output

# Combine actual and predicted values into a data frame
results <- data.frame(actual_values, predictions)

# Visualize the actual vs. predicted values using ggplot2
ggplot(results, aes(x = actual_values, y = predictions)) +
  geom_point(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Actual Output", y = "Predicted Output", title = paste("Actual vs. Predicted Output for", best_model_name)) +
  theme_minimal()

# Plot the architecture of the best model
plot(best_model)
