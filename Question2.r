NCol=30
NRow=100
# Generate the random dataset
dataset <- matrix(runif(NCol*NRow,min = 1,max=200),ncol = NCol)
head(dataset)

# Replace values between 10 and 60 with NA
dataset[dataset >= 10 & dataset <= 60] <- NA
dataset

# Count rows with missing values
missing_rows <- sum(rowSums(is.na(dataset)) > 0)

# Print the count of rows with missing values
print(missing_rows)

# Replace NA values with column means
dataset[is.na(dataset)] <- colMeans(dataset, na.rm = TRUE)[col(dataset)][is.na(dataset)]
dataset

# Calculate Pearson correlation matrix
cor_matrix <- cor(dataset, use = "pairwise.complete.obs")

# Plot the correlation heat map
heatmap(cor_matrix, col = colorRampPalette(c("blue", "white", "red")))

# Select columns with correlation <= 0.7
selected_columns <- colnames(cor_matrix)[which(cor_matrix <= 0.7, arr.ind = TRUE, useNames = TRUE)]

# Print the selected columns
print(selected_columns)


# Calculate the minimum and maximum values of the dataset
min_value <- min(dataset, na.rm = TRUE)
max_value <- max(dataset, na.rm = TRUE)

# Normalize the dataset between 0 and 10
normalized_dataset <- 10 * (dataset - min_value) / (max_value - min_value)

# Print the normalized dataset
print(normalized_dataset)

# Replace values in the dataset with 1 or 0 based on the condition
dataset <- ifelse(dataset <= 0.5, 1, 0)

# Print the updated dataset
print(dataset)


