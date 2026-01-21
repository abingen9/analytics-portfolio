rm(list = ls())

library(dplyr)
library(ggplot2)
library(randomForest)
library(pROC)
library(readxl)
library(arules)
library(caret)
library(tidyverse)


# 1. Lines 17-173: Random Forest Model
# 2. Lines 174-280: Associated Rules Model
# 3. Lines 281-627: Logistic Regression Model

##############################################################################
# ---------------------------
# RANDOM FORESTS MODEL
# ---------------------------
# Merge Data
# ---------------------------

orders <- read.csv("orders.csv")
order_products_train <- read.csv("order_products__train.csv")
products <- read.csv("products.csv")
aisles <- read.csv("aisles.csv")
departments <- read.csv("departments.csv")

# Merge products with aisle and department names
products_full <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id")

# Merge product details into order-product data
merged <- order_products_train %>%
  left_join(products_full, by = "product_id")

# Merge in order-level info (order_id, user_id, etc.)
merged <- merged %>%
  left_join(orders, by = "order_id")

# ---------------------------
# Clean and Prep Data
# ---------------------------
merged$reordered <- as.factor(merged$reordered)

top_products <- names(sort(table(merged$product_name), decreasing = TRUE))[1:50]

rf_data <- merged %>%
  filter(product_name %in% top_products) %>%
  select(reordered, product_name, aisle, department, add_to_cart_order) %>%
  mutate(
    product_name = droplevels(as.factor(product_name)),
    aisle = as.factor(aisle),
    department = as.factor(department)
  )

# ---------------------------
# Split into Test/Train Sets
# ---------------------------
set.seed(42)
train_index <- sample(1:nrow(rf_data), size = 0.7 * nrow(rf_data))
train_rf <- rf_data[train_index, ]
test_rf  <- rf_data[-train_index, ]

# ---------------------------
# Train Random Forest (Tuned)
# ---------------------------
set.seed(42)
rf_model <- randomForest(
  reordered ~ ., 
  data = train_rf, 
  ntree = 200,     # increased trees
  mtry = 3,        # manually tuned mtry
  importance = TRUE
)

print(rf_model)

# ---------------------------
# Predict and Evaluate
# ---------------------------
predictions <- predict(rf_model, test_rf)
probs <- predict(rf_model, test_rf, type = "prob")[, 2]

conf_matrix <- table(Predicted = predictions, Actual = test_rf$reordered)
print(conf_matrix)

accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Test Set Accuracy:", round(accuracy * 100, 2), "%\n")

roc_obj <- roc(test_rf$reordered, probs)
auc_value <- auc(roc_obj)
cat("AUC:", round(auc_value, 4), "\n")

# ---------------------------
# Optional: Manual Threshold Adjustment (E.g., 0.4)
# ---------------------------
threshold <- 0.4
manual_preds <- ifelse(probs > threshold, 1, 0)
manual_actual <- as.numeric(as.character(test_rf$reordered))

conf_matrix_manual <- table(Predicted = manual_preds, Actual = manual_actual)
print(conf_matrix_manual)

manual_acc <- sum(diag(conf_matrix_manual)) / sum(conf_matrix_manual)
cat("Adjusted Accuracy (Threshold =", threshold, "):", round(manual_acc * 100, 2), "%\n")

# ---------------------------
# Feature Importance Plot
# ---------------------------
importance_df <- as.data.frame(importance(rf_model))
importance_df$Feature <- rownames(importance_df)

ggplot(importance_df, aes(x = reorder(Feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Feature Importance: Mean Decrease in Gini",
    x = "Feature",
    y = "Importance"
  ) +
  theme_minimal()

# ---------------------------
# Class Balance Plot
# ---------------------------
ggplot(rf_data, aes(x = reordered)) +
  geom_bar(fill = "steelblue") +
  labs(
    title = "Class Distribution of Reordered Variable",
    x = "Reordered (0 = No, 1 = Yes)",
    y = "Count"
  ) +
  theme_minimal()

# ---------------------------
# Top 10 Product Frequencies
# ---------------------------
top_counts <- rf_data %>%
  count(product_name) %>%
  top_n(10, n)

ggplot(top_counts, aes(x = reorder(product_name, n), y = n)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 Most Frequently Ordered Products",
    x = "Product Name",
    y = "Order Count"
  ) +
  theme_minimal()
# ---------------------------
# Precision, Recall, F1 Score
# ---------------------------

# Convert to factors
manual_preds_factor <- as.factor(manual_preds)
manual_actual_factor <- as.factor(manual_actual)

# Create confusion matrix using caret
caret_cm <- confusionMatrix(manual_preds_factor, manual_actual_factor, positive = "1")

# Print precision, recall, and F1 score
precision <- caret_cm$byClass["Precision"]
recall <- caret_cm$byClass["Recall"]
f1 <- caret_cm$byClass["F1"]

cat("Precision:", round(precision, 4), "\n")
cat("Recall:", round(recall, 4), "\n")
cat("F1 Score:", round(f1, 4), "\n")

###############################################################################
# ---------------------------
# ASSOCIATED RULES
# ---------------------------
# Merge Data
# ---------------------------

rm(list=ls())
orders <- read.csv("orders.csv")
order_products_train <- read.csv("order_products__train.csv")
products <- read.csv("products.csv")
aisles <- read.csv("aisles.csv")
departments <- read.csv("departments.csv")

# Merge products with aisle and department names
products_full <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id")

# Merge product details into order-product data
merged <- order_products_train %>%
  left_join(products_full, by = "product_id")

# Merge in order-level info (order_id, user_id, etc.)
merged <- merged %>%
  left_join(orders, by = "order_id")

# ---------------------------
# Covert to categorical variables
# ---------------------------
merged$Newreordered[merged$reordered>0]<-"Yes"
merged$Newreordered[merged$reordered==0]<-"No"

merged$DaysSinceOrder[merged$days_since_prior_order<=5.0]<-"Less5"
merged$DaysSinceOrder[merged$days_since_prior_order<=10.0&merged$days_since_prior_order>=5.0]<-"5to9"
merged$DaysSinceOrder[merged$days_since_prior_order<=15.0&merged$days_since_prior_order>=10.0]<-"10to14"
merged$DaysSinceOrder[merged$days_since_prior_order<=20.0&merged$days_since_prior_order>=15.0]<-"15to19"
merged$DaysSinceOrder[merged$days_since_prior_order<=25.0&merged$days_since_prior_order>=20.0]<-"20to24"
merged$DaysSinceOrder[merged$days_since_prior_order<=30.0&merged$days_since_prior_order>=25.0]<-"25to30"

merged$DayOfWeek[merged$order_dow==0]<-"Sunday"
merged$DayOfWeek[merged$order_dow==1]<-"Monday"
merged$DayOfWeek[merged$order_dow==2]<-"Tuesday"
merged$DayOfWeek[merged$order_dow==3]<-"Wednesday"
merged$DayOfWeek[merged$order_dow==4]<-"Thursday"
merged$DayOfWeek[merged$order_dow==5]<-"Friday"
merged$DayOfWeek[merged$order_dow==6]<-"Saturday"

merged$WeekendWeekday[merged$order_dow==0]<-"Weekend"
merged$WeekendWeekday[merged$order_dow==6]<-"Weekend"
merged$WeekendWeekday[merged$order_dow<=5&merged$order_dow>=1]<-"Weekday"

merged$HourOfDay[merged$order_hour_of_day<=5.0&merged$order_hour_of_day>=0.0]<-"Midnightto5am"
merged$HourOfDay[merged$order_hour_of_day<=11.0&merged$order_hour_of_day>=6.0]<-"6amto11am"
merged$HourOfDay[merged$order_hour_of_day<=17.0&merged$order_hour_of_day>=12.0]<-"Noonto5pm"
merged$HourOfDay[merged$order_hour_of_day<=23.0&merged$order_hour_of_day>=18.0]<-"6pmto11pm"

merged$NumberOfUserOrders[merged$order_number<=25.0&merged$order_number>=0.0]<-"0to25"
merged$NumberOfUserOrders[merged$order_number<=50.0&merged$order_number>=26.0]<-"26to50"
merged$NumberOfUserOrders[merged$order_number<=75.0&merged$order_number>=51.0]<-"51to75"
merged$NumberOfUserOrders[merged$order_number<=100&merged$order_number>=76.0]<-"76to100"

str(merged)
View(merged)

# ---------------------------
# Covert to factor and omit unnecessary variables
# ---------------------------
merged$Newreordered<-as.factor(merged$Newreordered)
merged$department<-as.factor(merged$department)
merged$DaysSinceOrder<-as.factor(merged$DaysSinceOrder)
merged$DayOfWeek<-as.factor(merged$DayOfWeek)
merged$HourOfDay<-as.factor(merged$HourOfDay)
merged$WeekendWeekday<-as.factor(merged$WeekendWeekday)
merged$NumberOfUserOrders<-as.factor(merged$NumberOfUserOrders)
merged$aisle <- as.factor(merged$aisle)
merged$product_name <- as.factor(merged$product_name)
merged$order_id <- as.factor(merged$order_id)
merged_final<-merged[,c(1,5,8:9,16:21)]
str(merged_final)
View(merged_final)

#Convert data frame to list of transactions
orders_trans <- as(merged_final, "transactions")

# ---------------------------
# Reordered=Yes
# ---------------------------
orderrules<-apriori(orders_trans, parameter=list(supp=0.05, conf=0.59, minlen=2, maxlen=5))
Reordered_Yes<-subset(orderrules, subset = rhs %in% "Newreordered=Yes" & lift > 1)
summary(Reordered_Yes)

Reordered_Yes_df<-as(Reordered_Yes, "data.frame")
Reordered_Yes_df<-Reordered_Yes_df[order(Reordered_Yes_df$lift, Reordered_Yes_df$support, decreasing=TRUE), ]
View(Reordered_Yes_df)

# ---------------------------
# Reordered=No
# ---------------------------
orderrules<-apriori(orders_trans, parameter=list(supp=0.01, conf=0.55, minlen=2, maxlen=5))
Reordered_No<-subset(orderrules, subset = rhs %in% "Newreordered=No" & lift > 1.38)
summary(Reordered_No)

Reordered_No_df<-as(Reordered_No, "data.frame")
Reordered_No_df<-Reordered_No_df[order(Reordered_No_df$lift, Reordered_No_df$support, decreasing=TRUE), ]
View(Reordered_No_df)

#############################################################################
# ---------------------------
# LOGISTIC REGRESSION
# ---------------------------
# Merge Data
# ---------------------------

orders <- read.csv("orders.csv")
order_products_train <- read.csv("order_products__train.csv")
products <- read.csv("products.csv")
aisles <- read.csv("aisles.csv")
departments <- read.csv("departments.csv")

products_full <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id")

merged <- order_products_train %>%
  left_join(products_full, by = "product_id") %>%
  left_join(orders, by = "order_id")

# Convert necessary columns to factors
merged <- merged %>%
  mutate(
    reordered = as.factor(reordered),
    department = as.factor(department),
    aisle = as.factor(aisle),
    order_dow = as.factor(order_dow),
    order_hour_of_day = as.factor(order_hour_of_day)
  )

# ---------------------------
# Split into Test, Training, Validation
# ---------------------------
set.seed(123) # ensures reproducibility
merged_small <- merged %>% sample_n(16000)

n <- nrow(merged_small)
indices <- sample(1:n)

train_size <- floor(0.6 * n)
valid_size <- floor(0.2 * n)
test_size  <- n - train_size - valid_size

train_idx <- indices[1:train_size]
valid_idx <- indices[(train_size + 1):(train_size + valid_size)]
test_idx  <- indices[(train_size + valid_size + 1):n]

train_orders <- merged_small[train_idx, ]
valid_orders <- merged_small[valid_idx, ]
test_orders  <- merged_small[test_idx, ]

# ---------------------------
# Model
# ---------------------------

model <- glm(reordered ~ department + aisle + order_dow + order_hour_of_day + days_since_prior_order,
             data = train_orders, family = "binomial")
summary(model)

step_model <- step(model, direction = "both")
summary(step_model)


# Predict probabilities on validation set
pred_probs <- predict(step_model, newdata = valid_orders, type = "response")
# Align factor levels in valid_orders with train_orders
valid_orders$aisle <- factor(valid_orders$aisle, levels = levels(train_orders$aisle))
valid_orders$order_hour_of_day <- factor(valid_orders$order_hour_of_day, levels = levels(train_orders$order_hour_of_day))
# Predict probabilities
pred_probs <- predict(step_model, newdata = valid_orders, type = "response")
# Align factor levels
valid_orders$aisle <- factor(valid_orders$aisle, levels = levels(train_orders$aisle))
valid_orders$order_hour_of_day <- factor(valid_orders$order_hour_of_day, levels = levels(train_orders$order_hour_of_day))
# Drop rows with NA (these had unseen levels in factor)
valid_orders_clean <- valid_orders[complete.cases(valid_orders), ]
# Predict
pred_probs <- predict(step_model, newdata = valid_orders_clean, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)
# Filter out any rows in valid_orders that have aisles not present in training data
valid_orders_clean <- valid_orders %>%
  filter(aisle %in% levels(train_orders$aisle)) %>%
  mutate(
    aisle = factor(aisle, levels = levels(train_orders$aisle)),
    order_hour_of_day = factor(order_hour_of_day, levels = levels(train_orders$order_hour_of_day))
  )
pred_probs <- predict(step_model, newdata = valid_orders_clean, type = "response")
# Step 1: Keep only known aisle levels from training
valid_orders_clean <- valid_orders[valid_orders$aisle %in% levels(train_orders$aisle), ]
# Step 2: Reset factor levels to match training (and drop extras)
valid_orders_clean$aisle <- factor(valid_orders_clean$aisle, levels = levels(train_orders$aisle))
valid_orders_clean$order_hour_of_day <- factor(valid_orders_clean$order_hour_of_day, levels = levels(train_orders$order_hour_of_day))
# Step 3: Drop unused factor levels across entire dataset
valid_orders_clean <- droplevels(valid_orders_clean)
pred_probs <- predict(step_model, newdata = valid_orders_clean, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

# Step 1: Filter to only aisles that exist in training set
valid_orders_clean <- valid_orders %>%
  filter(aisle %in% unique(train_orders$aisle))
# Step 2: Drop unused levels
valid_orders_clean <- droplevels(valid_orders_clean)
# Step 3: Explicitly reset levels to match training
valid_orders_clean$aisle <- factor(valid_orders_clean$aisle, levels = levels(train_orders$aisle))
valid_orders_clean$order_hour_of_day <- factor(valid_orders_clean$order_hour_of_day, levels = levels(train_orders$order_hour_of_day))
pred_probs <- predict(step_model, newdata = valid_orders_clean, type = "response")
pred_classes <- ifelse(pred_probs > 0.5, 1, 0)

confusionMatrix(as.factor(pred_classes), valid_orders_clean$reordered)

# Filter to known aisles
test_orders_clean <- test_orders %>%
  filter(aisle %in% unique(train_orders$aisle))
# Drop unused levels and match factor levels
test_orders_clean <- droplevels(test_orders_clean)
test_orders_clean$aisle <- factor(test_orders_clean$aisle, levels = levels(train_orders$aisle))
test_orders_clean$order_hour_of_day <- factor(test_orders_clean$order_hour_of_day, levels = levels(train_orders$order_hour_of_day))
# Predict probabilities
test_probs <- predict(step_model, newdata = test_orders_clean, type = "response")
# Use a custom threshold of 0.6
test_orders_clean$predicted_reorder <- ifelse(test_probs > 0.6, 1, 0)
# View only predicted reorders
reordered_products <- test_orders_clean %>%
  filter(predicted_reorder == 1) %>%
  select(order_id, product_id, product_name, aisle, department)
# Optional: Export to CSV
write.csv(reordered_products, "predicted_reorders.csv", row.names = FALSE)
# Get tidy coefficients
coef_df <- as.data.frame(coef(summary(step_model)))
coef_df$variable <- rownames(coef_df)
# Remove intercept
coef_df <- coef_df[coef_df$variable != "(Intercept)", ]
# Remove NA estimates (just in case)
coef_df <- coef_df[!is.na(coef_df$Estimate), ]
# Top 10 predictors increasing reorder probability
top_positive <- coef_df[order(-coef_df$Estimate), ][1:10, ]
# Top 10 predictors decreasing reorder probability
top_negative <- coef_df[order(coef_df$Estimate), ][1:10, ]

# Combine top positive and negative
top_combined <- rbind(
  transform(top_positive, direction = "Increase"),
  transform(top_negative, direction = "Decrease")
)
# Plot
ggplot(top_combined, aes(x = reorder(variable, Estimate), y = Estimate, fill = direction)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top Predictors of Reorder (Logistic Regression)",
       x = "Variable", y = "Coefficient Estimate") +
  theme_minimal()
# Step 1: Predict reorder probabilities using your final model
pred_probs <- predict(step_model, newdata = test_orders_clean, type = "response")
# Step 2: Apply a threshold to classify predictions
test_orders_clean$predicted_reorder <- ifelse(pred_probs > 0.6, 1, 0)
# Step 3: Filter only the predicted reorders (predicted_reorder == 1)
predicted_reorders <- test_orders_clean[test_orders_clean$predicted_reorder == 1, ]
# Step 4: Export final predicted reorder products
write.csv(predicted_reorders, "predicted_reorders.csv", row.names = FALSE)
# Optional: View a preview
head(predicted_reorders)
# Step 1: Join predictions with product details
predicted_reorders_final <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Step 2: Optional – select and reorder relevant columns for clean export
predicted_reorders_final <- predicted_reorders_final %>%
  select(order_id, product_id, product_name, aisle, department, predicted_reorder)
# Step 0: Confirm product details are in products_full
# This should include product_name, aisle, department, and product_id
glimpse(products_full)
# Step 1: Merge predictions with product info (ensure correct key)
predicted_reorders_final <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Step 2: Check if product_name exists now
names(predicted_reorders_final)
# Step 3: Select relevant columns only if they exist
# Use intersect to safely avoid select() errors
wanted_cols <- intersect(
  c("order_id", "product_id", "product_name", "aisle", "department", "predicted_reorder"),
  colnames(predicted_reorders_final)
)
predicted_reorders_final <- predicted_reorders_final %>%
  select(all_of(wanted_cols))
# Step 4: Export to CSV
write.csv(predicted_reorders_final, "predicted_reorders_with_product_names.csv", row.names = FALSE)
# Step 1: Rebuild products_full to include all needed columns
products_full <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id")
# Step 2: Merge predictions with full product info
predicted_reorders_final <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Step 3: Safely select only the relevant columns that exist
wanted_cols <- intersect(
  c("order_id", "product_id", "product_name", "aisle", "department", "predicted_reorder"),
  colnames(predicted_reorders_final)
)
predicted_reorders_final <- predicted_reorders_final %>%
  select(all_of(wanted_cols))
# Step 4: Export final merged file
write.csv(predicted_reorders_final, "predicted_reorders_with_product_names.csv", row.names = FALSE)
# Optional preview
head(predicted_reorders_final)
# Are both product_id columns the same class?
class(predicted_reorders$product_id)
class(products$product_id)
# Should return the same — if not, convert:
products$product_id <- as.integer(products$product_id)
# Confirm no duplicate product_ids in products
sum(duplicated(products$product_id))  # Should return 0
# Rebuild clean products_full
products_full <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id")
# Ensure all product_id types match
predicted_reorders$product_id <- as.integer(predicted_reorders$product_id)
products_full$product_id <- as.integer(products_full$product_id)
# Re-merge with fixed types
predicted_reorders_final <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Check merge worked
head(predicted_reorders_final)
# Final selection and export
wanted_cols <- intersect(
  c("order_id", "product_id", "product_name", "aisle", "department", "predicted_reorder"),
  colnames(predicted_reorders_final)
)
predicted_reorders_final <- predicted_reorders_final %>%
  select(all_of(wanted_cols))
write.csv(predicted_reorders_final, "predicted_reorders_with_product_names.csv", row.names = FALSE)
# Ensure matching types
predicted_reorders$product_id <- as.integer(predicted_reorders$product_id)
products$product_id <- as.integer(products$product_id)
aisles$aisle_id <- as.integer(aisles$aisle_id)
departments$department_id <- as.integer(departments$department_id)
# Rebuild complete product metadata
products_full <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id")
# Merge predictions with product info
predicted_reorders_final <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Select clean columns (handle missing safely)
wanted_cols <- intersect(
  c("order_id", "product_id", "product_name", "aisle", "department", "predicted_reorder"),
  colnames(predicted_reorders_final)
)
predicted_reorders_final <- predicted_reorders_final %>%
  select(all_of(wanted_cols))
# Export
write.csv(predicted_reorders_final, "predicted_reorders_with_product_names.csv", row.names = FALSE)
# Preview
head(predicted_reorders_final)
class(predicted_reorders$product_id)
class(products$product_id)
glimpse(products_full)
# Ensure matching data types
predicted_reorders$product_id <- as.integer(predicted_reorders$product_id)
products_full$product_id <- as.integer(products_full$product_id)
# Join predictions with full product info
joined <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Check how many product_name values are NA (should be 0 or very few)
sum(is.na(joined$product_name))
# Select clean columns
predicted_reorders_final <- joined %>%
  select(order_id, product_id, product_name, aisle, department, predicted_reorder)
# Step 1: Check data types
print("Class of predicted_reorders$product_id:")
print(class(predicted_reorders$product_id))
print("Class of products_full$product_id:")
print(class(products_full$product_id))
# Step 2: Convert both to integer if they’re not
predicted_reorders$product_id <- as.integer(predicted_reorders$product_id)
products_full$product_id <- as.integer(products_full$product_id)
# Step 3: Do the join
joined <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Step 4: Confirm if product_name exists
print("Column names after join:")
print(colnames(joined))
# Check how many rows have missing product_name
na_count <- sum(is.na(joined$product_name))
print(paste("Rows with missing product_name:", na_count))
# Step 5: If product_name exists, safely select available columns
wanted_cols <- intersect(
  c("order_id", "product_id", "product_name", "aisle", "department", "predicted_reorder"),
  colnames(joined)
)
# Step 6: Select and export
predicted_reorders_final <- joined %>%
  select(all_of(wanted_cols))
write.csv(predicted_reorders_final, "predicted_reorders_with_product_names.csv", row.names = FALSE)
# Final preview
head(predicted_reorders_final)

# Step 1: Force all IDs to integer
predicted_reorders$product_id <- as.integer(predicted_reorders$product_id)
products$product_id <- as.integer(products$product_id)
aisles$aisle_id <- as.integer(aisles$aisle_id)
departments$department_id <- as.integer(departments$department_id)
# Step 2: Rebuild full product metadata
products_full <- products %>%
  left_join(aisles, by = "aisle_id") %>%
  left_join(departments, by = "department_id")
# Step 3: Join prediction results with product info
predicted_reorders_final <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Step 4: Check column presence
print("Column names:")
print(colnames(predicted_reorders_final))
# Step 5: Select and export only valid columns
wanted_cols <- intersect(
  c("order_id", "product_id", "product_name", "aisle", "department", "predicted_reorder"),
  colnames(predicted_reorders_final)
)
predicted_reorders_final <- predicted_reorders_final %>%
  select(all_of(wanted_cols))
# Step 6: Export to CSV
write.csv(predicted_reorders_final, "predicted_reorders_with_product_names.csv", row.names = FALSE)
# Optional: Preview
head(predicted_reorders_final)
# Check if the merge actually worked
joined <- predicted_reorders %>%
  left_join(products_full, by = "product_id")
# Check number of matches
print("Total rows in predicted_reorders:")
print(nrow(predicted_reorders))
# Now check how many rows came through after the join
print("Total rows in joined result:")
print(nrow(joined))
# Check if product_name exists now
print("Available columns after join:")
print(colnames(joined))
# Rename the .y columns to clean names
predicted_reorders_final <- joined %>%
  mutate(
    product_name = product_name.y,
    aisle = aisle.y,
    department = department.y
  ) %>%
  select(order_id, product_id, product_name, aisle, department, predicted_reorder)
# Export to CSV
write.csv(predicted_reorders_final, "predicted_reorders_with_product_names.csv", row.names = FALSE)
# Optional: Preview
head(predicted_reorders_final)
