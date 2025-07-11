library(ggplot2)
library(dplyr)
library(readr)
library(psych)
library(GGally)
library(e1071)
library(corrplot)
library(psych)

df <- read.csv(file.choose(),header = T)
table(df$Shift)

#Qualitative Data Analysis

shift_data <- df %>%
  count(Shift) %>%
  mutate(Percent = round(100 * n / sum(n), 1),
         Label = paste0(Shift, " - ", Percent, "%"))

# Create pie chart
ggplot(shift_data, aes(x = "", y = n, fill = Shift)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 5) +
  labs(title = "Shift Distribution (Pie Chart)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

table(df$Product)


# Create bar chart of Product counts
product_counts <- df %>%
  count(Product) %>%
  arrange(desc(n))  # Optional: reorder bars by frequency

# Create enhanced bar chart
ggplot(product_counts, aes(x = reorder(Product, -n), y = n, fill = Product)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.3, size = 2, fontface = "bold") +
  labs(
    title = "Product-Wise Frequency of Observations",
    x = "Product Type",
    y = "Number of Observations"
  ) +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray70")
  )

table(df$Operator)

operator_counts <- df %>%
  count(Operator) %>%
  arrange(desc(n))  # Optional: order bars by count

# Plot enhanced bar chart
ggplot(operator_counts, aes(x = reorder(Operator, -n), y = n, fill = Operator)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 2, fontface = "bold") +
  labs(
    title = "Operator-Wise Frequency of Observations",
    x = "Operator",
    y = "Number of Observations"
  ) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray70")
  )

table(df$Machine_ID)

machine_counts <- df %>%
  count(Machine_ID) %>%
  arrange(desc(n))  # Optional: sort by frequency

# Create bar chart
ggplot(machine_counts, aes(x = reorder(Machine_ID, -n), y = n, fill = Machine_ID)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.4, size = 3, fontface = "bold") +
  labs(
    title = "Frequency of Observations by Machine",
    x = "Machine ID",
    y = "Number of Observations"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major.y = element_line(linetype = "dashed", color = "gray80")
  )

#  Quantitative Data Analysis


summary(df$Weight_g)
mean(df$Weight_g)
median(df$Weight_g)
# Function to calculate statistical mode
get_mode <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

# Mode of Weight_g
mode_weight <- get_mode(df$Weight_g)
cat("Mode of Weight_g:", mode_weight, "\n")
min(df$Weight_g)
max(df$Weight_g)
range = max(df$Weight_g) - min(df$Weight_g)
range
var(df$Weight_g)
sd(df$Weight_g)
quantile(df$Weight_g)
IQR(df$Weight_g)

# --- Skewness and Kurtosis ---
skew_value <- skewness(df$Weight_g, na.rm = TRUE)
kurt_value <- kurtosis(df$Weight_g, na.rm = TRUE)

cat("Skewness:", round(skew_value, 2), "\n")
cat("Kurtosis:", round(kurt_value, 2), "\n")

# Histogram + density curve for Weight_g
ggplot(df, aes(x = Weight_g)) +
  geom_histogram(aes(y = ..density..), binwidth = 2, fill = "skyblue", color = "white", alpha = 0.7) +
  geom_density(color = "darkblue", size = 1.2) +
  geom_vline(aes(xintercept = mean(Weight_g)), color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Bottle Weights with Density Curve",
    x = "Weight (g)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black")
  )


# --- Skewness and Kurtosis ---
skew_value <- skewness(df$Weight_g, na.rm = TRUE)
kurt_value <- kurtosis(df$Weight_g, na.rm = TRUE)

cat("Skewness:", round(skew_value, 2), "\n")
cat("Kurtosis:", round(kurt_value, 2), "\n")

ggplot(df, aes(y = Weight_g)) +
  geom_boxplot(
    fill = "#69b3a2",
    color = "darkblue",
    outlier.color = "red",
    outlier.shape = 17,
    outlier.size = 3,
    width = 0.3,
    notch = TRUE
  ) +
  labs(
    title = "Boxplot of Bottle Weight (g)",
    y = "Weight (grams)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )


Q1 <- quantile(df$Weight_g, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Weight_g, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Identify outliers
outliers <- df[df$Weight_g < lower_bound | df$Weight_g > upper_bound, ]
non_outliers <- df[df$Weight_g >= lower_bound & df$Weight_g <= upper_bound, ]

# Display summary
cat("Number of Outliers:", nrow(outliers), "\n")
cat("Number of Clean Records:", nrow(non_outliers), "\n")

# IQR-based filtering for Weight_g
Q1 <- quantile(df$Weight_g, 0.25)
Q3 <- quantile(df$Weight_g, 0.75)
IQR <- Q3 - Q1

# Define bounds
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Remove outliers
df_clean <- df[df$Weight_g >= lower_bound & df$Weight_g <= upper_bound, ]


mean(df$Temperature_C)
median(df$Temperature_C)
# Function to calculate statistical mode
get_mode <- function(x) {
  uniq_vals <- unique(x)
  uniq_vals[which.max(tabulate(match(x, uniq_vals)))]
}

# Mode of Weight_g
mode_weight <- get_mode(df$Temperature_C)
cat("Mode of Temperature:", mode_weight, "\n")
min(df$Temperature_C)
max(df$Temperature_C)
range = max(df$Temperature_C) - min(df$Temperature_C)
range
var(df$Temperature_C)
sd(df$Temperature_C)
quantile(df$Temperature_C)
IQR(df$Temperature_C)

# --- Skewness and Kurtosis ---
skew_value <- skewness(df$Temperature_C, na.rm = TRUE)
kurt_value <- kurtosis(df$Temperature_C, na.rm = TRUE)

cat("Skewness:", round(skew_value, 2), "\n")
cat("Kurtosis:", round(kurt_value, 2), "\n")

ggplot(df, aes(x = Temperature_C)) +
  geom_histogram(aes(y = ..density..), binwidth = 1, fill = "lightcoral", color = "white", alpha = 0.7) +
  geom_density(color = "darkred", size = 1.2) +
  geom_vline(aes(xintercept = mean(Temperature_C, na.rm = TRUE)), color = "blue", linetype = "dashed", size = 1) +
  labs(
    title = "Distribution of Production Temperature with Density Curve",
    x = "Temperature (°C)",
    y = "Density"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    axis.text = element_text(color = "black")
  )

ggplot(df, aes(y = Temperature_C)) +
  geom_boxplot(
    fill = "#FFA07A",
    color = "darkred",
    outlier.color = "blue",
    outlier.shape = 18,
    outlier.size = 3,
    width = 0.3,
    notch = TRUE
  ) +
  labs(
    title = "Boxplot of Production Temperature",
    y = "Temperature (°C)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.major.x = element_blank()
  )

# Calculate IQR and bounds
Q1 <- quantile(df$Temperature_C, 0.25, na.rm = TRUE)
Q3 <- quantile(df$Temperature_C, 0.75, na.rm = TRUE)
IQR <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

# Count outliers
num_outliers <- sum(df$Temperature_C < lower_bound | df$Temperature_C > upper_bound, na.rm = TRUE)

# Print result
cat("Number of outliers in Temperature_C:", num_outliers, "\n")


ggplot(df, aes(x = Product, y = Weight_g, fill = Product)) +
  geom_boxplot() +
  labs(title = "Weight Variation by Product", x = "Product", y = "Weight (g)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(df, aes(x = Shift, y = Weight_g, fill = Shift)) +
  geom_boxplot() +
  labs(title = "Weight Variation by Shift", x = "Shift", y = "Weight (g)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(df, aes(x = Machine_ID, y = Weight_g, fill = Machine_ID)) +
  geom_boxplot() +
  labs(title = "Weight Variation by Machine", x = "Machine ID", y = "Weight (g)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

ggplot(df, aes(x = Operator, y = Weight_g, fill = Operator)) +
  geom_boxplot() +
  labs(title = "Weight Variation by Operator", x = "Operator", y = "Weight (g)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

anova_product <- aov(Weight_g ~ Product, data = df)
summary(anova_product)

anova_shift <- aov(Weight_g ~ Shift, data = df)
summary(anova_shift)

anova_machine <- aov(Weight_g ~ Machine_ID, data = df)
summary(anova_machine)

anova_operator <- aov(Weight_g ~ Operator, data = df)
summary(anova_operator)


# Select only numeric columns
numeric_df <- df[, sapply(df, is.numeric)]

# Compute correlation matrix
cor_matrix <- cor(numeric_df, use = "complete.obs")

# Test for statistical significance
cor.test(df$Temperature_C, df$Weight_g)
# Basic correlation plot
corrplot(cor_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8,
         title = "Correlation Matrix of Numeric Variables",
         mar = c(0, 0, 2, 0))


ggplot(df, aes(x = Temperature_C, y = Weight_g)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "darkred", se = TRUE, linetype = "dashed") +
  labs(
    title = "Relationship Between Temperature and Bottle Weight",
    x = "Temperature (°C)",
    y = "Weight (g)"
  ) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Control chart for individual bottle weights
qcc(df$Weight_g, type = "xbar.one",
    title = "Individuals Control Chart for Bottle Weight",
    xlab = "Observation Number",
    ylab = "Weight (g)")

model <- lm(Weight_g ~ Temperature_C, data = df)
summary(model)


# Check the column name – assuming it's 'Defects'
table(df$Defect)

ggplot(df, aes(x = Defect)) +
  geom_bar(fill = "tomato", color = "black") +
  labs(title = "Frequency of Defect Types",
       x = "Defect Type",
       y = "Count") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

