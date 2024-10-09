# Load necessary libraries
library(ggplot2)
library(dplyr)

# Create your dataset
data <- read.table("elemene.txt",header=T)

# Calculate summary statistics
summary_stats <- data %>%
  group_by(class) %>%
  summarise(mean = mean(score), sd = sd(score))

# Perform pairwise t-tests between classes
pairwise_test <- pairwise.t.test(data$score, data$class, p.adjust.method = "bonferroni")
pairwise_test
print(pairwise_test)

# Add significance labels based on t-test p-values
significance_levels <- function(p) {
  if (p < 0.001) return("***")
  if (p < 0.01) return("**")
  if (p < 0.05) return("*")
  return("ns")
}
pvals <- pairwise_test$p.value
labels <- c(
  paste0("S1 vs S2: ", significance_levels(pvals[1, 1])),
  paste0("S1 vs S3: ", significance_levels(pvals[2, 1])),
  paste0("S2 vs S3: ", significance_levels(pvals[2, 2]))
)

# Merge summary stats with original data for plot
merged_data <- merge(data, summary_stats, by = "class")

# Plot
plot <- ggplot(merged_data, aes(x = class, y = score)) +
  geom_point(aes(color = class), size = 3, alpha = 0.7, position = position_jitter(width = 0.15)) +  # Show all data points
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", alpha = 0.6, color = "black", width = 0.5) +  # Mean bar
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.2, color = "black") +  # Error bars (SD)
  labs(title = "Class Scores with Error Bars", x = "Class", y = "Score") +
  annotate("text", x = 2, y = 20, label = labels[1], size = 5) +  # Add p-value label
  annotate("text", x = 2, y = 18, label = labels[2], size = 5) +  # Add p-value label
  annotate("text", x = 2, y = 16, label = labels[3], size = 5) +  # Add p-value label
  theme_minimal()

print(plot)


# Print p-values for reference
pairwise_test$p.value
pairwise_test$p.adjust.method


