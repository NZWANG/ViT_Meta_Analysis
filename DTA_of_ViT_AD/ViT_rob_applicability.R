## The code plot the risk of bias for applicability and concerns##
# Load required libraries
library(ggplot2)

# Sample data// See Table S1: Supplementary Table for Risk of Bias Assessment ##
data <- data.frame(
  Domain = c("Patient selection", "Index test", "Reference standard"),
  Low_risk = c(5, 6, 10),
  Unclear_risk = c(7, 5, 4),
  High_risk = c(5, 5, 2)
)

# Calculate percentages for each risk level within each domain
data$Total <- rowSums(data[, -1])
data[, -1] <- data[, -1] / data$Total

# Reshape data for plotting
data_long <- tidyr::gather(data, Key, Value, -Domain, -Total)
# Plot
ggplot(data_long, aes(x = Value, y = Domain, fill = Key)) +
  geom_bar(stat = "identity", position = "fill", width = 0.6) + # Adjust the width as desired
  labs(title = "QUADAS-2 Summary of Risk of Bias for Applicability Concerns",
       x = "Percentage of Studies",
       y = "Domain") +
  scale_fill_manual(values = c("Low_risk" = "green", "Unclear_risk" = "yellow", "High_risk" = "red"),
                    labels = c("Low risk", "Unclear risk", "High risk")) +
  theme_minimal() +
  theme(axis.text.x = element_text(face = "bold"),
        axis.text.y = element_text(face = "bold"),
        legend.text = element_text(face = "bold"),
        panel.grid.major = element_blank(),  # Remove major grid lines
        panel.grid.minor = element_blank())  # Remove minor grid lines


