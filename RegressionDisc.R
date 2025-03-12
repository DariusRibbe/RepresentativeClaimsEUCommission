
if (!require("rdrobust")) install.packages("rdrobust")
if (!require("broom")) install.packages("broom")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("dplyr")) install.packages("dplyr")

library(rdrobust)
library(broom)
library(ggplot2)
library(dplyr)

# Convert date variable to Date format (assuming "date" column exists in dataset)
final_data$date <- as.Date(final_data$date, format="%Y-%m-%d")

# Define event date (cutoff) for RDD
event_date <- as.Date("1996-02-01")

# Create a running variable (time to event in **DAYS**)
final_data$time_to_event <- as.numeric(difftime(final_data$date, event_date, units = "days"))

# Create an indicator for being after the event
final_data$post_event <- ifelse(final_data$time_to_event >= 0, 1, 0)

# Restrict dataset to ±8 years (±2920 days) around event
final_data <- final_data %>% filter(time_to_event >= -2920 & time_to_event <= 2920)


logit_rdd <- glm(claimbinary ~ time_to_event + post_event + time_to_event:post_event +
                   Sex_speakerinfo + post_event:Sex_speakerinfo, 
                 data = final_data, 
                 family = binomial)

summary(logit_rdd)

# Select bandwidth using an improved method for discrete data
bw <- rdbwselect(y = final_data$claimbinary, x = final_data$time_to_event, c = 0, bwselect = "msetwo")

# Run RDD using local polynomial regression
rdd_results <- rdrobust(y = final_data$claimbinary, x = final_data$time_to_event, c = 0, p = 1, h = 2920, bwselect = "msetwo")

# Print results
summary(rdd_results)


# Bin the data into 180-day intervals (~6 months) for plotting
rdd_plot_data <- final_data %>%
  mutate(bin = cut(time_to_event, breaks = seq(min(time_to_event), max(time_to_event), by = 180))) %>%
  group_by(bin, post_event, Sex_speakerinfo) %>%
  summarize(avg_claim = mean(claimbinary, na.rm = TRUE),
            se = sd(claimbinary, na.rm = TRUE) / sqrt(n()))

# Convert bin to numeric for plotting
rdd_plot_data$bin <- as.numeric(gsub("[^0-9.-]", "", rdd_plot_data$bin))

# Plot RDD results by Sex
ggplot(rdd_plot_data, aes(x = bin, y = avg_claim, color = as.factor(post_event), shape = as.factor(Sex_speakerinfo))) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("0" = "#003399", "1" = "#FFCC00"),
                     labels = c("Before Feb 1996", "After Feb 1996")) +
  scale_shape_manual(values = c(16, 17), labels = c("Men", "Women")) +
  labs(title = "Regression Discontinuity: Claim Probability Before & After Feb 1996 by Speaker Sex",
       x = "Time to Event (Days)",
       y = "Average Probability of Making a Claim",
       color = "Period",
       shape = "Speaker Sex") +
  theme_minimal() +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black", size = 1) +
  annotate("text", x = -2000, y = max(rdd_plot_data$avg_claim), label = "Pre-Feb 1996", hjust = 1, size = 5, color = "#003399") +
  annotate("text", x = 2000, y = max(rdd_plot_data$avg_claim), label = "Post-Feb 1996", hjust = 0, size = 5, color = "#FFCC00")

# Save the plot
ggsave("Regression_Discontinuity_ClaimProbability_BySex_Days.svg", width = 8, height = 6, dpi = 300, device = "svg")

# Create a summary table of the RD regression model
rd_table <- modelsummary(list("Logistic RDD" = logit_rdd),
                         stars = TRUE,
                         title = "Regression Discontinuity Estimates (Logistic Model)",
                         coef_map = c("time_to_event" = "Time to Event (Days)",
                                      "post_event" = "Post-Feb 1996",
                                      "time_to_event:post_event" = "Interaction: Time x Post Event",
                                      "Sex_speakerinfo" = "Speaker Sex (Women)",
                                      "post_event:Sex_speakerinfo" = "Interaction: Post Event x Sex"),
                         gof_map = c("nobs" = "Number of Observations",
                                     "aic" = "AIC",
                                     "bic" = "BIC"),
                         output = "html")  # Change to "latex" for LaTeX format

# Save table as an HTML file
writeLines(rd_table, "RD_Model_Summary.html")

# Print the table in the R console
rd_table

ggplot(safe, aes(x = length)) +
  geom_histogram(binwidth = 10, fill = "#003399", color = "white", alpha = 0.8) +  # Dark blue bars
  labs(title = "Distribution of Length",
       x = "Length",
       y = "Frequency") +
  theme_minimal()

ggplot(dffinal, aes(y = length)) +
  geom_boxplot(fill = "#FFCC00", color = "#003399", outlier.color = "#003399", outlier.shape = 16, outlier.size = 2) + 
  labs(title = "Distribution of Length",
       y = "Length") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),  # Remove x-axis label
        axis.text.x = element_blank(),   # Remove x-axis values
        axis.ticks.x = element_blank()) +  # Remove x-axis ticks
  coord_flip()  # Makes the boxplot horizontal for better readability

summary(safe$length)
