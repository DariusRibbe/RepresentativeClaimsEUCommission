### Analysis Paper 3 - Influence of Portfolios and Sex on Claim-Making ###
### Darius Ribbe ###

if (!require("glmmTMB")) install.packages("glmmTMB", dependencies=TRUE)
if (!require("lme4")) install.packages("lme4")
if (!require("broom.mixed")) install.packages("broom.mixed")
if (!require("dplyr")) install.packages("dplyr")
if (!require("ggeffects")) install.packages("ggeffects")
if (!require("ggplot2")) install.packages("ggplot2")
if (!require("sjPlot")) install.packages("sjPlot")
if (!require("flextable")) install.packages("flextable")
if (!require("officer")) install.packages("officer")
if (!require("performance")) install.packages("performance")
if (!require("modelsummary"))install.packages("modelsummary")
library(modelsummary)
library(sjPlot)
library(flextable)
library(officer)
library(ggeffects)
library(ggplot2)
library(glmmTMB)
library(lme4)
library(broom.mixed)
library(dplyr)
library(performance)
library(kableExtra)
final_data <- read.csv("C:/Users/User/Downloads/final_data.csv")

final_data <- final_data %>%
  mutate(
    PortfolioG_speakerinfo = as.factor(PortfolioG_speakerinfo),  # Convert portfolio to categorical
    length_scaled = scale(length),  # Standardize continuous variables
    w_top_tot_scaled = scale(w_top_tot),
    w_top_rel_scaled = scale(w_top_rel)
  )

final_data$European.Party_speakerinfo <- gsub("(.*?)\\[.*|(.*?) .*|(.*?) \\/", "\\1", final_data$European.Party_speakerinfo)
final_data$European.Party_speakerinfo <- gsub("PES /", "PES", final_data$European.Party_speakerinfo)
# Baseline Model: Portfolio Only
logit_glmm_1 <- glmer(claimbinary ~ PortfolioG_speakerinfo + 
                        (1 | Commission_speakerinfo),  
                      data = final_data, 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))

# Add Length
logit_glmm_2 <- glmer(claimbinary ~ PortfolioG_speakerinfo + length + 
                        (1 | Commission_speakerinfo),  
                      data = final_data, 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))

# Add w_top_tot
logit_glmm_3 <- glmer(claimbinary ~ PortfolioG_speakerinfo + length + w_top_tot +
                        (1 | Commission_speakerinfo),  
                      data = final_data, 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))

# Add w_top_rel
logit_glmm_4 <- glmer(claimbinary ~ PortfolioG_speakerinfo + length + European.Party_speakerinfo +
                        (1 | Commission_speakerinfo),  
                      data = final_data, 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))

table(final_data$claimbinary, final_data$European.Party_speakerinfo)


# Add Interaction: Portfolio * Sex
logit_glmm_5 <- glmer(claimbinary ~ PortfolioG_speakerinfo * Sex_speakerinfo + length + European.Party_speakerinfo +
                        (1 | Commission_speakerinfo),  
                      data = final_data, 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))

logit_glmm_6 <- glmer(claimbinary ~ PortfolioG_speakerinfo * Sex_speakerinfo + length + European.Party_speakerinfo + w_top_tot +
                        (1 | Commission_speakerinfo),  
                      data = final_data, 
                      family = binomial,
                      control = glmerControl(optimizer = "bobyqa"))

summary(logit_glmm_5)

# Model Comparison
AIC(logit_glmm_1, logit_glmm_2, logit_glmm_3, logit_glmm_4, logit_glmm_5, logit_glmm_6)



# Define the list of logistic GLMM models
logit_models <- list(
  "Model 1" = logit_glmm_1, 
  "Model 2" = logit_glmm_2, 
  "Model 3" = logit_glmm_3, 
  "Model 4" = logit_glmm_4, 
  "Model 5" = logit_glmm_5, 
  "Model 6" = logit_glmm_6
)

# Generate a logistic GLMM regression table
modelsummary(logit_models,
             stars = TRUE,
             title = "Logistic GLMM Regression Results",
             gof_omit = "IC|Log|Deviance")


plot(ggpredict(logit_glmm_6, terms = c("PortfolioG_speakerinfo", "Sex_speakerinfo")))




# Get predicted values for the interaction effect
predicted_values <- ggpredict(logit_glmm_6, terms = c("PortfolioG_speakerinfo", "Sex_speakerinfo"))

# Rename factor levels for clarity
predicted_values$x <- factor(predicted_values$x, 
                             levels = c(1, 2, 3), 
                             labels = c("Masculine", "Neutral", "Feminine"))

predicted_values$group <- factor(predicted_values$group, 
                                 levels = c(0, 1), 
                                 labels = c("Men", "Women"))

# Create the scatter plot with 95% CIs
# Install/load required packages

# Get predicted values for the interaction effect
predicted_values <- ggpredict(logit_glmm_6, terms = c("PortfolioG_speakerinfo", "Sex_speakerinfo"))

# Rename factor levels for clarity
predicted_values$x <- factor(predicted_values$x, 
                             levels = c(1, 2, 3), 
                             labels = c("Masculine", "Neutral", "Feminine"))

predicted_values$group <- factor(predicted_values$group, 
                                 levels = c(0, 1), 
                                 labels = c("Men", "Women"))

# Create the scatter plot with thicker error bars and a black frame
plot1 <- ggplot(predicted_values, aes(x = x, y = predicted, color = group)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +  # Scatter points, slightly dodged
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1.3,  # Thicker error bars
                position = position_dodge(width = 0.3), alpha = 0.8) +
  scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +  # Custom colors
  labs(
    x = "Portfolio-Group",
    y = "Pred. Prob. of Making a Claim",
    color = "Sex Commissioner",
    title = "Interaction of Portfolio and Sex on Claim-Making Probability"
  ) +
  theme_minimal() +  # Clean theme
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major = element_blank(),  # Remove all background grid lines
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Black frame around the plot
  )

save_folder <- "C:/Users/User/Documents/plots/"
ggsave(filename = paste0(save_folder, "Interaction Effect Portfolio Sex Binary.svg"), plot = plot1, width = 8, height = 6, dpi = 300, device = "svg")


# Baseline Model: Portfolio Only
zinb_glmm_1 <- glmmTMB(claimcount ~ PortfolioG_speakerinfo +  
                         (1 | Commission_speakerinfo),  
                       data = final_data, 
                       family = nbinom2,
                       ziformula = ~1,  # Simplified zero-inflation model
                       control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

# Add Length
zinb_glmm_2 <- glmmTMB(claimcount ~ PortfolioG_speakerinfo + length +  
                         (1 | Commission_speakerinfo),  
                       data = final_data, 
                       family = nbinom2,
                       ziformula = ~1,  
                       control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

# Add w_top_tot
zinb_glmm_3 <- glmmTMB(claimcount ~ PortfolioG_speakerinfo + length + w_top_tot +  
                         (1 | Commission_speakerinfo),  
                       data = final_data, 
                       family = nbinom2,
                       ziformula = ~1,  
                       control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

# Add European Party (Matches logit_glmm_4)
zinb_glmm_4 <- glmmTMB(claimcount ~ PortfolioG_speakerinfo + length + European.Party_speakerinfo +  
                         (1 | Commission_speakerinfo),  
                       data = final_data, 
                       family = nbinom2,
                       ziformula = ~1,  
                       control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

# Add Interaction: Portfolio * Sex (Matches logit_glmm_5)
zinb_glmm_5 <- glmmTMB(claimcount ~ PortfolioG_speakerinfo * Sex_speakerinfo + length + European.Party_speakerinfo +  
                         (1 | Commission_speakerinfo),  
                       data = final_data, 
                       family = nbinom2,
                       ziformula = ~1,  
                       control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

# Add w_top_tot (Matches logit_glmm_6)
zinb_glmm_6 <- glmmTMB(claimcount ~ PortfolioG_speakerinfo * Sex_speakerinfo + length + European.Party_speakerinfo + w_top_tot +  
                         (1 | Commission_speakerinfo),  
                       data = final_data, 
                       family = nbinom2,
                       ziformula = ~1,  
                       control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

# Model Summaries
summary(zinb_glmm_6)

# Model Comparison
AIC(zinb_glmm_1, zinb_glmm_2, zinb_glmm_3, zinb_glmm_4, zinb_glmm_5, zinb_glmm_6)

# Define the list of ZINB GLMM models
zinb_models <- list(
  "Model 1" = zinb_glmm_1, 
  "Model 2" = zinb_glmm_2, 
  "Model 3" = zinb_glmm_3, 
  "Model 4" = zinb_glmm_4, 
  "Model 5" = zinb_glmm_5, 
  "Model 6" = zinb_glmm_6
)

# Generate a ZINB regression table
modelsummary(zinb_models,
             stars = TRUE,
             shape = term ~ model + component,  # Separates conditional & zero-inflation coefficients
             title = "Zero-Inflated Negative Binomial GLMM Results",
             gof_omit = "IC|Log|Deviance")  


logit_glmm_time <- glmer(claimbinary ~ PortfolioG_speakerinfo * Sex_speakerinfo + length + European.Party_speakerinfo + w_top_tot + 
                           factor(Commission_speakerinfo) +  # Treat Commission as a fixed effect
                           (1 | speaker.name),  # Keep nesting at the speaker level
                         data = final_data, 
                         family = binomial,
                         control = glmerControl(optimizer = "bobyqa"))

summary(logit_glmm_time)

zinb_glmm_time <- glmmTMB(claimcount ~ PortfolioG_speakerinfo * Sex_speakerinfo + length + European.Party_speakerinfo + w_top_tot + 
                            factor(Commission_speakerinfo) +  # Fixed effect for Commission
                            (1 | speaker.name),  # Keep nesting at speaker level
                          data = final_data, 
                          family = nbinom2,
                          ziformula = ~1,  
                          control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

summary(zinb_glmm_time)



# Get predicted values over time
predicted_values <- ggpredict(logit_glmm_time, terms = c("PortfolioG_speakerinfo", "Sex_speakerinfo", "Commission_speakerinfo"))

# Rename variables for clarity
predicted_values$group <- factor(predicted_values$group, 
                                 levels = c(0, 1), 
                                 labels = c("Men", "Women"))

# Order Commission terms chronologically using the Year variable
predicted_values$x <- factor(predicted_values$x, levels = sort(unique(final_data$Year)))

# Plot the predicted effects over time
ggplot(predicted_values, aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(size = 1.5) +  # Line plot for trends
  geom_point(size = 4) +  # Points at each Commission
  scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +  # Custom colors
  labs(
    x = "Commission Period (Ordered by Year)",
    y = "Pred. Prob. of Making a Claim",
    color = "Sex Commissioner",
    title = "Change in Claim-Making Probability Across Commissions"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    panel.grid.major.x = element_blank(),  # Removes vertical grid lines
    panel.grid.minor = element_blank()
  )


predicted_values <- predict(zinb_glmm_6, type = "response", se.fit = TRUE)

# Convert to a dataframe
predicted_df <- data.frame(
  predicted = predicted_values$fit,
  se = predicted_values$se.fit
)

# Compute approximate 95% confidence intervals
predicted_df$conf.low <- predicted_df$predicted - 1.96 * predicted_df$se
predicted_df$conf.high <- predicted_df$predicted + 1.96 * predicted_df$se

# Merge with the original data
predicted_df <- cbind(final_data[, c("PortfolioG_speakerinfo", "Sex_speakerinfo")], predicted_df)

# Convert factor levels
predicted_df$PortfolioG_speakerinfo <- factor(predicted_df$PortfolioG_speakerinfo, 
                                              levels = c(1, 2, 3), 
                                              labels = c("Masculine", "Neutral", "Feminine"))

predicted_df$Sex_speakerinfo <- factor(predicted_df$Sex_speakerinfo, 
                                       levels = c(0, 1), 
                                       labels = c("Men", "Women"))

# Create the scatter plot with thicker error bars and a black frame
plot2 <- ggplot(predicted_df, aes(x = PortfolioG_speakerinfo, y = predicted, color = Sex_speakerinfo)) +
  geom_point(size = 4, position = position_dodge(width = 0.3)) +  # Scatter points, slightly dodged
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1.3,  # Thicker error bars
                position = position_dodge(width = 0.3), alpha = 0.8) +
  scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +  # Custom colors
  labs(
    x = "Portfolio-Group",
    y = "Predicted Number of Claims",
    color = "Sex Commissioner",
    title = "Interaction of Portfolio and Sex on Claim Count"
  ) +
  theme_classic() +  # Better for adding a black frame
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Black frame around the plot
  )

# Ensure folder exists before saving
save_folder <- "C:/Users/User/Documents/plots/"
if (!dir.exists(save_folder)) dir.create(save_folder, recursive = TRUE)


# Save the plot
ggsave(filename = paste0(save_folder, "Interaction_Effect_Portfolio_Sex_Count.svg"), 
       plot = plot2, width = 8, height = 6, dpi = 300, device = "svg")

levels(final_data$Commission_speakerinfo)  # If it's a factor
unique(final_data$Commission_speakerinfo)  # If it's numeric or character


# Generate predictions
predicted_values <- predict(zinb_glmm_6, type = "response", se.fit = TRUE)

# Convert to a dataframe
predicted_df <- data.frame(
  predicted = predicted_values$fit,
  se = predicted_values$se.fit
)

# Compute approximate 95% confidence intervals
predicted_df$conf.low <- predicted_df$predicted - 1.96 * predicted_df$se
predicted_df$conf.high <- predicted_df$predicted + 1.96 * predicted_df$se

# Merge predictions back with the relevant columns from final_data
predicted_df <- cbind(final_data[, c("PortfolioG_speakerinfo", "Sex_speakerinfo", "Commission_speakerinfo")], predicted_df)

# Ensure Commission is treated as an ordered factor based on the correct chronology
predicted_df$Commission_speakerinfo <- factor(predicted_df$Commission_speakerinfo,
                                              levels = c("Delors I", "Delors II", "Delors III", 
                                                         "Santer", "Marín", "Prodi", 
                                                         "Barroso I", "Barroso II", 
                                                         "Juncker", "Von der Leyen I", "Von der Leyen II"),
                                              ordered = TRUE)

# Convert PortfolioG_speakerinfo and Sex_speakerinfo to factors
predicted_df$PortfolioG_speakerinfo <- factor(predicted_df$PortfolioG_speakerinfo, 
                                              levels = c(1, 2, 3), 
                                              labels = c("Masculine", "Neutral", "Feminine"))

predicted_df$Sex_speakerinfo <- factor(predicted_df$Sex_speakerinfo, 
                                       levels = c(0, 1), 
                                       labels = c("Men", "Women"))

# Create the line plot with separate lines for each Portfolio category over time
plot2 <- ggplot(predicted_df, aes(x = Commission_speakerinfo, y = predicted, 
                                  color = PortfolioG_speakerinfo, group = PortfolioG_speakerinfo)) +
  geom_line(size = 1.5) +  # Line plot for trends
  geom_point(size = 3) +  # Add points for clarity
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1.2, alpha = 0.8) +  # Add error bars
  scale_color_manual(values = c("Masculine" = "#003399", "Neutral" = "#009933", "Feminine" = "#FFCC00")) +  # Custom colors
  labs(
    x = "Commission Period",
    y = "Predicted Number of Claims",
    color = "Portfolio Category",
    title = "Predicted Claims by Portfolio Category Over Time"
  ) +
  theme_classic() +  # Clean theme
  theme(
    text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate Commission labels for readability
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 12),
    panel.border = element_rect(color = "black", fill = NA, size = 1)  # Black frame around the plot
  ) +
  facet_wrap(~Sex_speakerinfo)  # Separate panels for Men and Women

# Save the plot
ggsave(filename = paste0(save_folder, "Predicted_Claims_by_Portfolio_Over_Time.svg"), 
       plot = plot2, width = 10, height = 6, dpi = 300, device = "svg")


# Compute average predicted claims per Commission period, Portfolio category, and Sex
predicted_avg <- predicted_df %>%
  group_by(Commission_speakerinfo, Sex_speakerinfo, PortfolioG_speakerinfo) %>%
  summarise(
    predicted = mean(predicted, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE)
  ) %>%
  ungroup()

# Ensure the Commission order remains correct
predicted_avg$Commission_speakerinfo <- factor(predicted_avg$Commission_speakerinfo,
                                               levels = c("Delors I", "Delors II", "Delors III", 
                                                          "Santer", "Marín", "Prodi", 
                                                          "Barroso I", "Barroso II", 
                                                          "Juncker", "Von der Leyen I", "Von der Leyen II"),
                                               ordered = TRUE)

# Define color palette for Sex
sex_colors <- c("Men" = "#003399", "Women" = "#FFCC00")

# Create separate plots for each Portfolio category
plots <- list()
for (category in c("Masculine", "Neutral", "Feminine")) {
  
  plot <- ggplot(predicted_avg %>% filter(PortfolioG_speakerinfo == category), 
                 aes(x = Commission_speakerinfo, y = predicted, color = Sex_speakerinfo, group = Sex_speakerinfo)) +
    geom_line(size = 1.5) +  # Line plot for trends
    geom_point(size = 3) +  # Add points for clarity
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1.2, alpha = 0.8) +  # Add error bars
    scale_color_manual(values = sex_colors) +  # Custom colors
    labs(
      x = "Commission Period",
      y = "Average Predicted Number of Claims",
      color = "Sex Commissioner",
      title = paste("Predicted Claims in", category, "Portfolio Over Time")
    ) +
    theme_classic() +  # Clean theme
    theme(
      text = element_text(size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate Commission labels for readability
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 12),
      panel.border = element_rect(color = "black", fill = NA, size = 1)  # Black frame around the plot
    )
  
  # Save each plot to the list
  plots[[category]] <- plot
  
  # Save the plot as a separate file
  ggsave(filename = paste0(save_folder, "Predicted_Claims_", category, "_Portfolio_Over_Time.svg"), 
         plot = plot, width = 10, height = 6, dpi = 300, device = "svg")
}

# Display all three plots
plots



# Compute average predicted claims and confidence intervals per Commission period, Portfolio category, and Sex
predicted_avg <- predicted_df %>%
  group_by(Commission_speakerinfo, Sex_speakerinfo, PortfolioG_speakerinfo) %>%
  summarise(
    predicted = mean(predicted, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE)
  ) %>%
  ungroup()

# Ensure Commission order remains correct
predicted_avg$Commission_speakerinfo <- factor(predicted_avg$Commission_speakerinfo,
                                               levels = c("Delors I", "Delors II", "Delors III", 
                                                          "Santer", "Marín", "Prodi", 
                                                          "Barroso I", "Barroso II", 
                                                          "Juncker", "Von der Leyen I", "Von der Leyen II"),
                                               ordered = TRUE)

# Define color palette for Sex
sex_colors <- c("Men" = "#003399", "Women" = "#FFCC00")

# Loop through each Portfolio category and save separate marginal effects plots
for (category in c("Masculine", "Neutral", "Feminine")) {
  
  plot <- ggplot(predicted_avg %>% filter(PortfolioG_speakerinfo == category), 
                 aes(x = Commission_speakerinfo, y = predicted, color = Sex_speakerinfo, group = Sex_speakerinfo)) +
    geom_point(size = 4, position = position_dodge(width = 0.4)) +  # Larger points
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.2, size = 1.4, position = position_dodge(width = 0.4)) +  # More visible whiskers
    scale_color_manual(values = sex_colors) +  # Custom colors
    labs(
      x = "Commission Period",
      y = "Average Predicted Number of Claims",
      color = "Sex Commissioner",
      title = paste("Predicted Claims in", category, "Portfolio Over Time")
    ) +
    theme_classic() +  # Clean theme
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate Commission labels for readability
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 14),
      panel.border = element_rect(color = "black", fill = NA, size = 1)  # Black frame around the plot
    )
  
  # Save each plot as an SVG file
  ggsave(filename = paste0(save_folder, "Predicted_Claims_", category, "_Portfolio_Marginal_Effects.svg"), 
         plot = plot, width = 10, height = 6, dpi = 300, device = "svg")
}

# Print confirmation message
print("All three marginal effects plots with confidence intervals have been saved as SVG files.")
str(predicted_avg)

predicted_values <- predict(zinb_glmm_6, type = "response", se.fit = TRUE)
summary(predicted_values$se.fit)  # Check if standard errors are missing

# Model does not return SE - Bootstrapping them is necessary

predict_fun <- function(model) {
  predict(model, type = "response")
}

set.seed(123)  # Ensure reproducibility
boot_results <- bootMer(zinb_glmm_6, predict_fun, nsim = 1000)  # 1000 bootstrap samples

saveRDS(boot_results, file = "C:/Users/User/Documents/boot_results.rds")

# Extract confidence intervals (2.5% and 97.5% percentiles)
conf_intervals <- apply(boot_results$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)

# Convert to dataframe
conf_df <- data.frame(
  conf.low = conf_intervals[1, ],
  conf.high = conf_intervals[2, ]
)

# Merge with predictions
predicted_df <- cbind(predicted_df, conf_df)

# Check if confidence intervals are now valid
summary(predicted_df$conf.low)
summary(predicted_df$conf.high)

predicted_avg <- predicted_df %>%
  group_by(Commission_speakerinfo, Sex_speakerinfo, PortfolioG_speakerinfo) %>%
  summarise(
    predicted = mean(predicted, na.rm = TRUE),
    conf.low = mean(conf.low, na.rm = TRUE),
    conf.high = mean(conf.high, na.rm = TRUE)
  ) %>%
  ungroup()
for (category in c("Masculine", "Neutral", "Feminine")) {
  
  plot <- ggplot(predicted_avg %>% filter(PortfolioG_speakerinfo == category), 
                 aes(x = Commission_speakerinfo, y = predicted, color = Sex_speakerinfo, group = Sex_speakerinfo)) +
    geom_point(size = 4, position = position_dodge(width = 0.4)) +  # Larger points
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.2, size = 1.4, position = position_dodge(width = 0.4)) +  # More visible whiskers
    scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +  # Custom colors
    labs(
      x = "Commission Period",
      y = "Average Predicted Number of Claims",
      color = "Sex Commissioner",
      title = paste("Predicted Claims in", category, "Portfolio Over Time")
    ) +
    theme_classic() +  # Clean theme
    theme(
      text = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate Commission labels for readability
      legend.position = "bottom",
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 14),
      panel.border = element_rect(color = "black", fill = NA, size = 1)  # Black frame around the plot
    )
  
  # Save each plot as an SVG file
  ggsave(filename = paste0(save_folder, "Predicted_Claims_", category, "_Portfolio_Marginal_Effects.svg"), 
         plot = plot, width = 10, height = 6, dpi = 300, device = "svg")
}

print("All three marginal effects plots with bootstrapped confidence intervals have been saved as SVG files.")

library(dplyr)
library(lubridate)

# Ensure SpeechDate is in Date format
final_data$SpeechDate <- as.Date(final_data$date, format = "%Y-%m-%d")

library(dplyr)
library(lubridate)
library(lme4)
library(car)

# (Optional) Create a unique identifier if not already present
final_data <- final_data %>% mutate(row_id = row_number())

### 1. Function to Compute Binary Lag Indicators ###

compute_lags <- function(data, time_window) {
  data %>%
    arrange(SpeechDate) %>%
    group_by(Reassigned_Topic) %>%
    mutate(
      women_topic_lag = sapply(row_number(), function(i) {
        if(i == 1) return(FALSE)
        current_date <- SpeechDate[i]
        any(Sex_speakerinfo[1:(i-1)] == 1 & 
              SpeechDate[1:(i-1)] >= (current_date - weeks(time_window)))
      }),
      women_claim_lag = sapply(row_number(), function(i) {
        if(i == 1) return(FALSE)
        current_date <- SpeechDate[i]
        any(Sex_speakerinfo[1:(i-1)] == 1 & claimbinary[1:(i-1)] == 1 &
              SpeechDate[1:(i-1)] >= (current_date - weeks(time_window)))
      })
    ) %>%
    ungroup()
}

### 2. Generate Lag Variables for Multiple Time Windows (1-5 weeks) ###

# We'll join new lag columns back to the original data using the unique row_id
final_data_lag <- final_data

for (w in 1:5) {
  tmp <- compute_lags(final_data, time_window = w) %>%
    select(row_id, women_topic_lag, women_claim_lag) %>%
    rename(!!paste0("women_topic_lag_", w, "w") := women_topic_lag,
           !!paste0("women_claim_lag_", w, "w") := women_claim_lag)
  
  final_data_lag <- left_join(final_data_lag, tmp, by = "row_id")
}

### 3. Fit GLMMs Using the Lag Variables ###

# Filter for male speakers only
male_speeches <- final_data_lag %>% filter(Sex_speakerinfo == 0)

# Fit logistic GLMMs for each lag window
models <- list()
for (w in 1:5) {
  formula <- as.formula(paste0("claimbinary ~ women_topic_lag_", w, "w + women_claim_lag_", w, "w + 
                                 PortfolioG_speakerinfo + length + European.Party_speakerinfo + 
                                 (1 | Commission_speakerinfo)"))
  
  models[[paste0("logit_glmm_", w, "w")]] <- glmer(formula, data = male_speeches, family = binomial, 
                                                   control = glmerControl(optimizer = "bobyqa"))
}

# Compare AIC scores for the different lag windows
AIC_results <- sapply(models, AIC)
print(AIC_results)

# Select best model (lowest AIC)
best_lag_index <- which.min(AIC_results)
best_model <- models[[best_lag_index]]
summary(best_model)

# Check multicollinearity for the 1-week lag model using VIF
vif(glm(claimbinary ~ women_topic_lag_1w + women_claim_lag_1w + 
          PortfolioG_speakerinfo + length + European.Party_speakerinfo, 
        data = male_speeches, family = binomial))

# Display a summary of lag variables for weeks 1-3
summary(male_speeches[, c("women_topic_lag_1w", "women_claim_lag_1w",
                          "women_topic_lag_2w", "women_claim_lag_2w",
                          "women_topic_lag_3w", "women_claim_lag_3w")])

### 4. Alternative Approach: Creating Lag Count Variables ###

compute_lag_counts <- function(data, time_window) {
  data %>%
    arrange(SpeechDate) %>%
    group_by(speaker.name, Reassigned_Topic) %>%
    mutate(
      women_topic_lag_count = sapply(row_number(), function(i) {
        if(i == 1) return(0)
        current_date <- SpeechDate[i]
        sum(Sex_speakerinfo[1:(i-1)] == 1 & 
              SpeechDate[1:(i-1)] >= (current_date - weeks(time_window)))
      }),
      women_claim_lag_count = sapply(row_number(), function(i) {
        if(i == 1) return(0)
        current_date <- SpeechDate[i]
        sum(Sex_speakerinfo[1:(i-1)] == 1 & claimbinary[1:(i-1)] == 1 & 
              SpeechDate[1:(i-1)] >= (current_date - weeks(time_window)))
      })
    ) %>%
    ungroup()
}

# Define the time window (e.g., 3 weeks)
time_window <- 3

male_speeches_counts <- final_data %>%
  filter(Sex_speakerinfo == 0) %>%
  mutate(row_id = row_number())  # in case a new unique id is needed

male_speeches_counts <- compute_lag_counts(male_speeches_counts, time_window)

# Fit a GLMM using the count variables
logit_glmm_opt <- glmer(claimbinary ~ women_topic_lag_count +
                          PortfolioG_speakerinfo + length_scaled + European.Party_speakerinfo + 
                          (1 | Commission_speakerinfo),  
                        data = male_speeches_counts, 
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
summary(logit_glmm_opt)

# Check VIF for the count model
vif(glm(claimbinary ~ women_topic_lag_count +  
          PortfolioG_speakerinfo + length_scaled + European.Party_speakerinfo, 
        data = male_speeches_counts, 
        family = binomial))

# Check for aliasing and correlation between count variables
alias(glm(claimbinary ~ women_topic_lag_count +  
            PortfolioG_speakerinfo + length_scaled + European.Party_speakerinfo, 
          data = male_speeches_counts, 
          family = binomial))
cor(male_speeches_counts$women_topic_lag_count, male_speeches_counts$women_claim_lag_count, use = "complete.obs")

# Fit a model with only the claim count predictor
logit_glmm_fix <- glmer(claimbinary ~ women_topic_lag_count +  
                          PortfolioG_speakerinfo + length_scaled + European.Party_speakerinfo + 
                          (1 | Commission_speakerinfo),  
                        data = male_speeches_counts, 
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))
summary(logit_glmm_fix)
