### Analysis Paper 2 - Influence of Speech Topics on Claim-Making ###
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
# Load libraries
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

# Convert categorical variables
final_data$Commission_speakerinfo <- as.factor(final_data$Commission_speakerinfo)
final_data$speaker.name <- as.factor(final_data$speaker.name)
final_data$Sex_speakerinfo <- as.factor(final_data$Sex_speakerinfo)
final_data <- final_data %>%
  mutate(
    length_scaled = scale(length),
    w_top_tot_scaled = scale(w_top_tot),
    w_top_rel_scaled = scale(w_top_rel)
  )

# ------------------------------------
# **1️⃣ Logistic GLMM for Claim Probability (Binary Outcome)**
# ------------------------------------
logit_glmm <- glmer(claimbinary ~ length + Sex_speakerinfo + 
                      (1 | Commission_speakerinfo/speaker.name), 
                    data = final_data, 
                    family = binomial,
                    control = glmerControl(optimizer = "bobyqa"))

summary(logit_glmm)

# **With `w_top_tot`**
logit_glmm_tot <- glmer(claimbinary ~ length + Sex_speakerinfo + w_top_tot +
                          (1 | Commission_speakerinfo/speaker.name), 
                        data = final_data, 
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))

summary(logit_glmm_tot)

# **With `w_top_rel`**
logit_glmm_rel <- glmer(claimbinary ~ length + Sex_speakerinfo + w_top_rel +
                          (1 | Commission_speakerinfo/speaker.name), 
                        data = final_data, 
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))

summary(logit_glmm_rel)

# ------------------------------------
# **2️⃣ Zero-Inflated Negative Binomial (ZINB) GLMM for Claim Count**
# ------------------------------------
# **Baseline Model**
zinb_glmm <- glmmTMB(claimcount ~ length + Sex_speakerinfo +
                       (1 | Commission_speakerinfo) + (1 | speaker.name), 
                     data = final_data, 
                     family = nbinom2,
                     ziformula = ~1,
                     control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

summary(zinb_glmm)
check_collinearity(zinb_glmm)
# **With `w_top_tot`**
zinb_glmm_tot <- glmmTMB(claimcount ~ length + Sex_speakerinfo + w_top_tot +
                           (1 | Commission_speakerinfo/speaker.name),
                         data = final_data, 
                         family = nbinom2,
                         ziformula = ~1,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

summary(zinb_glmm_tot)

# **With `w_top_rel`**
zinb_glmm_rel <- glmmTMB(claimcount ~ length + Sex_speakerinfo + w_top_rel +
                           (1 | Commission_speakerinfo/speaker.name),
                         data = final_data, 
                         family = nbinom2,
                         ziformula = ~1,
                         control = glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS")))

summary(zinb_glmm_rel)


save_folder <- "C:/Users/User/Documents/plots/"

# **1️⃣ Marginal Effects for Speech Length**
me_length <- ggpredict(zinb_glmm_rel, terms = "length")
plot1 <- ggplot(me_length, aes(x = x, y = predicted)) +
  geom_line(color = "#003399", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#003399") +
  labs(title = "Effect of Speech Length on Claim Count",
       x = "Speech Length (Words)",
       y = "Predicted Number of Claims") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )
ggsave(filename = paste0(save_folder, "Effect of Speech Length on Claim Count.svg"), plot = plot1, width = 8, height = 6, dpi = 300, device = "svg")


# **2️⃣ Marginal Effects for Speaker Sex**
me_sex <- ggpredict(zinb_glmm_rel, terms = "Sex_speakerinfo")
plot2 <- ggplot(me_sex, aes(x = x, y = predicted, fill = x)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#003399", "1" = "#FFCC00")) +
  labs(title = "Effect of Speaker Sex on Claim Count",
       x = "Speaker Sex (0 = Men, 1 = Women)",
       y = "Predicted Number of Claims") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )
ggsave(filename = paste0(save_folder, "Effect of Speaker Sex on Claim Count.svg"), plot = plot2, width = 8, height = 6, dpi = 300, device = "svg")


# **3️⃣ Marginal Effects for Women's Issues Focus (`w_top_rel`)**
me_w_top <- ggpredict(zinb_glmm_rel, terms = "w_top_rel")
plot3 <- ggplot(me_w_top, aes(x = x, y = predicted)) +
  geom_line(color = "#FFCC00", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FFCC00") +
  labs(title = "Effect of Women's Issues Focus on Claim Count",
       x = "Relative Percentage of Speech on Women's Issues",
       y = "Predicted Number of Claims") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )
ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Claim Count.svg"), plot = plot3, width = 8, height = 6, dpi = 300, device = "svg")


# **3️⃣ Marginal Effects for Women's Issues Focus (`w_top_tot`)**
me_w_top_tot <- ggpredict(zinb_glmm_tot, terms = "w_top_tot")

# Plot the marginal effects
plot4 <- ggplot(me_w_top_tot, aes(x = x, y = predicted)) +
  geom_line(color = "#FFCC00", size = 1) +  # Yellow line
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FFCC00") +
  labs(title = "Effect of Women's Issues Focus on Claim Count",
       x = "Total Percentage of Speech on Women's Issues (`w_top_tot`)",
       y = "Predicted Number of Claims") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )
ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Claim Count total.svg"), plot = plot4, width = 8, height = 6, dpi = 300, device = "svg")


# **1️⃣ Marginal Effects of Speech Length by Speaker Sex**
me_length_sex <- ggpredict(zinb_glmm_rel, terms = c("length", "Sex_speakerinfo"))
plot5 <- ggplot(me_length_sex, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_color_manual(values = c("0" = "#003399", "1" = "#FFCC00")) +
  scale_fill_manual(values = c("0" = "#003399", "1" = "#FFCC00")) +
  labs(title = "Effect of Speech Length on Claim Count by Speaker Sex",
       x = "Speech Length (Words)",
       y = "Predicted Number of Claims",
       color = "Speaker Sex",
       fill = "Speaker Sex") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )
ggsave(filename = paste0(save_folder, "Effect of Speech Length on Claim Count by Speaker Sex.svg"), plot = plot5, width = 8, height = 6, dpi = 300, device = "svg")


# **2️⃣ Marginal Effects of Women's Issues Focus (`w_top_rel`) by Speaker Sex**
me_w_top_sex <- ggpredict(zinb_glmm_rel, terms = c("w_top_rel", "Sex_speakerinfo"))
me_w_top_sex$group <- factor(me_w_top_sex$group, levels = c("0", "1"), labels = c("Men", "Women"))
plot6 <- ggplot(me_w_top_sex, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  scale_fill_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  labs(title = "Effect of Women's Issues Focus on Claim Count by Speaker Sex",
       x = "Relative Percentage of Speech on Women's Issues",
       y = "Predicted Number of Claims",
       color = "Speaker Sex",
       fill = "Speaker Sex") +
  theme_minimal()+
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )
ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Claim Count by Speaker Sex.svg"), plot = plot6, width = 8, height = 6, dpi = 300, device = "svg")


# **2️⃣ Marginal Effects of Women's Issues Focus (`w_top_tot`) by Speaker Sex**
# **Marginal Effects of Women's Issues Focus (`w_top_tot`) by Speaker Sex**
me_w_top_tot_sex <- ggpredict(zinb_glmm_tot, terms = c("w_top_tot", "Sex_speakerinfo"))

# Convert "Sex_speakerinfo" values to meaningful labels
me_w_top_tot_sex$group <- factor(me_w_top_tot_sex$group, levels = c("0", "1"), labels = c("Men", "Women"))

# Plot with no grid and black outer frame
plot7 <- ggplot(me_w_top_tot_sex, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  scale_fill_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  labs(title = "Effect of Women's Issues Focus on Claim Count by Speaker Sex",
       x = "Total Percentage of Speech on Women's Issues",
       y = "Predicted Number of Claims",
       color = "Speaker Sex",
       fill = "Speaker Sex") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )

ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Claim Count by Speaker Sex Total.svg"), plot = plot7, width = 8, height = 6, dpi = 300, device = "svg")


# **1️⃣ Marginal Effects for Speech Length (`length`)**
me_length_logit <- ggpredict(logit_glmm_rel, terms = "length")
plot8 <- ggplot(me_length_logit, aes(x = x, y = predicted)) +
  geom_line(color = "#003399", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#003399") +
  labs(title = "Effect of Speech Length on Probability of Claim-Making",
       x = "Speech Length (Words)",
       y = "Pred. Prob. of Making a Claim") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)
  )

ggsave(filename = paste0(save_folder, "Effect of Speech Length on Probability of Claim-Making.svg"), plot = plot8, width = 8, height = 6, dpi = 300, device = "svg")

# **2️⃣ Marginal Effects for Speaker Sex (`Sex_speakerinfo`)**
me_sex_logit <- ggpredict(logit_glmm_rel, terms = "Sex_speakerinfo")
plot9 <- ggplot(me_sex_logit, aes(x = x, y = predicted, fill = x)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#003399", "1" = "#FFCC00"),
                    labels = c("Men", "Women")) +
  labs(title = "Effect of Speaker Sex on Probability of Claim-Making",
       x = "Speaker Sex",
       y = "Pred. Prob. of Making a Claim") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)
  )
ggsave(filename = paste0(save_folder, "Effect of Speaker Sex on Probability of Claim-Making.svg"), plot = plot9, width = 8, height = 6, dpi = 300, device = "svg")

# **3️⃣ Marginal Effects for Women's Issues Focus (`w_top_rel`)**
me_w_top_logit <- ggpredict(logit_glmm_rel, terms = "w_top_rel")
plot10 <- ggplot(me_w_top_logit, aes(x = x, y = predicted)) +
  geom_line(color = "#FFCC00", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FFCC00") +
  labs(title = "Effect of Women's Issues Focus on Probability of Claim-Making",
       x = "Relative Percentage of Speech on Women's Issues",
       y = "Pred. Prob. of Making a Claim") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)
  )

ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Probability of Claim-Making.svg"), plot = plot10, width = 8, height = 6, dpi = 300, device = "svg")

# **Marginal Effects of Women's Issues Focus by Speaker Sex**
me_w_top_sex_logit <- ggpredict(logit_glmm_rel, terms = c("w_top_rel", "Sex_speakerinfo"))

# Convert "Sex_speakerinfo" values to meaningful labels
me_w_top_sex_logit$group <- factor(me_w_top_sex_logit$group, levels = c("0", "1"), labels = c("Men", "Women"))

# Plot with corrected legend labels
plot11 <- ggplot(me_w_top_sex_logit, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  scale_fill_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  labs(title = "Effect of Women's Issues Focus on Probability of Claim-Making by Speaker Sex",
       x = "Relative Percentage of Speech on Women's Issues",
       y = "Pred. Prob. of Making a Claim",
       color = "Speaker Sex",
       fill = "Speaker Sex") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)
  )
ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Probability of Claim-Making by Speaker Sex.svg"), plot = plot11, width = 8, height = 6, dpi = 300, device = "svg")


# **1️⃣ Marginal Effects for Speech Length (`length`)**
me_length_logit_tot <- ggpredict(logit_glmm_tot, terms = "length")
plot12 <- ggplot(me_length_logit_tot, aes(x = x, y = predicted)) +
  geom_line(color = "#003399", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#003399") +
  labs(title = "Effect of Speech Length on Probability of Claim-Making",
       x = "Speech Length (Words)",
       y = "Pred. Prob. of Making a Claim") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
ggsave(filename = paste0(save_folder, "Effect of Speech Length on Probability of Claim-Making total.svg"), plot = plot12, width = 8, height = 6, dpi = 300, device = "svg")

# **2️⃣ Marginal Effects for Speaker Sex (`Sex_speakerinfo`)**
me_sex_logit_tot <- ggpredict(logit_glmm_tot, terms = "Sex_speakerinfo")
plot13 <- ggplot(me_sex_logit_tot, aes(x = x, y = predicted, fill = x)) +
  geom_bar(stat = "identity", color = "black", alpha = 0.8) +
  scale_fill_manual(values = c("0" = "#003399", "1" = "#FFCC00"),
                    labels = c("Men", "Women")) +
  labs(title = "Effect of Speaker Sex on Probability of Claim-Making",
       x = "Speaker Sex",
       y = "Pred. Prob. of Making a Claim") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
ggsave(filename = paste0(save_folder, "Effect of Speaker Sex on Probability of Claim-Making total.svg"), plot = plot13, width = 8, height = 6, dpi = 300, device = "svg")

# **3️⃣ Marginal Effects for Women's Issues Focus (`w_top_tot`)**
me_w_top_logit_tot <- ggpredict(logit_glmm_tot, terms = "w_top_tot")
plot14 <- ggplot(me_w_top_logit_tot, aes(x = x, y = predicted)) +
  geom_line(color = "#FFCC00", size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "#FFCC00") +
  labs(title = "Effect of Women's Issues Focus on Probability of Claim-Making",
       x = "Total Percentage of Speech on Women's Issues",
       y = "Pred. Prob. of Making a Claim") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)
  )
ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Probability of Claim-Making total.svg"), plot = plot14, width = 8, height = 6, dpi = 300, device = "svg")

# **Marginal Effects of `w_top_tot` by Speaker Sex**
me_w_top_sex_logit_tot <- ggpredict(logit_glmm_tot, terms = c("w_top_tot", "Sex_speakerinfo"))

# Convert "Sex_speakerinfo" values to meaningful labels
me_w_top_sex_logit_tot$group <- factor(me_w_top_sex_logit_tot$group, levels = c("0", "1"), labels = c("Men", "Women"))

# Plot with corrected legend labels
plot15 <- ggplot(me_w_top_sex_logit_tot, aes(x = x, y = predicted, color = group)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  scale_color_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  scale_fill_manual(values = c("Men" = "#003399", "Women" = "#FFCC00")) +
  labs(title = "Effect of Women's Issues Focus on Probability of Claim-Making by Speaker Sex",
       x = "Total Percentage of Speech on Women's Issues",
       y = "Pred. Prob. of Making a Claim",
       color = "Speaker Sex",
       fill = "Speaker Sex") +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Remove grid
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1)  # Add black frame
  )
ggsave(filename = paste0(save_folder, "Effect of Women's Issues Focus on Probability of Claim-Making by Speaker Sex total.svg"), plot = plot15, width = 8, height = 6, dpi = 300, device = "svg")



library(modelsummary)

# Define the list of models
model_list <- list("Model 1" = zinb_glmm, "Model 2" = zinb_glmm_rel, "Model 3" = zinb_glmm_tot)

# Extract estimates from one model to inspect its structure (optional debugging step)
get_estimates(zinb_glmm)

# Create a regression table, separating components to avoid duplicate term names
modelsummary(model_list,
             output = "regression_results.html",
             stars = TRUE,
             shape = term ~ model + component,  # Separates conditional & zero-inflation coefficients
             title = "Zero-Inflated Negative Binomial Regression Results",
             gof_omit = "IC|Log|Deviance")  # Removes some model fit statistics

library(clipr)
library(magrittr)# Install clipr if needed
library(modelsummary)
library(glmmTMB)

# Define the list of models
model_list <- list("Model 1" = zinb_glmm, "Model 2" = zinb_glmm_rel, "Model 3" = zinb_glmm_tot)

# Extract estimates from one model to inspect its structure (optional debugging step)
get_estimates(zinb_glmm)

# Create a regression table, separating components to avoid duplicate term names
modelsummary(model_list,
             stars = TRUE,
             shape = term ~ model + component,  # Separates conditional & zero-inflation coefficients
             title = "Zero-Inflated Negative Binomial Regression Results",
             gof_omit = "IC|Log|Deviance")  # Removes some model fit statistics

modelsummary(model_list, stars = TRUE, shape = term ~ model + component) %>%
  write_clip()  # Copies the table to clipboard, ready to paste in Word


# Define the list of models
model_list <- list("Model 1" = logit_glmm, "Model 2" = logit_glmm_rel, "Model 3" = logit_glmm_tot)


# Create a regression table, separating components to avoid duplicate term names
modelsummary(model_list,
             stars = TRUE,
             title = "Logistic GLMM Regression Results",
             gof_omit = "IC|Log|Deviance")  # Removes some model fit statistics

modelsummary(model_list, stars = TRUE, shape = term ~ model + component) %>%
  write_clip()  # Copies the table to clipboard, ready to paste in Word


