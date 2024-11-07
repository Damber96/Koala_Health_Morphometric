# Plotting significant blood-test results
library(tidyverse)
library(ggplot2)

# Hb
dat_blood$Sex <- factor(dat_blood$Sex, labels = c("Female", "Male"))

# ------------------------------------------------------------------------------
# Hb plot for Sex
new_data_hb <- with(dat_blood, expand.grid(Age_yr = sort(unique(dat_blood$Age_yr)), Sex = levels(Sex)))
predicted_hb <- predict(glm_Hb, newdata = new_data_hb, type = "response")

predicted_dat_hb <- cbind(new_data_hb, Predicted_HB = predicted_hb) # Use this data for plotting predicted hb


plot_hb <- ggplot(dat_blood, aes(x = Sex, y = Hb)) + # But, I am plotting raw data
  geom_boxplot(col = c("red", "blue"), outlier.shape = NA) +
  # geom_point(size = 2, col = ifelse(dat_blood$Sex == "Male", "cornflowerblue", "chocolate"),
  #        shape = 3, alpha = 0.75,
  #    position = position_jitter(width = 0.3, height = NULL)) +
  labs(x = "", y = "Hb (gm/L)") +
  theme_bw()


# --------------------------------------------------------------------------
# HCT plot for Sex
new_data_hct <- with(dat_blood, expand.grid(Age_yr = sort(unique(dat_blood$Age_yr)), Sex = levels(Sex)))
predicted_hct <- predict(aov_Hct, newdata = new_data_hct, type = "response")

predicted_dat_hct <- cbind(new_data_hct, Predicted_Hct = predicted_hct)

plot_hct <- ggplot(dat_blood, aes(x = Sex, y = Hct)) +
  geom_boxplot(col = c("red", "blue")) +
  
  labs(x = "", y = "Hct (gm/L)") +
  theme_bw()

# --------------------------------------------------------------------------
# AST plot for Sex
new_data_ast <- with(dat_AST, expand.grid(Age_yr = sort(unique(dat_AST$Age_yr)), Sex = levels(Sex)))
predicted_ast <- predict(glm_AST, newdata = new_data_ast, type = "response")

predicted_dat_ast <- cbind(new_data_ast, Predicted_AST = predicted_ast)

plot_ast <- ggplot(dat_AST, aes(x = Sex, y = AST)) +
  geom_boxplot(col = c("red", "blue")) +
  
  labs(x = "", y = "ast (gm/L)") +
  theme_bw()

?position_jitter
# --------------------------------------------------------------------------
# AST plot for Age
plot_ast_yr <- ggplot(dat_AST, aes(x = Age_yr, y = AST)) +
  # geom_point(size = 1.5, col = "cornflowerblue", shape = 3, alpha = 1) +
  geom_smooth(method = "lm", se = T, color = "orange") +
  labs(x = "Age (yr)",
       y = "AST (units/L)T") +
  theme_bw()

# --------------------------------------------------------------------------
# NRBC plot
new_data_nrbc <- with(dat_nrbc, expand.grid(Age_yr = sort(unique(dat_nrbc$Age_yr)), Sex = levels(Sex)))
predicted_nrbc <- predict(glm_NRBC, newdata = new_data_nrbc, type = "response")

predicted_dat_nrbc <- cbind(new_data_nrbc, Predicted_nrbc = predicted_nrbc)

plot_nrbc <- ggplot(dat_nrbc, aes(x = Age_yr, y = NRBC)) +
  #  geom_point(size = 1.5, col = "cornflowerblue", shape = 3, alpha = 1) +
  geom_smooth(method = "lm", se = T, color = "orange") +
  labs(x = "Age (yr)",
       y = "NRBC (/100 WBCs)") +
  theme_bw()

# --------------------------------------------------------------------------
# Neut plot
new_data_neut <- with(dat_neut, expand.grid(Age_yr = sort(unique(dat_neut$Age_yr)), Sex = levels(Sex)))
predicted_neut <- predict(glm_Neut, newdata = new_data_neut, type = "response")

predicted_dat_neut <- cbind(new_data_neut, Predicted_neut = predicted_neut)

plot_neut <- ggplot(dat_neut, aes(x = Age_yr, y = Neut)) +
  # geom_point(size = 1.5, col = "cornflowerblue", shape = 3, alpha = 1) +
  geom_smooth(method = "lm", se = T, color = "orange") +
  labs(x = "Age (yr)",
       y = "Neut (gm/L)") +
  theme_bw()  

# --------------------------------------------------------------------------
# NL-ratio plot
new_data_nl <- with(dat_blood, expand.grid(Age_yr = sort(unique(dat_blood$Age_yr)), Sex = levels(Sex)))
predicted_nl <- predict(glm_nl, newdata = new_data_nl, type = "response")

predicted_dat_nl <- cbind(new_data_nl, Predicted_nl = predicted_nl)

plot_NL <- ggplot(dat_blood, aes(x = Age_yr, y = NL_ratio)) +
  geom_smooth(method = "lm", se = T, color = "orange") +
  #  geom_point(size = 1.5, col = "cornflowerblue", shape = 3, alpha = 1) +
  labs(x = "Age (yr)",
       y = "N:L ratio") +
  theme_bw()  

# Plotting all plots
library(cowplot)
blood_plots <- plot_grid(plot_nrbc, plot_neut, plot_NL, plot_ast_yr, 
                         plot_hb, plot_hct, plot_ast_sex,
                         nrow = 2, ncol = 4,
                         labels = c('A.', 'B.', 'C.', 'D.', 'E', 'F', 'G'),
                         label_size = 10)

# Export plot
ggsave(filename = "./Output/Blood_plots.jpg", plot = blood_plots, 
       height = 5, width = 10, dpi = 300)


10/11

# ========================
ggplot(dat_onlySnow_Dist, aes(x = Snow_age, y = Distance)) +
  geom_boxplot(position = position_dodge(width = 0.9), size = 0.75,
               outlier.shape = 16,
               outlier.size = 0.5, col = c("tan2", "ivory4")) +
  
  geom_point(aes(y = mean(dat_onlySnow_Dist$Distance, na.rm = T)), color = "red", shape = 15, size = 2.5) +
  
  scale_y_continuous(limits = c(0, 3000)) +
  
  
  theme(axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
        axis.test.y = element_text(size = 10)) +
  labs(title = "", y = "", x = "") +
  theme_bw()

