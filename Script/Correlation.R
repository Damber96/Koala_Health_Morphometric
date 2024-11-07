# Correlation 

dat_cor <- dat[-33, ]


# -----------------------------------------------------------------------
hist(dat_cor$Age_yr)
shapiro.test(dat_cor$Age_yr) # Distribution is nonnormal (p < 0.05)


hist(dat_cor$Mass_kg)
shapiro.test(dat_cor$Mass_kg) # Distribution is normal (p > 0.05)


cor.test(dat_cor$Age_yr, dat_cor$Mass_kg, method = "spearman")

library(ggplot2)

# Plotting correlation
corplot_Age_Mass <- ggplot(dat_cor, aes(x = Age_yr, y = Mass_kg)) +
  geom_point(size = 0.75, shape = 3, alpha = 0.75) +  # Add data points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a fitted regression line
  labs(x = "Age (years)", y = "Body mass (kg)") +
  annotate("text", x=5.25, y=9.25, label= "|r| = 0.48, p < 0.005", size = 3.25) +
  theme_bw()


# -----------------------------------------------------------------------
hist(dat_cor$Head_len)
shapiro.test(dat_cor$Head_len) # Normal

cor.test(dat_cor$Age_yr, dat_cor$Head_len, method = "spearman")

# Plotting correlation
corplot_Age_HLen <- ggplot(dat_cor, aes(x = Age_yr, y = Head_len)) +
  geom_point(size = 0.75, shape = 3, alpha = 0.75) +  # Add data points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a fitted regression line
  labs(x = "Age (years)", y = "Head length (mm)") +
  annotate("text", x=5.5, y=160, label= "|r| = 0.5, p < 0.01", size = 3.25) +
  theme_bw()


# Age Vs Head width
hist(dat_cor$Head_wid)
shapiro.test(dat_cor$Head_wid) # Normal

cor.test(dat_cor$Age_yr, dat_cor$Head_wid, method = "spearman")


# Plotting correlation
View(dat)
corplot_Age_HWid <- ggplot(dat_cor, aes(x = Age_yr, y = Head_wid)) +
  geom_point(size = 0.75, shape = 3, alpha = 0.75) +  # Add data points
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a fitted regression line
  labs(x = "Age (years)", y = "Head width (mm)") +
  annotate("text", x=5.5, y=88.5, label= "|r| = 0.49, p < 0.01", size = 3.25) +
  theme_bw()

library("cowplot")

cor_plots <- plot_grid(corplot_Age_Mass, corplot_Age_HLen, corplot_Age_HWid,
                       nrow = 1, ncol = 3,
                       labels = c('A.', 'B.', 'C.'),
                       label_size = 10)
# Export plot
ggsave(filename = "./Output/Cor_plot.jpg", plot = cor_plots, 
       height = 3.5, width = 10, dpi = 300)
