library("readxl")
library("tidyverse")
library("ggplot2")

data <- read_excel("./Data/Data_KoalaStatus_20231130.xlsx")

View(dat)

dat <- data %>% filter(Status == "Cleanskin" ) # Retaining only cleanskins (for avoiding duplication)
dim(dat)

dat$Sex_Num <- ifelse(dat$Sex == "Male", 0, 1)

counts <- aggregate(Sex_Num ~ Age_yr + Sex, data = dat, FUN = length)


# --------------------------------------------------------------------


# Counting males and females
dat %>% count(Sex) %>% mutate(Percent = n / sum(n) * 100)

# --------------------------------------------------------------------
# Sex ratio

chi_result <- chisq.test(table(dat$Sex))
chi_result$expected # Checking for Fisher Exact test (expected obs is >5 so, chi-square is a good fit)

# --------------------------------------------------------------------
hist(dat$Age_yr)

hist(log(dat$Age_yr))
shapiro.test(log(dat$Age_yr)) # Normal
dat %>% summarise(Age = mean(Age_yr), sd = sd(Age_yr), median = median((Age_yr)))


dat %>% group_by(Sex) %>% summarise(Age = mean(Age_yr), 
                                    sd = sd(Age_yr), 
                                    median(Age_yr), 
                                    range = range(Age_yr))


t.test(data = dat, log(Age_yr) ~ Sex, var.equal = T)
library("effsize")

cohen.d(data = dat, log(Age_yr) ~ Sex, var.equal = TRUE)

dat %>% group_by(Sex, Age) %>% summarise(Age = range(Age_yr))

# --------------------------------------------------------------------

Age_plot <- ggplot(counts, aes(x = Age_yr, fill = Sex,
                 y = ifelse(test = Sex == "Male",
                            yes = -Sex_Num, no = Sex_Num))) + 
  geom_bar(stat = "identity", alpha = 0.5) +
  scale_fill_manual(values = c("Male" = "blue", "Female" = "red")) +
 coord_flip() +
  labs(x = "Age (years)", y = "Number of koalas") +
  scale_x_continuous(breaks = c(0,3,6,9,12,15,18)) +
# scale_y_continuous(labels=c("-2" = "2", "-1" = "1",
          #                  "0" = "0", "1" = "1", "2" = "2")) +
  
  scale_y_continuous(breaks = c(-3,-2,-1,0,1,2,3),
                     labels=c("-3" = "3", "-2" = "2", "-1" = "1",
                            "0" = "0", "1" = "1", "2" = "2", "3" = "3")) +
  
  geom_text(aes(x = 16, y = -1.25, label = paste("Male = 21")), 
            hjust = 1, color = "black", size = 2.5, fontface = "plain") +
  
  geom_text(aes(x = 16, y = 2, label = paste("Female = 22")), 
            hjust = 1, color = "black", size = 2.5, fontface = "plain") +
  theme_bw() +
  theme(legend.position = "none")

?geom_text
  
# Export plot
ggsave(filename = "./Output/Age_plot.jpg", plot = Age_plot, 
       height = 5.5, width = 4.5, dpi = 250)

# ------------------------------------------------------------------------------
# Body mass

# Summary stattistics
dat_adult <- dat %>% filter(Age == "Adult")

# Normality
hist(dat_adult$Mass_kg)

shapiro.test(dat_adult$Mass_kg) # Distribution is normal (p > 0.05)

dat_adult %>% group_by(Sex) %>% summarise(Mass = mean(Mass_kg), sd = sd(Mass_kg), range = range(Mass_kg))

# t-test
t.test(data = dat_adult, Mass_kg ~ Sex, var.equal = T)

# Effect size
# Cohen's d: This tells you how big or small the observed difference between groups is.
# A larger Cohen's d value suggests a more substantial effect.

cohen.d(data = dat_adult, Mass_kg ~ Sex, var.equal = TRUE)

# Sign of Cohen's d indicates the direction of the effect (i.e., whether Group A (Males)
# has a higher or lower mean than Group B - Females), while the magnitude quantifies the size of the effect.

# Plot the mean and error
library(ggplot2)
library(tidyverse)
str(dat)
Mass_df <- dat %>% group_by(Sex, Age) %>% summarise(Mass = mean(Mass_kg), sd = sd(Mass_kg))

plot_mass <- ggplot(Mass_df, aes(x = Sex, y = Mass, fill = Age)) +
  geom_point(aes(color = Age), shape = 15, size = 2, position = position_dodge(width = 0.5)) +
  
  # Add error bars only for Adults
  geom_errorbar(data = Mass_df %>% filter(Age == "Adult"),
                aes(ymin = Mass - sd, ymax = Mass + sd, color = Age),
                position = position_nudge(x = -0.125),
                width = 0.05, size = 0.75, alpha = 0.25) +
  
  
  
  labs(x = "", y = "Body mass (kg)") +
  scale_color_manual(values = c("Adult" = "orange", "Sub-adult" = "darkgreen")) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  theme(legend.position = "none", legend.title = element_blank())

unique(dat$Age)


# ------------------------------------------------------------------------------
# Body condition
hist(dat_adult$Body_score)
lines(x = density(x = dat_adult$Body_score), col = "red")
shapiro.test(log(dat_adult$Body_score)) # Non-normal

var(dat_adult$Body_score, na.rm = T)

dat_adult %>% group_by(Sex) %>% summarise(Score = median(Body_score),
                                          mean = mean(Body_score), sd = sd(Body_score), range = range(Body_score))

wilcox.test(data = dat_adult, Body_score ~ Sex) # non-parametric

Score_df <- dat %>% group_by(Sex, Age) %>% summarise(score = mean(Body_score, na.rm = T), sd = sd(Body_score, na.rm = T))

plot_score <- ggplot(Score_df, aes(x = Sex, y = score)) +
  geom_point(aes(color = Age), position = position_dodge(width = 0.5), shape = 15, size = 2) +
 
  geom_errorbar(data = Score_df %>% filter(Age == "Adult"),
                aes(ymin = score - sd, ymax = score + sd, color = Age),
                position = position_nudge(x = -0.125),  # Apply the same dodge for error bars
                width = 0.05, size = 0.75, alpha = 0.25) +
  
    labs(x = "", y = "Body score") +
  scale_color_manual(values = c("Adult" = "orange", "Sub-adult" = "darkgreen")) +
  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  labs(color = NULL) +
  theme(legend.position = "none", legend.title = element_blank())



# ------------------------------------------------------------------------------
# Head length and width

hist(dat_adult$Head_len)
shapiro.test(dat_adult$Head_len) # Normal

t.test(data = dat_adult, Head_len ~ Sex, var.equal = T)
cohen.d(data = dat_adult, Head_len ~ Sex, var.equal = TRUE)


hist(dat_adult$Head_wid)
shapiro.test(dat_adult$Head_wid) # Normal
t.test(data = dat_adult, Head_wid ~ Sex, var.equal = T)
cohen.d(data = dat_adult, Head_wid ~ Sex, var.equal = TRUE)

dat_adult %>% group_by(Sex) %>% summarise(HeadLen = mean(Head_len, na.rm = TRUE), sd_len = sd(Head_len, na.rm = TRUE), 
                                          rangeL = range(Head_len, na.rm = TRUE),
                                          HeadWid = mean(Head_wid, na.rm = TRUE), sd_wid = sd(Head_wid, na.rm = TRUE),
                                          rangeW = range(Head_wid, na.rm = TRUE))

head_df <- dat %>% group_by(Sex, Age) %>% summarise(HeadLen = mean(Head_len, na.rm = TRUE), sd_len = sd(Head_len, na.rm = TRUE),
                                                  HeadWid = mean(Head_wid, na.rm = TRUE), sd_wid = sd(Head_wid, na.rm = TRUE))
library("reshape2")

plot_headLen <- ggplot(head_df, aes(x = Sex)) +
  geom_point(aes(y = HeadLen, color = Age),  position = position_dodge(width = 0.5), shape = 15, size = 2) +
  
  geom_errorbar(data = head_df %>% filter(Age == "Adult"),
                aes(ymin = HeadLen - sd_len, ymax = HeadLen + sd_len, color = Age),
                position = position_nudge(x = -0.125),  # Apply the same dodge for error bars
                width = 0.05, size = 0.75, alpha = 0.25) +
  
  
  labs(x = "", y = "Head length (mm)") +
  
  scale_color_manual(values = c("Adult" = "orange", "Sub-adult" = "darkgreen")) +
  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  
  theme(legend.position = "none", legend.title = element_blank())

plot_headWid <- ggplot(head_df, aes(x = Sex)) + 
  geom_point(aes(y = HeadWid, color = Age),  position = position_dodge(width = 0.5), shape = 15, size = 2) +
 
  geom_errorbar(data = head_df %>% filter(Age == "Adult"),
                aes(ymin = HeadWid - sd_wid, ymax = HeadWid + sd_wid, color = Age),
                position = position_nudge(x = -0.125),  # Apply the same dodge for error bars
                width = 0.05, size = 0.75, alpha = 0.25) +
  
    labs(x = "", y = "Head width (mm)") +
  scale_color_manual(values = c("Adult" = "orange", "Sub-adult" = "darkgreen")) +
  
  theme_bw() +
  theme(panel.grid = element_blank()) +
  
  labs(color = NULL) +
  theme(legend.position = "none", legend.title = element_blank())



# ------------------------------------------------------------------------------
# Plotting legend

plot_score_legend <- ggplot(Score_df, aes(x = Sex, y = score)) +
  geom_point(aes(color = Age), position = position_dodge(width = 0.5), shape = 15, size = 2) +
  geom_errorbar(aes(ymin = score - sd, ymax = score + sd, color = Age),
                position = position_dodge(width = 0.5), width = 0.05,  size = 0.75, alpha = 0.25) +
  labs(x = "", y = "Body score") +
  scale_color_manual(values = c("Adult" = "orange", "Sub-adult" = "darkgreen")) +
  
  theme_classic() +
labs(color = NULL) +
  theme(legend.margin = margin(r = 10, l = 10))

# Plotting only legend
library("cowplot")

morph_legend <- get_legend(plot_score_legend)


# ------------------------------------------------------------------------------
# Plotting all four plots
morpho_plots <- plot_grid(plot_score, plot_mass, plot_headLen, plot_headWid, morph_legend,
                       nrow = 1, ncol = 5,
                       labels = c('A.', 'B.', 'C.', 'D.', ''),
                       label_size = 10)

# Export plot
ggsave(filename = "./Output/Morphometric_plots.jpg", plot = morpho_plots, 
       height = 3, width = 10, dpi = 200)

6/19
# ------------------------------------------------------------------------------

# Count females having pouch
dat_adult %>% filter(Sex == "Female") %>% count(Pouch) %>% mutate(Percent = n / sum(n) * 100)

View(dat_adult)
names(dat)

# Consider the age in the same month 'August' to maintain consistency
dat %>% filter(Pouch == "Young") %>% count(`J_age_aug`) %>% mutate(Percent = n / sum(n) * 100)
dat %>% filter(Pouch == "Young") %>% count(`J_age_aug`) %>% summarise(JAge = mean(J_age_aug, na.rm = TRUE),
                                                                      sd = sd(J_age_aug, na.rm = TRUE),
                                                                      range = range(J_age_aug, na.rm = TRUE))

dat %>% filter(Pouch == "Young") %>% count(`Joey_sex`) %>% mutate(Percent = n / sum(n) * 100)

dat %>% filter(Pouch == "Young") %>% count(Birth_month) %>% mutate(Percent = n / sum(n) * 100)


# ------------------------------------------------------------------------------
# Eye
names(dat)
dat %>% count(`L_eye`) %>% mutate(Percent = n / sum(n) * 100)
dat %>% count(`R_eye`) %>% mutate(Percent = n / sum(n) * 100)

12/42

# ------------------------------------------------------------------------------
dat %>% count(`Rump_col`) %>% mutate(Percent = n / sum(n) * 100)
dat %>% count(`Rump_wetness`) %>% mutate(Percent = n / sum(n) * 100)

# ------------------------------------------------------------------------------
dat %>% group_by(Sex) %>% count(Chlamydia_PCR) %>% mutate(Percent = n / sum(n) * 100)

# ------------------------------------------------------------------------------
# Counting tree species

TreeSpecies <- dat %>% filter(!is.na(Tree_species)) %>%  
  count(Tree_species) %>% mutate(Percent = n / sum(n) * 100)

Tree_plot <- ggplot(TreeSpecies, aes(x = Tree_species, y = n)) +
  geom_bar(stat = "identity", fill = "grey", col = "gold") +
  xlab("Tree species") +
  ylab("Number of koalas") +
  geom_text(aes(label = paste0(sprintf("%.1f", Percent), "%"), y = n), vjust = -0.5, col = "black", size = 2.75) +
theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Export plot
ggsave(filename = "./Output/Tree_plot.jpg", plot = Tree_plot, 
       height = 5.5, width = 4.5, dpi = 200)

# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------
