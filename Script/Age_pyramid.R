library("readxl")

data <- read_excel("./Data/Data_KoalaStatus_20230908.xlsx")

View(data)

dat <- data[-c(13,34), ] # Excluding recaptured koala (Kenny) and joey
names(dat)

dat$Sex_Num <- ifelse(dat$Sex == "Male", 0, 1)

counts <- aggregate(Sex_Num ~ Age + Sex, data = dat, FUN = length)


# --------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

# Counting males and females
dat %>% count(Sex) %>% mutate(Percent = n / sum(n) * 100)

# --------------------------------------------------------------------
# Sex ratio

chi_result <- chisq.test(table(dat$Sex))
chi_result$expected # Checking for Fisher Exact test (expected obs is >5 so, chi-square is a good fit)

# --------------------------------------------------------------------

Age_plot <- ggplot(counts, aes(x = Age, fill = Sex,
                 y = ifelse(test = Sex == "Male",
                            yes = -Sex_Num, no = Sex_Num))) + 
  geom_bar(stat = "identity") +
 coord_flip() +
  labs(x = "Age (Year)", y = "Number of koalas") +
  scale_x_continuous(breaks = c(0,3,6,9,12,15,18)) +
  scale_y_continuous(labels=c("-2" = "2", "-1" = "1",
                            "0" = "0", "1" = "1", "2" = "2")) +
  
  geom_text(aes(x = 16, y = -1.25, label = paste("Male = 16")), 
            hjust = 1, color = "black", size = 3.5) +
  
  geom_text(aes(x = 16, y = 2, label = paste("Female = 17")), 
            hjust = 1, color = "black", size = 3.5) +
  theme(legend.position = "none")
  
# Export plot
ggsave(filename = "./Output/Age_plot.jpg", plot = Age_plot, 
       height = 5.5, width = 4.5, dpi = 200)

# --------------------------------------------------------------------
# histogram of Body mass

ggplot(dat, aes(x=Mass_kg)) + geom_histogram(stat = "bin", bins = 10, col = "grey") +
  labs(x = "Body mass (kg)", y = "Counts") +
  geom_density(col = "red")

hist(dat$Mass_kg)
lines(density(dat$Mass_kg), col = 4, lwd = 2)

shapiro.test(dat$Mass_kg) # Distribution is normal (p > 0.05)
?t.test
t.test(data = dat, Mass_kg ~ Sex, var.equal = T)

# Compute effect size
# Cohen's d: This tells you how big or small the observed difference between groups is.
# A larger Cohen's d value suggests a more substantial effect.

library("effsize")
cohen.d(data = dat, Mass_kg ~ Sex, var.equal = TRUE)

# Sign of Cohen's d indicates the direction of the effect (i.e., whether Group A (Males)
# has a higher or lower mean than Group B - Females), while the magnitude quantifies the size of the effect.

# --------------------------------------------------------------------
  
hist(dat$Body_score)
lines(x = density(x = dat$Body_score), col = "red")
shapiro.test(dat$Body_score) # Non-normal
# --------------------------------------------------------------------

hist(dat$HeadLength_mm)
shapiro.test(dat$HeadLength_mm) # Normal
t.test(data = dat, HeadLength_mm ~ Sex, var.equal = T)
cohen.d(data = dat, HeadLength_mm ~ Sex, var.equal = TRUE)


hist(dat$HeadWidth_mm)
shapiro.test(dat$HeadWidth_mm) # Normal
t.test(data = dat, HeadWidth_mm ~ Sex, var.equal = T)
cohen.d(data = dat, HeadWidth_mm ~ Sex, var.equal = TRUE)

# --------------------------------------------------------------------
hist(dat$Age)
shapiro.test(log(dat$Age)) # Non-normal
t.test(data = dat, Age ~ Sex, var.equal = T)
cohen.d(data = dat, Age ~ Sex, var.equal = TRUE)


# --------------------------------------------------------------------
hist(dat$Body_score)
shapiro.test(log(dat$Body_score)) # Non-normal
var(dat$Body_score, na.rm = T)

wilcox.test(data = dat, Body_score ~ Sex)
t.test(data = dat, Body_score ~ Sex, var.equal = F)




# --------------------------------------------------------------------
dat %>% summarise(Ag = mean(Age), sd = sd(Age))

dat %>% group_by(Sex) %>% summarise(Ag = mean(Age), sd = sd(Age))
dat %>% group_by(Sex) %>% summarise(Ag = range(Age))

dat %>% summarise(Mass = mean(Mass_kg), sd = sd(Mass_kg))
dat %>% group_by(Sex) %>% summarise(Mass = mean(Mass_kg), sd = sd(Mass_kg))
dat %>% group_by(Sex) %>% summarise(Mass = range(Mass_kg))


dat %>% group_by(Sex) %>% summarise(Score = median(Body_score, na.rm = TRUE))
dat %>% group_by(Sex) %>% summarise(Body_score = range(Body_score, na.rm = T))


dat %>% group_by(Sex) %>% summarise(Score = mean(HeadLength_mm, na.rm = TRUE), sd = sd(HeadLength_mm, na.rm = TRUE))
dat %>% group_by(Sex) %>% summarise(HeadLength = range(HeadLength_mm, na.rm = T))


str(dat$HeadLength_mm)
dat %>% group_by(Sex) %>% summarise(Score = mean(HeadWidth_mm, na.rm = TRUE), sd = sd(HeadWidth_mm, na.rm = TRUE))
dat %>% group_by(Sex) %>% summarise(HeadWidth = range(HeadWidth_mm, na.rm = T))

# Count females having pouch
dat %>% filter(Sex == "Female") %>% count(Pouch) %>% mutate(Percent = n / sum(n) * 100)

# Consider the age in the same month 'August' to maintain consistency
dat %>% filter(Pouch == "Young") %>% count(`J_Age_Aug`) %>% mutate(Percent = n / sum(n) * 100)
dat %>% filter(Pouch == "Young") %>% count(`J_Age_Aug`) %>% summarise(JAge = mean(J_Age_Aug, na.rm = TRUE),
                                                                      sd = sd(J_Age_Aug, na.rm = TRUE))

dat %>% filter(Pouch == "Young") %>% count(`Joey_Sex`) %>% mutate(Percent = n / sum(n) * 100)

names(dat)

# --------------------------------------------------------------------
# Eye
dat %>% count(`Left Eye`) %>% mutate(Percent = n / sum(n) * 100)
dat %>% count(`Right Eye`) %>% mutate(Percent = n / sum(n) * 100)

# --------------------------------------------------------------------
dat %>% count(`Rump colour`) %>% mutate(Percent = n / sum(n) * 100)
dat %>% count(`Rump wetness`) %>% mutate(Percent = n / sum(n) * 100)

# --------------------------------------------------------------------
dat %>% group_by(Sex) %>% count(Chlamydia_PCR) %>% mutate(Percent = n / sum(n) * 100)

# --------------------------------------------------------------------
# Counting tree species
names(dat)

TreeSpecies <- dat %>% count(Tree_species) %>% mutate(Percent = n / sum(n) * 100)

Tree_plot <- ggplot(TreeSpecies, aes(x = Tree_species, y = n)) +
  geom_bar(stat = "identity", fill = "grey", col = "gold") +
  xlab("Species") +
  ylab("Count") +
  geom_text(aes(label = paste0(sprintf("%.1f", Percent), "%"), y = n), vjust = -0.5, col = "black", size = 2.75) +
theme(axis.text.x = element_text(angle = 45, hjust = 1))  +
  theme_bw()

# Export plot
ggsave(filename = "./Output/Tree_plot.jpg", plot = Tree_plot, 
       height = 5.5, width = 4.5, dpi = 200)

