# Blood data : analysis

library("readxl")

data_blood <- read_excel("./Data/BloodData.xlsx")
head(data_blood)
ref_range <- data_blood[39, -c(1:6, 41:42)]
rownames(ref_range) <- "Ref_values"
View(ref_range)

dat_blood <- data_blood[1:38, ]

View(dat_blood)

dat_blood[, 7:42] <- as.data.frame(sapply(dat_blood[, 7:42], as.numeric))

str(dat_blood)

names(dat_blood)

shapiro.test(na.omit(log(dat_blood$SDMA))) 
shapiro.test(na.omit(log(dat_blood$Hb))) # Non-Normal
shapiro.test(log(dat_blood$Hct)) 
shapiro.test(log(dat_blood$MCHC)) # Non-Normal
hist(log(dat_blood$MCHC)) # Non-Normal

shapiro.test(log(dat_blood$Plat)) # Non-Normal
shapiro.test(log(dat_blood$NRBC)) 

shapiro.test(log(dat_blood$WBC)) 
shapiro.test(log(dat_blood$Neut))
shapiro.test(log(dat_blood$Lymp))

shapiro.test(dat_blood$Mono) # Non-Normal
hist(dat_blood$Mono) 

shapiro.test(dat_blood$Eos) # Non-Normal
hist(dat_blood$Eos)

shapiro.test(dat_blood$Baso) # Non-Normal
hist(dat_blood$Baso)


shapiro.test(dat_blood$Phosphate) # Non-Normal
hist(dat_blood$Phosphate)

shapiro.test(log(dat_blood$Sodium))
shapiro.test(log(dat_blood$Potassium))
shapiro.test(log(dat_blood$Chloride))
shapiro.test(log(dat_blood$Bicarbonate)) # Non-Normal
shapiro.test(log(dat_blood$`Anion_Gap`)) # Non-Normal

shapiro.test(log(dat_blood$Urea)) # Non-Normal
shapiro.test(log(dat_blood$Creatinine))
shapiro.test(log(dat_blood$Glucose)) # Non-Normal

shapiro.test(dat_blood$Bilirubin) # Exclude this from report

shapiro.test(log(dat_blood$AST)) # Non-Normal

shapiro.test(log(dat_blood$ALT)) # Non-Normal
shapiro.test(log(dat_blood$GGT)) # Non-Normal
shapiro.test(log(dat_blood$`Alkaline_Phosphates`))
shapiro.test(log(dat_blood$Protein))
shapiro.test(log(dat_blood$Albumin))
shapiro.test(log(dat_blood$Globulin))
shapiro.test(log(dat_blood$`Albumin_Globulin_Ratio`))
shapiro.test(log(dat_blood$Calcium))# Non-Normal

shapiro.test(log(dat_blood$Phosphate)) # Non-Normal
shapiro.test(log(dat_blood$`Cretine_Kinase`)) # Non-Normal
shapiro.test(log(dat_blood$Chlosterol))
shapiro.test(log(dat_blood$Triglyceride))



# -----------------------------------------------
library("Hmisc")

View(dat_blood)

# Transform into long format

dim(dat_blood)
dat_cell <- dat_blood[, 7:18]
dim(dat_cell)

jpeg(file="./Output/BloodCells.jpg", width = 1000, height = 1000, units = "px", res = 125)

hist.data.frame(dat_cell, nclass = 6)

dev.off()

dat_serum <- dat_blood[, c(19:34, 36:40)]
dim(dat_serum)

graphics.off()

jpeg(file="./Output/Serum_chemistry.jpg", width = 1000, height = 1000, units = "px", res = 125)

hist.data.frame(dat_serum, nclass = 6,na.big = T,
                xlab = "")

dev.off()


# -----------------------------
View(dat_blood_Male)

dat_blood$NL_ratio <- dat_blood$Neut/dat_blood$Lymp
str(dat_blood)

library(tidyverse)
dat_blood_Male <- dat_blood %>% filter(Sex == "M")
dat_blood_Fem <- dat_blood %>% filter(Sex == "F")

blood_mean <- dat_blood[, c(7:40, 43)] %>% 
  summarise_all(~mean(., na.rm = T), 2)

blood_sd <- dat_blood[, c(7:40, 43)] %>%
  summarise_all(~sd(., na.rm = T), 2)

blood_mean_male <- dat_blood_Male[, c(7:40, 43)] %>% 
summarise_all(~mean(., na.rm = T), 2)

blood_sd_male <- dat_blood_Male[, c(7:40, 43)] %>%
  summarise_all(~sd(., na.rm = T), 2)

blood_mean_fem <- dat_blood_Fem[, c(7:40, 43)] %>% 
summarise_all(~mean(., na.rm = T), 2)

blood_sd_fem <- dat_blood_Fem[, c(7:40, 43)] %>%
  summarise_all(~sd(., na.rm = T), 2)

blood_min <- dat_blood[, c(7:40, 43)] %>%
  summarise_all(~min(., na.rm = T), 2)

blood_max <- dat_blood[, c(7:40, 43)] %>%
  summarise_all(~max(., na.rm = T), 2)

blood_median <- dat_blood[, c(7:40, 43)] %>%
  summarise_all(~median(., na.rm = T), 2)

blood_median_male <- dat_blood_Male[, c(7:40, 43)] %>%
  summarise_all(~median(., na.rm = T), 2)

blood_median_fem <- dat_blood_Fem[, c(7:40, 43)] %>%
  summarise_all(~median(., na.rm = T), 2)

blood_iqr1 <- dat_blood[, c(7:40, 43)] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.25), 2)

blood_iqr3 <- dat_blood[, c(7:40, 43)] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.75), 2)

blood_iqr1_mal <- dat_blood_Male[, c(7:40, 43)] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.25), 2)

blood_iqr3_mal <- dat_blood_Male[, c(7:40, 43)] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.75), 2)

blood_iqr1_fem <- dat_blood_Fem[, c(7:40, 43)] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.25), 2)

blood_iqr3_fem <- dat_blood_Fem[, c(7:40, 43)] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.75), 2)

blood_summary <- as.data.frame(round(bind_rows(blood_mean, blood_sd, blood_mean_male, blood_sd_male,
                                               blood_mean_fem, blood_sd_fem, 
                                               blood_median, blood_iqr1, blood_iqr3, 
                                               blood_median_male, blood_iqr1_mal, blood_iqr3_mal,
                                               blood_median_fem, blood_iqr1_fem, blood_iqr3_fem,
                                               blood_min, blood_max), 2))

View(blood_summary)

rownames(blood_summary) <- c("Mean", "SD", "Mean_Male", "SD_Male",
                             "Mean_Fem", "SD_Fem", 
                             "Median", "IQR_1", "IQR_3",
                             "Median_Male", "IQR1_male", "IQR3_male", 
                             "Median_Fem", "IQR1_Fem", "IQR3_Fem",
                             "Range_min", "Range_max")

blood_summary_ref <-rbind (blood_summary, ref_range)

head(blood_summary_ref)

names(blood_summary)
names(ref_range)

# Export data

library("writexl")

write_xlsx(as.data.frame(blood_summary), path = "./Output/Blood_test_summary_Finals.xlsx")

# t-test
t.test(data = dat_blood, SDMA ~ c(Sex, var.equal = T))

# ------------------------------------------------------------------------------
# ANCOVA
library("MASS")


# Haematological parameters

aov_sdma <- aov(SDMA ~ Age_yr + Sex, data = dat_blood)
summary(aov_sdma)

glm_Hb <-  glm.nb(Hb ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(glm_Hb) # Males have high Hb

aov_Hct <- aov(Hct ~ Age_yr + Sex, data = dat_blood)
summary(aov_Hct) # Males have high Hct

str(dat_blood$MCHC)
glm_MCHC <-  glm.nb(MCHC ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(aov_MCHC)

glm_Plat <-  glm(as.integer(Plat) ~ Age_yr + Sex, data = dat_blood, family = "poisson") # non-normal
summary(glm_Plat) # Count data

dat_nrbc <- dat_blood #Remove outliers
dat_nrbc <- dat_nrbc %>% filter(NRBC <= 60)

shapiro.test(log(dat_nrbc$NRBC)) #Normal

glm_NRBC <- glm(as.integer(NRBC) ~ Age_yr + Sex, data = dat_nrbc, family = "poisson")
summary(glm_NRBC) # count data # NRBC increases with age, and low in Males

glm_WBC <- glm(as.integer(WBC) ~ Age_yr + Sex, data = dat_blood, family = "poisson")
summary(glm_WBC) # count data

dat_neut <- dat_blood #Remove outliers
dat_neut <- dat_neut %>% filter(Neut <= 5)

glm_Neut <- glm(as.integer(Neut) ~ Age_yr + Sex, data = dat_neut, family = "poisson")
summary(glm_Neut) # count data # Neutrophyl increases with age

glm_Lymp <- glm(as.integer(Lymp) ~ Age_yr + Sex, data = dat_blood, family = "poisson")
summary(glm_Lymp) # count data

shapiro.test(dat_blood$NL_ratio) # Non-normal
glm_nl <- glm.nb(NL_ratio ~ Age_yr + Sex, data = dat_blood) # right skewed
summary(glm_nl)

dat_mono <- dat_blood #Remove outliers
dat_mono <- dat_mono %>% filter(Mono <= 0.4)
shapiro.test(dat_mono$Mono)

glm_Mono <- glm.nb(Mono ~ Age_yr + Sex, data = dat_mono) # right skewed
summary(glm_Mono) # count data

glm_Eos <- glm.nb(Eos ~ Age_yr + Sex, data = dat_blood) #right skewed
summary(glm_Eos) # count data

glm_Baso <- glm.nb(Baso ~ Age_yr + Sex, data = dat_blood) #right skewed
summary(glm_Baso) # count data

str(dat_blood)
# Serum biochemestry
aov_sdma <- aov(SDMA ~ Age_yr + Sex, data = dat_blood, na.action = na.omit)
summary(aov_sdma)

glm_Phosphate <- glm.nb(Phosphate ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(glm_Phosphate) # Phosphate increases with age

aov_Sodium <- aov(Sodium ~ Age_yr + Sex, data = dat_blood)
summary(aov_Sodium)

aov_Potassium <- aov(Potassium ~ Age_yr + Sex, data = dat_blood)
summary(aov_Potassium)

aov_Chloride <- aov(Chloride ~ Age_yr + Sex, data = dat_blood)
summary(aov_Chloride)

glm_Bicarbonate <- glm.nb(Bicarbonate ~ Age_yr + Sex, data = dat_blood)
summary(glm_Bicarbonate)

glm_Anion_Gap <- glm.nb(Anion_Gap ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(glm_Anion_Gap)

glm_Urea <- glm.nb(Urea ~ Age_yr + Sex, data = dat_blood)
summary(glm_Urea)

aov_Creatinine <- aov(Creatinine ~ Age_yr + Sex, data = dat_blood)
summary(aov_Creatinine)

glm_Glucose <- glm.nb(Glucose ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(glm_Glucose)

aov_Bilirubin <- aov(Bilirubin ~ Age_yr + Sex, data = dat_blood)
summary(aov_Bilirubin)

dat_AST <- dat_blood #Remove outliers
dat_AST <- dat_AST %>% filter(AST <= 50)
shapiro.test(dat_AST$AST)

glm_AST <- glm.nb(AST ~ Age_yr + Sex, data = dat_AST) # non-normal
summary(glm_AST) # High Ast in Females

aov_ALT <- glm.nb(as.integer(ALT) ~ Age_yr + Sex, data = dat_blood) 
summary(aov_ALT)

aov_GGT <- glm.nb(as.integer(GGT) ~ Age_yr + Sex, data = dat_blood)
summary(aov_GGT)

aov_Alkaline_Phosphates <- glm(as.integer(Alkaline_Phosphates) ~ Age_yr + Sex, data = dat_blood)
summary(aov_Alkaline_Phosphates) # Decreases with age

aov_Protein <- aov(Protein ~ Age_yr + Sex, data = dat_blood)
summary(aov_Protein) # Protein increase with age

aov_Albumin <- aov(Albumin ~ Age_yr + Sex, data = dat_blood)
summary(aov_Albumin)

aov_Globulin <- aov(Globulin ~ Age_yr + Sex, data = dat_blood)
summary(aov_Globulin) # Globulin increases with age

glm_Calcium <- glm.nb(Calcium ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(glm_Calcium) # High in males

glm_Phosphate <- glm.nb(Phosphate ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(glm_Phosphate) # Increases with age

glm_Cretine_Kinase <- glm.nb(Cretine_Kinase ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(glm_Cretine_Kinase)

aov_Chlosterol <- aov(Chlosterol ~ Age_yr + Sex, data = dat_blood)
summary(aov_Chlosterol)

aov_Triglyceride <- aov(Triglyceride ~ Age_yr + Sex, data = dat_blood)
summary(aov_Triglyceride) # Increases with age

aov_albuGlob <- aov(Albumin_Globulin_Ratio ~ Age_yr + Sex, data = dat_blood) # non-normal
summary(aov_albuGlob)



