# Blood data : analysis

library("readxl")

data_blood <- read_excel("./Data/BloodData.xlsx")
View(data_blood)
ref_range <- data_blood[32, -c(1:6, 41:42)]
rownames(ref_range) <- "Ref_values"
View(ref_range)

dat_blood <- data_blood[1:31, ]

View(dat_blood)

dat_blood[, 7:42] <- as.data.frame(sapply(dat_blood[, 7:42], as.numeric))

str(dat_blood)

names(dat_blood)

shapiro.test(na.omit(log(dat_blood$SDMA))) 
shapiro.test(log(dat_blood$Hct)) 
shapiro.test(log(dat_blood$MCHC)) # Non-Normal   ######
hist(log(dat_blood$MCHC)) # Non-Normal   ######

shapiro.test(log(dat_blood$Plat)) 
shapiro.test(log(dat_blood$NRBC)) 

shapiro.test(log(dat_blood$WBC)) 
shapiro.test(log(dat_blood$Neut))
shapiro.test(log(dat_blood$Lymp))

shapiro.test(dat_blood$Mono) # Non-Normal   ######
hist(dat_blood$Mono) 

shapiro.test(dat_blood$Eos) # Non-Normal   ######
hist(dat_blood$Eos)

shapiro.test(dat_blood$Baso) # Non-Normal   ######
hist(dat_blood$Baso)


shapiro.test(dat_blood$Phosphate) # Non-Normal   ######
hist(dat_blood$Phosphate)

shapiro.test(log(dat_blood$Sodium))
shapiro.test(log(dat_blood$Potassium))
shapiro.test(log(dat_blood$Chloride))
shapiro.test(log(dat_blood$Bicarbonate))
shapiro.test(log(dat_blood$`Anion_Gap`)) # Non-Normal   ######
hist(log(dat_blood$`Anion_Gap`))

shapiro.test(log(dat_blood$Urea))
shapiro.test(log(dat_blood$Creatinine))
shapiro.test(log(dat_blood$Glucose)) # Non-Normal   ######
hist(log(dat_blood$Glucose))

shapiro.test(dat_blood$Bilirubin) # Exclude this from report

shapiro.test(log(dat_blood$AST)) # Non-Normal   ######
hist(log(dat_blood$AST))

shapiro.test(log(dat_blood$ALT))
shapiro.test(log(dat_blood$GGT))
shapiro.test(log(dat_blood$`Alkaline_Phosphates`))
shapiro.test(log(dat_blood$Protein))
shapiro.test(log(dat_blood$Albumin))
shapiro.test(log(dat_blood$Globulin))
shapiro.test(log(dat_blood$`Albumin_Globulin_Ratio`))
shapiro.test(log(dat_blood$Calcium)) # Non-Normal   ######
hist(log(dat_blood$Calcium))

shapiro.test(log(dat_blood$Phosphate)) # Non-Normal   ######
hist(log(dat_blood$Phosphate))

shapiro.test(log(dat_blood$`Cretine_Kinase`)) # Non-Normal   ######
hist(log(dat_blood$`Cretine_Kinase`))

shapiro.test(log(dat_blood$Chlosterol))
shapiro.test(log(dat_blood$Triglyceride))



# -----------------------------------------------
library("Hmisc")

View(dat_blood)

# Transform into long format

dim(dat_blood)
dat_cell <- dat_blood[, 7:18]
dim(dat_cell)

jpeg(file="./Output/BloodCell.jpg", width = 1000, height = 1000, units = "px", res = 125)

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
dat_blood

library(tidyverse)

blood_mean <- dat_blood[, 7:40] %>%
      summarise_all(~mean(., na.rm = T), 2)
blood_sd <- dat_blood[, 7:40] %>%
  summarise_all(~sd(., na.rm = T), 2)
blood_min <- dat_blood[, 7:40] %>%
  summarise_all(~min(., na.rm = T), 2)
blood_max <- dat_blood[, 7:40] %>%
  summarise_all(~max(., na.rm = T), 2)
blood_median <- dat_blood[, 7:40] %>%
  summarise_all(~median(., na.rm = T), 2)
blood_iqr1 <- dat_blood[, 7:40] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.25), 2)
blood_iqr3 <- dat_blood[, 7:40] %>%
  summarise_all(~quantile(., na.rm = T, probs = 0.75), 2)


?summarise

blood_summary <- as.data.frame(round(bind_rows(blood_mean, blood_sd, blood_min, blood_max,
                                               blood_median, blood_iqr1, blood_iqr3), 2))

rownames(blood_summary) <- c("Mean", "SD", "Range_min", "Range_max", "Median", "IQR_1", "IQR_3")

blood_summary_ref <-rbind (blood_summary, ref_range)

names(blood_summary)
names(ref_range)

# Export data

library("writexl")

write_xlsx(blood_summary_ref, path = "./Output/Blood_test_summary.xlsx")

# t-test
t.test(data = dat_blood, SDMA ~ c(Sex, var.equal = T))

# ------------------------------------------------------------------------------
# Regression
library("MuMIn")


# Family selection (family = )
gaussian(link = "identity") # For normal distribution data with continuous dependent variable
binomial(link = "logit") # for binary response variable
poisson(link = "log") # Suitable for count variable
gamma(link = "log") # Suitable for right-skewed (also left-skewed) variable
inverse.gaussian(link = "1/mu^2") # Suitable for right-skewed (also left-skewed) variable (I have fitted this)


glm_sdma <- glm(SDMA ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
options('na.action' = 'na.omit')

dredged_sdma <- dredge(glm_sdma)

summary(glm_sdma)

glm_Hb <- glm(Hb ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Hb) # Males have high Hb

glm_Hct <- glm(Hct ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Hct) # Males have high Hct

glm_MCHC <- glm(MCHC ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_MCHC)

glm_Plat <- glm(Plat ~ Age_yr + Sex, data = dat_blood, family = "poisson")
options('na.action' = 'na.fail')

plat_poisson <- anova(glm_Plat, test = "Chi")
print(plat_poisson)

dred_plat <- dredge(glm_Plat) # Count data

get.models(dred_plat, subset = delta < 2)


glm_NRBC <- glm(NRBC ~ Age_yr + Sex, data = dat_blood, family = poisson(link = "log"))
summary(glm_NRBC) # count data # NRBC increases with age, and low in Males

glm_WBC <- glm(WBC ~ Age_yr + Sex, data = dat_blood, family = poisson(link = "log"))
summary(glm_WBC) # count data

glm_Neut <- glm.nb(Neut ~ Age_yr + Sex, data = dat_blood)
summary(glm_Neut) # count data # Neutrophyl increases with age
neut_poisson <- anova(glm_Neut, test = "Chi")
print(neut_poisson)

glm_Lymp <- glm(Lymp ~ Age_yr + Sex, data = dat_blood, family = poisson(link = "log"))
summary(glm_Lymp) # count data

library("MASS")

glm_Mono <- glm.nb(Mono ~ Age_yr + Sex, data = dat_blood) # This fits the family = negative.binomial(link = "log")) #right skewed
summary(glm_Mono) # count data

glm_Eos <- glm.nb(Eos ~ Age_yr + Sex, data = dat_blood) #right skewed
summary(glm_Eos) # count data

glm_Baso <- glm.nb(Baso ~ Age_yr + Sex, data = dat_blood) #right skewed
summary(glm_Baso) # count data

glm_Phosphate <- glm(Phosphate ~ Age_yr + Sex, data = dat_blood, family = inverse.gaussian(link = "1/mu^2")) # non-normal
summary(glm_Phosphate) # Phosphate increases with age

glm_Sodium <- glm(Sodium ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Sodium)

glm_Potassium <- glm(Potassium ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Potassium)

glm_Chloride <- glm(Chloride ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Chloride)

glm_Bicarbonate <- glm(Bicarbonate ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Bicarbonate)

glm_Anion_Gap <- glm(Anion_Gap ~ Age_yr + Sex, data = dat_blood, family = inverse.gaussian(link = "1/mu^2")) # non-normal
summary(glm_Anion_Gap)

glm_Urea <- glm(Urea ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Urea)

glm_Creatinine <- glm(Creatinine ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Creatinine)

glm_Glucose <- glm(Glucose ~ Age_yr + Sex, data = dat_blood, family = inverse.gaussian(link = "1/mu^2")) # non-normal
summary(glm_Glucose)

glm_Bilirubin <- glm(Bilirubin ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Bilirubin)

glm_AST <- glm(AST ~ Age_yr + Sex, data = dat_blood, family = inverse.gaussian(link = "1/mu^2")) # non-normal
summary(glm_AST) # High Ast in Females

glm_ALT <- glm(ALT ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity")) 
summary(glm_ALT)

glm_GGT <- glm(GGT ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_GGT)

glm_Alkaline_Phosphates <- glm(Alkaline_Phosphates ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Alkaline_Phosphates) # Decreases with age

glm_Protein <- glm(Protein ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Protein) # Protein increase with age

glm_Albumin <- glm(Albumin ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Albumin)

glm_Globulin <- glm(Globulin ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Globulin) # Globulin increases with age

glm_Calcium <- glm(Calcium ~ Age_yr + Sex, data = dat_blood, family = inverse.gaussian(link = "1/mu^2")) # non-normal
summary(glm_Calcium) # High in males

glm_Phosphate <- glm(Phosphate ~ Age_yr + Sex, data = dat_blood, family = inverse.gaussian(link = "1/mu^2")) # non-normal
summary(glm_Phosphate) # Increases with age

glm_Cretine_Kinase <- glm(Cretine_Kinase ~ Age_yr + Sex, data = dat_blood, family = inverse.gaussian(link = "1/mu^2")) # non-normal
summary(glm_Cretine_Kinase)

glm_Chlosterol <- glm(Chlosterol ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Chlosterol)

glm_Triglyceride <- glm(Triglyceride ~ Age_yr + Sex, data = dat_blood, family = gaussian(link = "identity"))
summary(glm_Triglyceride) # Increases with age


# Generalized Linear Model was employed to analyse the effect of age and sex on blood parameters.
# Models were fitted with following families considerint their distribution and variable types
gaussian(link = "identity") # For normal distribution data with continuous dependent variable
inverse.gaussian(link = "1/mu^2") # Suitable for right-skewed (also left-skewed) variable (I have fitted this)
poisson(link = "log") # For count data (normal)



# ------------------------
str(blood_plots)
