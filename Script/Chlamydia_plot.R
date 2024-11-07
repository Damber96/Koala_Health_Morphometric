library("readxl")
library("VennDiagram")
library("tidyverse")

dat_chlamydia <- read_excel("./Data/Data_Chlamydia.xlsx")
str(dat_chlamydia)

dat_chlamydia$Overt_sign <- ifelse(dat_chlamydia$Reproductive_cyst == "Yes" |
                                     dat_chlamydia$Conjunctivitis == "Yes" |
                                     dat_chlamydia$Cystitis == "Yes", "Yes", "No")


# Create a list to represent the sets for the Venn diagram

# PCR and overt signs
venn_list1 <- list(
  Overt_sign = which(dat_chlamydia$Overt_sign == "Yes"),
  PCR = which(dat_chlamydia$PCR == "Yes"))

venn_labels1 <- c("Overt_sign" = "Overt signs",
                 "PCR" = "PCR")

venn_colors1 <- c("Overt_sign" = "red",
                "PCR" = "green")

plot1 <- venn.diagram(
  x = venn_list1,
  category.names = venn_labels1,
  filename = NULL,
  output = TRUE,
  imagetype = "png",
  fill = venn_colors1)
#  height = 800,  # Increase the height of the figure
 # width = 800)    # Increase the width of the figure

grid.draw(plot1)

# Only overt signs
venn_list2 <- list(
  Reproductive_cyst = which(dat_chlamydia$Reproductive_cyst == "Yes"),
  Conjunctivitis = which(dat_chlamydia$Conjunctivitis == "Yes"),
  Cystitis = which(dat_chlamydia$Cystitis == "Yes"))

venn_labels2 <- c("Reproductive_cyst" = "Reproductive Cysts",
                 "Conjunctivitis" = "Conjunctivitis",
                  "Cystitis" = "Cystitis")

venn_colors2 <- c("Reproductive_cyst" = "red",
                 "Conjunctivitis" = "blue",
                  "Cystitis" = "green")

# Plot the Venn diagram
plot2 <- venn.diagram(
  x = venn_list2,
  category.names = venn_labels2,
  filename = NULL,
  output = TRUE,
  imagetype = "png",
  fill = venn_colors2)


# Display the Venn diagram

grid.newpage()
grid.draw(plot2)


# ------------------------------------------------------------------------------
# All four signs
venn_list3 <- list(
          Reproductive_cyst = which(dat_chlamydia$Reproductive_cyst == "Yes"),
          Conjunctivitis = which(dat_chlamydia$Conjunctivitis == "Yes"),
          Cystitis = which(dat_chlamydia$Cystitis == "Yes"),
          PCR = which(dat_chlamydia$PCR == "Yes"))

venn_labels3 <- c("Reproductive_cyst" = "Reproductive Cysts",
                  "Conjunctivitis" = "Conjunctivitis",
                  "Cystitis" = "Cystitis",
                  "PCR" = "PCR")



# Plot the Venn diagram
plot3 <- venn.diagram(
  x = venn_list3,
  category.names = venn_labels3,
  filename = NULL,
  output = TRUE,
  imagetype = "png",
  col = "transparent",
  fill = c("cornflowerblue", "green", "yellow", "darkorchid1"),
  alpha = 0.4,
  label.col = c("black", "black", "black", "transparent", 
               "black", "transparent", "black", "black", "black", "black", 
               "black", "transparent", "transparent", "black", "transparent"),
  cex = 1.25,
  
  cat.col = c("darkblue", "darkgreen", "orange", "darkorchid4"),
  cat.fontface = "bold",
  cat.cex = 1.1,
  cat.pos = 0,
  cat.dist = 0.03,
  resolution = 300,
  units = 'px',
  )


# Display the Venn diagram

grid.newpage()
grid.draw(plot3)

# Export the ven diagram

png(file = "./Output/Venn_diagram.jpg", width = 9, height = 7.5, units = "in", res = 300)
grid.draw(plot3)
dev.off()

?png

