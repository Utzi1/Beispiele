# laden von biotech
library(devtools)
load_all(path = "../../Biotech")

# Laden des Datensatz
library(readxl)
HSA <- read_excel("HSA_Arno.xlsx", sheet = "Tabelle2")
summary(HSA)

# 2, 3, 4 sind die Standards
library(tidyverse)
HSA <- mutate(HSA, Std=((HSA$s2 + HSA$s3 + HSA$s4) / 3))
# Die Spalten 5&6, 7&8 sowie 9&10 führen neben einander die 
# Probenabsorption
Proben.1 <- c((HSA$s5 + HSA$s6)/2 , (HSA$s7[1:4]+HSA$s8[1:4])/2)
Proben.2 <- c((HSA$s9 + HSA$s10)/2 , (HSA$s7[5:8]+HSA$s8[5:8])/2)
# Es ist bekannt, dass Proben.1 1:5000 verdünnt wurde:
Proben.1 <- Proben.1 * 5000
# Proben.2 wurde 1:1000 Verdünnt
Proben.2 <- Proben.2 * 1000
# Substratkonzentration
subs <- seq(0, 50, length.out = length(HSA$Std))

# Plot der Regression
plot_regression(abs = HSA$Std[2:8], conc = subs[2:8])

# Definition einer einfachen Funktion
cHSA <- function (abs.P) {
    conc_eval( abs.P, abs_std = HSA$Std, conc_std = subs)
}

# Testen der Funktion
conc.1.1 <- conc_eval( abs_P = Proben.1, abs_std = HSA$Std , conc_std = subs )
conc.1 <- cHSA(abs.P = Proben.1)

conc.1 == conc.1.1

# Berechnung der Konzentrationen
conc.2 <- cHSA(Proben.2) 

# Untersuchung der Patienten-Proben
Patient.1 <- (sum(HSA[1:2, 11], HSA[1:2, 12]) / 4) * 20

Patient.2 <- (sum(HSA[4:5, 11], HSA[4:5, 12]) /4) * 100 

# Berechnung der Patienten-Proben-Konzentration

cHSA(Patient.1)
cHSA(Patient.2)

rm(list=ls())
