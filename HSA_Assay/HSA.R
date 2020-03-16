# laden von biotech
library(devtools)
load_all(path = "../../Biotech")
# Es werden die Ergebnisse zweier unabhängi von einander durchgeführten
# nicht-kompetitven ELISAs errechnet und verglichen

#### Clara's ELISA ################################################

# Laden des Datensatz

library(readxl)
HSA <- read_excel("HSA.xlsx", sheet = "Tabelle2")
summary(HSA)

# 2, 3, 4 sind die Standards
library(tidyverse)
HSA <- mutate(HSA, Std=((HSA$s2 + HSA$s3 + HSA$s4) / 3))

HSA.2 <- mutate( HSA, Std=((HSA[,3] + HSA[,4] + HSA[,5]) / 3) )


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
Patient.1 <- (sum(HSA[1:2, 11], HSA[1:2, 12]) / 4) * 20000

Patient.2 <- (sum(HSA[4:5, 11], HSA[4:5, 12]) /4) * 100000

# Berechnung der Patienten-Proben-Konzentration

cHSA(Patient.1)
cHSA(Patient.2)

# Untersuchung der Verteilung der Messwerte

boxplot(conc.1, conc.2, Patient.1, Patient.2)

###### ARNO's ELISA ###############################################
HSA.A <- read_excel("HSA_Arno.xlsx", sheet = "Tabelle2")
summary(HSA)

# 2, 3, 4 sind die Standards
HSA.A <- mutate(HSA.A, Std=((HSA.A$s2 + HSA.A$s3 + HSA.A$s4) / 3))
# Die Spalten 5&6, 7&8 sowie 9&10 führen neben einander die 
# Probenabsorption
Proben.1.A <- c((HSA.A$s5 + HSA.A$s6)/2 , (HSA.A$s7[1:4]+HSA.A$s8[1:4])/2)
Proben.2.A <- c((HSA.A$s9 + HSA.A$s10)/2 , (HSA.A$s7[5:8]+HSA.A$s8[5:8])/2)
# Es ist bekannt, dass Proben.1 1:5000 verdünnt wurde:
Proben.1.A <- Proben.1.A * 5000
# Proben.2 wurde 1:1000 Verdünnt
Proben.2.A <- Proben.2.A * 1000

# Plot der Regression
plot_regression(abs = HSA.A$Std[2:8], conc = subs[2:8])

# Definition einer einfachen Funktion
cHSA.A <- function (abs.P) {
    conc_eval( abs.P, abs_std = HSA.A$Std, conc_std = subs)
}

# Testen der Funktion
conc.1.A <- cHSA(abs.P = Proben.1.A)

# Berechnung der Konzentrationen
conc.2.A <- cHSA(Proben.2.A) 

# Untersuchung der Patienten-Proben
Patient.1.A <- (sum(HSA.A[1:2, 11], HSA.A[1:2, 12]) / 4) * 20000

Patient.2.A <- (sum(HSA.A[4:5, 11], HSA.A[4:5, 12]) /4) * 100000 

# Berechnung der Patienten-Proben-Konzentration

cHSA(Patient.1.A)
cHSA(Patient.2.A)

####### Vergleich der Ergebnisse ##################################

# als erstes sollten wir überprüfen, dass es sich tatsächlich um 
# zwei verschiedene Datensätze handelt
HSA == HSA.A

boxplot(Patient.1.A ,Patient.1, Patient.2.A, Patient.2)
boxplot(conc.1, conc.1.A)
boxplot(conc.2, conc.2.A)
boxplot(conc.1.A, conc.2.A)
hist(conc.2.A)

library(outliers)
outlier(conc.2.A)

# rm(list=ls())

