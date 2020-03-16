# Laden der Standardreih# Die Werte, welche als Grundlage für die Standardreihe verwendet werden wurden
# hierbei noch verändert, es wird angenommen, dass die Spalte MW Werte auslässt, 
# welche den Mittelwert zu stark verfälschene
library(readxl)
STR1 <- read_excel("~/Bachelor/Paket_BHT/Beispielauswertungen/progesteron_Assay/STR1.xlsx")
str(STR1)

# Laden von Biotech
library(devtools)
load_all("../../Biotech")

# 
library(tidyverse)

# Errechnen der Mittelwerte
STR1 <- mutate(STR1, Std = (STR1$E1 + STR1$E2 + STR1$E3)/3)

# Vergleich der Mittelwerte aus den Standards und den von der Gruppe errechneten Werten:
tibble(
        STR1$MW == STR1$Std,
        STR1$MW, 
        STR1$Std
)
# Die Werte, welche als Grundlage für die Standardreihe verwendet werden wurden
# hierbei noch verändert, es wird angenommen, dass die Spalte MW Werte auslässt, 
# welche den Mittelwert zu stark verfälschen
dose_response_plot(STR1$conc,STR1$Std)
dose_response_plot(STR1$conc,STR1$MW)
# Das nicht-lineare Modell:
library(dr4pl)
fit <-  dr4pl(STR1$conc[1:8]~STR1$MW[1:8])
conc.eval.DR <- function (abs.P) {
   fit$parameters[1] + ((fit$parameters[4] - fit$parameters[1])/(1 + (abs.P/fit$parameters[2])^fit$parameters[3]))
}
conc.eval.DR(STR1$Std)

# Export der generierten Daten 
write.csv(STR1, "./STR1.csv")
