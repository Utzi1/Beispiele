# Laden der Standardreih# Die Werte, welche als Grundlage für die Standardreihe verwendet werden wurden
# hierbei noch verändert, es wird angenommen, dass die Spalte MW Werte auslässt, 
# welche den Mittelwert zu stark verfälschene
library(readxl)
STR1 <- read_excel("ProgesteronVK.xlsx", 
                            sheet = "Tabelle2", col_names = FALSE)
str(STR1)

# Laden von Biotech
library(devtools)
load_all("../../Biotech")

mean(STR1$...1)

Standards <- tibble(
  c(mean(STR1$...1), mean(STR1$...2), mean(STR1$...3), mean(STR1$...4), mean(STR1$...5), mean(STR1$...6), mean(STR1$...7), mean(STR1$...8), mean(STR1$...9), mean(STR1$...10))
)

# 
library(tidyverse)


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

