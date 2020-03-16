library(tidyverse)
library(OutlierDetection)
library(readxl)
library(devtools)
load_all("../../Biotech")
Messdaten <- read_xlsx(path = "Diffusionskoeffizient.xlsx", sheet = "Tabelle2", col_names = TRUE)
CaSTR <- read_xlsx(path = "Diffusionskoeffizient.xlsx", sheet = "Tabelle3")
plot_regression(conc = CaSTR$c, abs =  CaSTR$lf)
# Berechnung der Stoffmengenkonzentration
c.mes <- conc_eval(abs_P = Messdaten$lf, conc_std = CaSTR$c, CaSTR$lf)
# Daraus Berechnung der Massenkonzentration 
M.CaCl <- 110.98
b.mes  <- c.mes * M.CaCl

# Berechnung der CaCl-Masse zu Beginn 
C.0 <- function (b.mes = NULL, V.ges = NULL, V.Alg = NULL) {
    (max(b.mes) * V.ges) / V.Alg
} 
C.0 <- C.0(b.mes = b.mes, V.ges = .075 + 0.00807, V.Alg = .00807)

# Berechnung der CaCl-Konzentration in den Alginatkugeln
C.t <- function (C.0, b.mes, V.lsg, V.Kugel) {
    # Konzentration in der Lösung
    N.lsg <- b.mes * V.lsg
    # Berechnung der Konzentration in den Kugeln
    N.Kugel <- (rep(C.0, length(b.mes))-N.lsg)/V.Kugel 
    return(list(N.lsg, N.Kugel))
}
C.t <- C.t(
    C.0,
    b.mes,
    .075,
    .00805
)
ggplot()+
  geom_point(mapping = aes(x = Messdaten$MW, y = C.t[[2]]))

