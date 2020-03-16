library(tidyverse)
library(OutlierDetection)
library(readxl)
library(devtools)
load_all("../../Biotech")
Messdaten <- read_xlsx(path = "Diffusionskoeffizient.xlsx", sheet = "Tabelle2", col_names = TRUE, na = "")
CaSTR <- read_xlsx(path = "Diffusionskoeffizient.xlsx", sheet = "Tabelle3")
plot_regression(CaSTR$c, CaSTR$lf)
c.mes <- conc_eval(abs_P = Messdaten$lf, conc_std = CaSTR$c, CaSTR$lf)
plot(c.mes~Messdaten$t)
ggplot(mapping  = aes(y = c.mes, x = Messdaten$t))+
    geom_point()+
    geom_smooth(method = "")

