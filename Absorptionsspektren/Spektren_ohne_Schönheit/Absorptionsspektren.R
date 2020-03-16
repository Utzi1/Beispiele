# laden der benötigten Pakete
library(readxl)
# Laden des Datesatz
Spektrum <- read_xlsx("Absoprtionspektrum210-600nm.xlsx")

# Ein einfacher plot:
plot(Spektrum$`Probe 1` ~ Spektrum$nm, type = "l")

# wir können erkennen, dass auf dem Spektrum gerade am Anfang
# Fehlermessungen auftreten, diese werden entfernt:
library(tidyverse)
Spektrum <- slice(Spektrum, 20:391)

# noch man plotten, dieses mal mit ggplot:
ggplot()+
    geom_line(aes(y = Spektrum$`Probe 1` , x = Spektrum$nm))+
    xlab("Wellenlänge in nm")+
    ylab("Absorption")+
    theme_minimal()
# nun wollen wir mal sehen ob wir auch zwei oder mehr Spektren in einen Plot bekommen
library(readr)
Spektren <- read.csv2("~/Bachelor/Paket_BHT/Beispielauswertungen/Absorptionsspektren/absspkrt.csv")

#wollen wir mal sehen, wie die Daten strukturiert sind
str(Spektren)

# Hier hat vor allem der Datenimport schwierigkeiten gemacht, das muss sich ändern!
# Besonders unvorteilhaft ist das problem mit dem Komma...  
  
length(Spektren$`Wavelength (nm)`)

# nun selektieren wir die für uns interessanten Spalten
# und überführen sie in "dbl"

Spektren <- tibble(
                   nm = Spektren[2:111,]$Wavelength..nm. ,
                   Abs.1 = Spektren[2:111,]$Abs,
                   Abs.2 = Spektren[2:111,]$Abs.1
)

ggplot()+
  geom_line(mapping = aes( x = nm, y = Abs.1), data = Spektren, color = "red")+
  geom_line(mapping = aes( x = nm, y = Abs.2), data = Spektren, color = "blue")

