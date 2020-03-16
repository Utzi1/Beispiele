library(magrittr)
# Vektor der Verdünnungsschritte:
dilfac <- c( 1, 100, 1000, 10000 )
# umrechnen in amount:
amount <- 1/dilfac
# Ladend der lib
library("MPN")
# Ein paar positive Rörchen
tubes.pos <- c(5,3,1,0)
# die Gesamtzahl der Röhrchen
tubes.tot <- c(12, 10, 8, 6)
mpn(
    positive = tubes.pos,
    tubes = tubes.tot,
    amount = amount
) %>%
print()

