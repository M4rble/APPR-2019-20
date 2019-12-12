# Podatki

Tukaj bomo zbirali vse podatke (datoteke v obliki CSV, XML, ...), ki jih bo naš
program uvozil.


library(readr)
library(tidyr)
library(dplyr)

dostop_do_interneta <- read_csv("dostop_do_interneta.csv", col_names=c("Država", 2008:2019),
              skip=1, na="-", locale=locale(encoding="Windows-1250"))
              