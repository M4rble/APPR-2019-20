library(readr)
library(tidyr)
library(dplyr)

dostop_do_interneta <- read_csv("dostop_do_internta.csv", col_names=c("Država", 2007:2018),
                                skip=1, na="-", locale=locale(encoding="Windows-1250"))

razlogi_za_ne_dostopanje_do_interneta <- read_csv("razlogi.csv", col_names=c("Država", "previsoki stroški dostopa", 
                                               "dostop drugje", "previsoki stroški opreme", "ne potrebujejo dostopa",
                                               "skrb za varnost", "pomanjkanje sposobnosti", "drugo"),
                                               skip=1, na="-", locale=locale(encoding="Windows-1250"))
                                               