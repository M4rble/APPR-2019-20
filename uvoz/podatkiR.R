
dostop_do_interneta <- read_csv("podatki/dostop_do_internta.csv", col_names=c("drzava", 2007:2018),
                                skip=1, na="-", locale=locale(encoding="Windows-1250"))%>%
gather(key = leto, value = vrednost,-drzava)

razlogi_za_ne_dostopanje_do_interneta <- read_csv("podatki/razlogi.csv", col_names=c("drzava", "previsoki stroski dostopa", 
                                               "dostop drugje", "previsoki stroski opreme", "ne potrebujejo dostopa",
                                               "skrb za varnost", "pomanjkanje sposobnosti", "drugo"),
                                               skip=1, na="-", locale=locale(encoding="Windows-1250"))
                                               

komunikacija_posameznikov_z_drzavo <- read_csv("podatki/komunikacija_posameznikov_z_drzavo.csv", col_names=c("drzava", 2008:2018),
                                                  skip=1, na="-", locale=locale(encoding="Windows-1250"))

namen_uporabe_interneta_za_komunikacijo_z_drzavo <- read_csv("podatki/namen_uporabe_interneta_za_komunikacijo_z_drzavo.csv")
