dostop_do_interneta <- read_csv("podatki/dostop_do_interneta.csv", 
                                col_names=c("leto", "drzava", "UNIT" ,"HHTYP","delez"),
                                skip=1, na="-", locale=locale(encoding="Windows-1250"))
dostop_do_interneta$UNIT <- NULL
dostop_do_interneta$HHTYP <- NULL
dostop_do_interneta$delez <- gsub(':', 'ni podatka', dostop_do_interneta$delez)
dostop_do_interneta$drzava[dostop_do_interneta$drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"

                                
razlogi_za_ne_dostopanje_do_interneta <- read_csv("podatki/razlogi.csv", col_names=c("razlog", "drzava", 
                                               "leto", "odstotki", "total", "delez"),
                                               skip=1, na="-", locale=locale(encoding="Windows-1250"))
razlogi_za_ne_dostopanje_do_interneta$odstotki <- NULL
razlogi_za_ne_dostopanje_do_interneta$total <- NULL
razlogi_za_ne_dostopanje_do_interneta$delez <- gsub(':', 'ni podatka', razlogi_za_ne_dostopanje_do_interneta$delez)
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because the access costs are too high (telephone, etc.)"] <- "previsoki stroski"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because the access and equipment costs are too high"] <- "predrag dostop"                                       
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because of access elsewhere"] <- "dostop drugje"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because the equipment costs are too high"] <- "predraga oprema"      
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because access not needed (content is not useful, not interesting, etc.)"] <- "nepotreben"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because of privacy or security concerns"] <- "varnostni razlogi"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because of lack of skills"] <- "pomanjkanje spretnosti"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to the internet at home, because broadband is not available in the area"] <- "na obmocju ni dostopa"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because access not wanted (content is harmful, etc.)"] <- "dostop ni zazeljen"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because of a physical disability"] <- "fizicne pomanjkljivosti"
razlogi_za_ne_dostopanje_do_interneta$razlog[razlogi_za_ne_dostopanje_do_interneta$razlog == "Households without access to internet at home, because of other reasons"] <- "drugo"
razlogi_za_ne_dostopanje_do_interneta$drzava[razlogi_za_ne_dostopanje_do_interneta$drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"

                           
                                               
komunikacija_posameznikov_z_drzavo <- read_csv("podatki/komunikacija_posameznikov_z_drzavo.csv", col_names=c("drzava", 2008:2018),
                                                  skip=1, na="-", locale=locale(encoding="Windows-1250"))

namen_uporabe_interneta_za_komunikacijo_z_drzavo <- read_csv("podatki/namen_uporabe_interneta_za_komunikacijo_z_drzavo.csv")

