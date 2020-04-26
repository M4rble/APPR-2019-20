# 3. faza: Vizualizacija podatkov

#slovar imen držav
slovar <- c("Belgium" = "Belgija",
            "Bulgaria" = "Bolgarija",
            "Czechia" = "Češka",
            "Denmark" = "Danska",
            "Germany" = "Nemčija",
            "Estonia" = "Estonija",
            "Ireland" = "Irska",
            "Greece" = "Grčija",
            "Spain" = "Španija",
            "France" = "Francija",
            "Croatia" = "Hrvaška",
            "Italy" = "Italija",
            "Cyprus" = "Ciper",
            "Latvia" = "Latvija",
            "Lithuania" = "Litva",
            "Luxembourg" = "Luksemburg",
            "Hungary" = "Madžarska",
            "Malta" = "Malta",
            "Netherlands" = "Nizozemska",
            "Austria" = "Avstrija",
            "Poland" = "Poljska",
            "Portugal" = "Portugalska",
            "Romania" = "Romunija",
            "Slovenia" = "Slovenija",
            "Slovakia" = "Slovaška",
            "Finland" = "Finska",
            "Sweden" = "Švedska",
            "United Kingdom" = "Združeno kraljestvo (Velika Britanija)",
            "Iceland" = "Islandija",
            "Norway" = "Norveška",
            "North Macedonia" = "Severna Makedonija",
            "Serbia" = "Srbija",
            "Turkey" = "Turčija")


# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
#zemljevid <- fortify(zemljevid)

#################################################################################

#uvoz vseh potrebnih datotek v obliki tidy data

source("uvoz/uvoz_podatkov.r", encoding="UTF-8")

graf1 <- ggplot(data=dostop_do_interneta, aes(x=leto, y=delez, col=drzava)) + geom_line() + 
         ylab('delež v %') + ggtitle('Dostop do interneta') + scale_x_continuous(breaks = 1*2007:2019) + 
         labs(fill = "Država")

povprecni_delez_dostopa <- dostop_do_interneta %>% 
                           group_by(drzava) %>%
                           summarise(povprecje = mean(delez, na.rm = TRUE))
povprecni_delez_dostopa$povprecje <- round(povprecni_delez_dostopa$povprecje, 2)

graf1a <- ggplot(data=povprecni_delez_dostopa, aes(x=drzava, y=povprecje, fill=drzava)) + 
          geom_col() + coord_cartesian(ylim = c(40, 100)) + ggtitle('Povprečen delež dostopanja do interneta') +
          ylab('povprečje v %') + xlab('Države') + theme(axis.text.x = element_blank())

najvecja_rast <- dostop_do_interneta %>%
                 group_by(drzava) %>%
                 summarise(razlika = (max(delez, na.rm = TRUE) - min(delez, na.rm = TRUE)))

primerjava_rasti <- dostop_do_interneta %>% 
                    filter(drzava %in% c("Islandija", "Nizozemska", "Norveška",
                                         "Bolgarija", "Turčija", "Grčija")) 

graf1b <- ggplot(data=primerjava_rasti, aes(x=leto, y=delez, col=drzava)) + geom_line() + 
          ylab('delež v %') + ggtitle('Primerjava rasti držav z največjo in najmanjšo rastjo') +
          scale_x_continuous(breaks = 1*2007:2019) +labs(fill = "Država")

povprecen_delez_razloga <- razlogi_za_ne_dostopanje_do_interneta %>%
                           group_by(razlog) %>%
                           summarise(sum(delez, na.rm = TRUE) / 132)
names(povprecen_delez_razloga)[names(povprecen_delez_razloga) == "sum(delez, na.rm = TRUE)/132"] <- "delez"
povprecen_delez_razloga$delez <- round(povprecen_delez_razloga$delez, 2)

razlogi_po_letih <- razlogi_za_ne_dostopanje_do_interneta %>%
                    filter(razlog %in% c("nepotreben", "pomanjkanje spretnosti", "predraga oprema")) %>%
                    group_by(leto, razlog) %>%
                    summarise(sum(delez, na.rm = TRUE) / 33)
names(razlogi_po_letih)[names(razlogi_po_letih) == "sum(delez, na.rm = TRUE)/33"] <- "delez"
razlogi_po_letih$delez <- round(razlogi_po_letih$delez, 2)

#graf2a <- ggplot(data=razlogi_po_letih, aes(x=))















# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
