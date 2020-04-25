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

graf1_2 <- ggplot(data=povprecni_delez_dostopa, aes(x=drzava)) + geom_histogram() + 
           ylab('povprečje v %') + xlab('Države') + ggtitle('Povprečen delež dostopanja do interneta')

















# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
