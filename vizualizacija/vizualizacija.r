# 3. faza: Vizualizacija podatkov

#slovar imen držav
slovar <- c("Belgium" = "Belgija",
            "Bulgaria" = "Bolgarija",
            "Czechia" = "Ceska",
            "Denmark" = "Danska",
            "Germany" = "Nemcija",
            "Estonia" = "Estonija",
            "Ireland" = "Irska",
            "Greece" = "Grcija",
            "Spain" = "Spanija",
            "France" = "Francija",
            "Croatia" = "Hrvaska",
            "Italy" = "Italija",
            "Cyprus" = "Ciper",
            "Latvia" = "Latvija",
            "Lithuania" = "Litva",
            "Luxembourg" = "Luksemburg",
            "Hungary" = "Madzarska",
            "Malta" = "Malta",
            "Netherlands" = "Nizozemska",
            "Austria" = "Avstrija",
            "Poland" = "Poljska",
            "Portugal" = "Portugalska",
            "Romania" = "Romunija",
            "Slovenia" = "Slovenija",
            "Slovakia" = "Slovaska",
            "Finland" = "Finska",
            "Sweden" = "Svedska",
            "United Kingdom" = "Zdruzeno kraljestvo (Velika Britanija)",
            "Iceland" = "Islandija",
            "Norway" = "Norveska",
            "North Macedonia" = "Severna Makedonija",
            "Serbia" = "Srbija",
            "Turkey" = "Turcija")


# Uvozimo zemljevid.
#zemljevid <- uvozi.zemljevid("http://baza.fmf.uni-lj.si/OB.zip", "OB",
#                             pot.zemljevida="OB", encoding="Windows-1250")
#levels(zemljevid$OB_UIME) <- levels(zemljevid$OB_UIME) %>%
#  { gsub("Slovenskih", "Slov.", .) } %>% { gsub("-", " - ", .) }
#zemljevid$OB_UIME <- factor(zemljevid$OB_UIME, levels=levels(obcine$obcina))
#zemljevid <- fortify(zemljevid)

#################################################################################


graf1 <- ggplot(data=dostop_do_interneta, aes(x=leto, y=delez, col=drzava)) + geom_line()


















# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
