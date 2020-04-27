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
          scale_x_continuous(breaks = 1*2007:2019) + labs(fill = "Država")

povprecen_delez_razloga <- razlogi_za_ne_dostopanje_do_interneta %>%
                            group_by(razlog) %>%
                            summarise(mean(delez, na.rm = TRUE))
names(povprecen_delez_razloga)[names(povprecen_delez_razloga) == "mean(delez, na.rm = TRUE)"] <- "delez"
povprecen_delez_razloga$delez <- round(povprecen_delez_razloga$delez, 2)

graf2a <- ggplot(data=povprecen_delez_razloga, aes(x=razlog, y=delez, fill=razlog)) + geom_col() +
          ggtitle('Povprečen delež razlogov za nedostopanje do interneta') + 
          ylab('delež v %') + xlab('razlog') + theme(axis.text.x = element_blank())

razlogi_po_letih <- razlogi_za_ne_dostopanje_do_interneta %>%
                    filter(razlog %in% c("nepotreben", "pomanjkanje spretnosti", "predraga oprema")) %>%
                    group_by(leto, razlog) %>%
                    summarise(mean(delez, na.rm = TRUE))
names(razlogi_po_letih)[names(razlogi_po_letih) == "mean(delez, na.rm = TRUE)"] <- "delez"
razlogi_po_letih$delez <- round(razlogi_po_letih$delez, 2)

graf2b <- ggplot(data=razlogi_po_letih, aes(x=razlog)) + geom_col(aes(y=delez, fill=leto)) +
          ggtitle('Delež glavnih razlogov za nedostopanje do interneta po letih') + 
          ylab('deleži v %') + xlab('razlogi')

#razlogi_po_drzavah_v_letu_2019 <- razlogi_za_ne_dostopanje_do_interneta %>%
#                                  group_by(drzava, razlog) %>%
#                                  summarise(max(delez, na.rm = TRUE))
# graf 2c bomo vidli še kaj bo


povprecen_delez_aktivnosti <- internetne_aktivnosti %>%
                              group_by(uporaba) %>%
                              summarise(mean(delez, na.rm = TRUE))
names(povprecen_delez_aktivnosti)[names(povprecen_delez_aktivnosti) == "mean(delez, na.rm = TRUE)"] <- "delez"
povprecen_delez_aktivnosti$delez <- round(povprecen_delez_aktivnosti$delez, 2)

graf3a <- ggplot(data=povprecen_delez_aktivnosti, aes(x=uporaba, y=delez, fill=uporaba)) + geom_col() +
          ggtitle('Povprečen delež internetnih aktivnosti') + 
          ylab('delež v %') + xlab('aktivnost') + theme(axis.text.x = element_blank())

aktivnosti_po_letih <- internetne_aktivnosti %>%
                       filter(uporaba %in% c("branje novic", "posiljanje/prejemanje e-poste", 
                                            "poslusanje glasbe", 
                                            "pridobivanje informacij o proizvodih in storitvah",
                                            "uporaba aplikacij za izmenjavo sporocil (npr Messenger)",
                                            "uporaba socialnih omrezij")) %>%
                                            group_by(leto, uporaba) %>%
                                            summarise(mean(delez, na.rm = TRUE))
names(aktivnosti_po_letih)[names(aktivnosti_po_letih) == "mean(delez, na.rm = TRUE)"] <- "delez"
aktivnosti_po_letih$delez <- round(aktivnosti_po_letih$delez, 2)
aktivnosti_po_letih$delez[aktivnosti_po_letih$delez == "NaN"] <- "0"

#POPRAVI ŠE
graf3b <- ggplot(data=aktivnosti_po_letih, aes(x=uporaba, y=delez, fill=leto)) + geom_col() +
          ggtitle('Delež glavnih aktivnosti pri uporabi interneta') + 
          ylab('deleži v %') + xlab('aktivnost') 

#graf3c bomo vidl kaj bo, kot pri 2c

povprecno_znanje_drzav_po_letih <- digitalno_znanje %>%
                                   group_by(nivo.znanja, leto) %>%
                                   filter(skupina == "skupno posamezniki") %>%
                                   summarise(delez = mean(delez, na.rm = TRUE))
povprecno_znanje_drzav_po_letih$delez <- round(povprecno_znanje_drzav_po_letih$delez, 2)

graf4a <- ggplot(data=povprecno_znanje_drzav_po_letih, aes(x=nivo.znanja, y=delez, fill=leto)) + 
          geom_col(position = "dodge2") + ggtitle('Povprečna raven znanja državljanov EU po letih') + 
          ylab('deleži v %') + xlab('nivo znanja')

znanje_moskih <- digitalno_znanje %>%
                 group_by(nivo.znanja, leto) %>%
                 filter(skupina == "moski (16-74 let)") %>%
                 summarise(povprecje_moskih = mean(delez, na.rm = TRUE))
znanje_moskih$povprecje_moskih <- round(znanje_moskih$povprecje_moskih, 2)
names(znanje_moskih)[names(znanje_moskih) == "povprecje_moskih"] <- "povprecje moskih"
znanje_zensk <- digitalno_znanje %>%
                group_by(nivo.znanja, leto) %>%
                filter(skupina == "zenske (16-74 let)") %>%
                summarise(povprecje_zensk = mean(delez, na.rm = TRUE))
znanje_zensk$povprecje_zensk <- round(znanje_zensk$povprecje_zensk, 2)
names(znanje_zensk)[names(znanje_zensk) == "povprecje_zensk"] <- "povprecje zensk"
primerjava_znanja_moskih_in_zensk <- merge(znanje_moskih, znanje_zensk)









# Izračunamo povprečno velikost družine
#povprecja <- druzine %>% group_by(obcina) %>%
#  summarise(povprecje=sum(velikost.druzine * stevilo.druzin) / sum(stevilo.druzin))
