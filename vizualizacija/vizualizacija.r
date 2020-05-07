# 3. faza: Vizualizacija podatkov

#################################################################################

#Uvozimo zemljevid

zemljevid <- uvozi.zemljevid(
  "http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip", "ne_50m_admin_0_countries", encoding="UTF-8")
zemljevid <- zemljevid[zemljevid$CONTINENT == "Europe",]

#################################################################################

graf1 <- ggplot(data=dostop_do_interneta %>% mutate(drzava=slovar[drzava]), aes(x=leto, y=delez, col=drzava)) + 
         geom_point() + geom_line() + 
         ylab('delež v %') + ggtitle('Dostop do interneta') + scale_x_continuous(breaks = 1*2007:2019) + 
         labs(fill = "Država") + theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))

povprecni_delez_dostopa <- dostop_do_interneta %>% 
                           group_by(drzava) %>%
                           summarise(povprecje = mean(delez, na.rm = TRUE))
povprecni_delez_dostopa$povprecje <- round(povprecni_delez_dostopa$povprecje, 2)

graf1a <- ggplot(data=povprecni_delez_dostopa %>% mutate(drzava=slovar[drzava]), aes(x=drzava, y=povprecje, fill=drzava)) + 
          geom_col() + coord_cartesian(ylim = c(40, 100)) + ggtitle('Povprečen delež dostopanja do interneta') +
          ylab('povprečje v %') + xlab('Države') + theme(axis.text.x = element_blank())

zemljevid1 <- tm_shape(merge(zemljevid,
                             povprecni_delez_dostopa %>% group_by(drzava),
                             by.x="SOVEREIGNT", by.y="drzava"), xlim=c(-25,32), ylim=c(32,72)) +
              tm_polygons("povprecje") + 
              tm_layout(main.title = "Povprečni deleži dostopanja do interneta držav EU")

najvecja_rast <- dostop_do_interneta %>%
                 group_by(drzava) %>%
                 summarise(razlika = (max(delez, na.rm = TRUE) - min(delez, na.rm = TRUE)))
primerjava_rasti <- dostop_do_interneta %>% mutate(drzava=slovar[drzava]) %>%
                    filter(drzava %in% c("Islandija", "Nizozemska", "Norveška",
                                         "Bolgarija", "Turčija", "Grčija"))

graf1b <- ggplot(data=primerjava_rasti, aes(x=leto, y=delez, col=drzava)) + 
          geom_point() + geom_line() + 
          ylab('delež v %') + ggtitle('Primerjava rasti držav z največjo in najmanjšo rastjo') +
          scale_x_continuous(breaks = 1*2007:2019) + labs(fill = "Država")

##################################################################################################

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
razlogi_po_letih$leto <- as.character(razlogi_po_letih$leto)

graf2b <- ggplot(data=razlogi_po_letih, aes(x=razlog)) + 
          geom_col(aes(y=delez, fill=leto), position = "dodge2") +
          ggtitle('Delež glavnih razlogov za nedostopanje do interneta po letih') + 
          ylab('deleži v %') + xlab('razlogi')

razlogi_po_drzavah_v_letu_2019 <- razlogi_za_ne_dostopanje_do_interneta %>%
                                  group_by(drzava) %>%
                                  filter(leto == "2019") %>%
                                  summarise(max(delez, na.rm = TRUE), razlog = which.max(delez))
names(razlogi_po_drzavah_v_letu_2019)[names(razlogi_po_drzavah_v_letu_2019) == "max(delez, na.rm = TRUE)"] <- "delez"
razlogi_po_drzavah_v_letu_2019$razlog[razlogi_po_drzavah_v_letu_2019$razlog == "1"] <- "previsoki stroski"
razlogi_po_drzavah_v_letu_2019$razlog[razlogi_po_drzavah_v_letu_2019$razlog == "2"] <- "predrag dostop"
razlogi_po_drzavah_v_letu_2019$razlog[razlogi_po_drzavah_v_letu_2019$razlog == "5"] <- "nepotreben"
razlogi_po_drzavah_v_letu_2019$razlog[razlogi_po_drzavah_v_letu_2019$razlog == "7"] <- "pomanjkanje spretnosti"
razlogi_po_drzavah_v_letu_2019$razlog[razlogi_po_drzavah_v_letu_2019$razlog == "11"] <- "drugo"

graf2c <- ggplot(data=razlogi_po_drzavah_v_letu_2019, aes(x=razlog)) + geom_bar(aes(fill=drzava)) +
          ggtitle('Najpogostejši razlogi za nedostopanje do interneta po državah') + 
          ylab('število držav') + xlab('razlog')


#########################################################################################

povprecen_delez_aktivnosti <- internetne_aktivnosti %>%
                              group_by(uporaba) %>%
                              summarise(mean(delez, na.rm = TRUE))
names(povprecen_delez_aktivnosti)[names(povprecen_delez_aktivnosti) == "mean(delez, na.rm = TRUE)"] <- "delez"
povprecen_delez_aktivnosti$delez <- round(povprecen_delez_aktivnosti$delez, 2)

graf3a <- ggplot(data=povprecen_delez_aktivnosti, aes(x=uporaba, y=delez, fill=uporaba)) + geom_col() +
          ggtitle('Povprečen delež internetnih aktivnosti') + 
          ylab('delež v %') + xlab('aktivnost') + theme(axis.text.x = element_blank())

aktivnosti_po_letih <- internetne_aktivnosti %>% mutate(leto=factor(leto)) %>%
                       filter(uporaba %in% c("branje novic", "posiljanje/prejemanje e-poste", 
                                            "poslusanje glasbe", 
                                            "pridobivanje informacij o proizvodih in storitvah",
                                            "uporaba aplikacij za izmenjavo sporocil (npr Messenger)",
                                            "uporaba socialnih omrezij")) %>%
                                            group_by(leto, uporaba) %>%
                                            summarise(mean(delez, na.rm = TRUE))
names(aktivnosti_po_letih)[names(aktivnosti_po_letih) == "mean(delez, na.rm = TRUE)"] <- "delez"
aktivnosti_po_letih$delez <- round(aktivnosti_po_letih$delez, 2)
aktivnosti_po_letih$leto <- as.character(aktivnosti_po_letih$leto)

graf3b <- ggplot(data=aktivnosti_po_letih, aes(x=uporaba, y=delez, fill=leto)) + 
          geom_col(position = "dodge2") + ggtitle('Delež glavnih aktivnosti pri uporabi interneta') + 
          ylab('deleži v %') + xlab('aktivnost') + theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

aktivnosti_po_drzavah_v_letu_2019 <- internetne_aktivnosti %>%
                                     group_by(drzava) %>%
                                     filter(leto == "2019") %>%
                                     summarise(max(delez, na.rm = TRUE), aktivnost = which.max(delez))
names(aktivnosti_po_drzavah_v_letu_2019)[names(aktivnosti_po_drzavah_v_letu_2019) == "max(delez, na.rm = TRUE)"] <- "delez"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "2"] <- "posiljanje/prejemanje e-poste"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "3"] <- "videoklici"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "4"] <- "uporaba socialnih omrezij"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "5"] <- "objavljanje avtorskega materiala na spletnih straneh"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "6"] <- "pridobivanje informacij o proizvodih in storitvah"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "7"] <- "branje novic"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "8"] <- "internetno bancnistvo"
aktivnosti_po_drzavah_v_letu_2019$aktivnost[aktivnosti_po_drzavah_v_letu_2019$aktivnost == "18"] <- "uporaba aplikacij za izmenjavi sporocil (npr Messenger)"

graf3c <- ggplot(data=aktivnosti_po_drzavah_v_letu_2019 %>% mutate(drzava=slovar[drzava]), aes(x=aktivnost)) + geom_bar(aes(fill=drzava)) +
          ggtitle('Najpogostejši razlogi za uporabo interneta po državah') + 
          ylab('število držav') + xlab('razlog')

zemljevid2 <- tm_shape(merge(zemljevid,
                             aktivnosti_po_drzavah_v_letu_2019 %>% group_by(drzava),
                             by.x="SOVEREIGNT", by.y="drzava"), xlim=c(-25,32), ylim=c(32,72)) +
              tm_polygons("aktivnost", palette = "Pastel1") + 
              tm_layout(main.title = "Najpogostejši razlogi za uporabo interneta")

#####################################################################################################

povprecno_znanje_drzav_po_letih <- digitalno_znanje %>%
                                   group_by(nivo.znanja, leto) %>%
                                   filter(skupina == "skupno posamezniki") %>%
                                   summarise(delez = mean(delez, na.rm = TRUE))
povprecno_znanje_drzav_po_letih$delez <- round(povprecno_znanje_drzav_po_letih$delez, 2)
povprecno_znanje_drzav_po_letih$leto <- as.character(povprecno_znanje_drzav_po_letih$leto)

graf4a <- ggplot(data=povprecno_znanje_drzav_po_letih, aes(x=nivo.znanja, y=delez, fill=leto)) + 
          geom_col(position = "dodge2") + ggtitle('Povprečna raven znanja državljanov EU po letih') + 
          ylab('deleži v %') + xlab('nivo znanja')

moski=c('moski')
znanje_moskih <- digitalno_znanje %>%
                 group_by(nivo.znanja, leto) %>%
                 filter(skupina == "moski (16-74 let)", leto == "2019") %>%
                 summarise(povprecje = mean(delez, na.rm = TRUE))
znanje_moskih$spol <- moski
znanje_moskih$povprecje <- round(znanje_moskih$povprecje, 2)
zenske=c('zenske')
znanje_zensk <- digitalno_znanje %>%
                group_by(nivo.znanja, leto) %>%
                filter(skupina == "zenske (16-74 let)", leto == "2019") %>%
                summarise(povprecje = mean(delez, na.rm = TRUE))
znanje_zensk$spol <- zenske
znanje_zensk$povprecje <- round(znanje_zensk$povprecje, 2)
primerjava_znanja_moskih_in_zensk <- rbind(znanje_moskih, znanje_zensk)
primerjava_znanja_moskih_in_zensk$leto <- NULL

graf4b <- ggplot(data=primerjava_znanja_moskih_in_zensk, aes(x=nivo.znanja, y=povprecje, fill=spol)) + 
          geom_col(position = "dodge2") + ggtitle('Povprečna raven znanja državljanov EU po spolu') + 
          ylab('deleži v %') + xlab('nivo znanja')

drzave_po_znanju <- digitalno_znanje %>%
                    group_by(drzava, leto) %>%
                    filter(skupina == "skupno posamezniki", nivo.znanja == "nadpovprecen")
drzave_po_znanju$nivo.znanja <- NULL
drzave_po_znanju$skupina <- NULL
primerjava_znanja_drzav <- drzave_po_znanju %>%
                           filter(drzava %in% c("Islandija", "Norveška", "Nizozemska", "Romunija", "Bolgarija", "Severna Makedonija"))
primerjava_znanja_drzav[is.na(primerjava_znanja_drzav)] <- 0
primerjava_znanja_drzav$leto <- as.character(primerjava_znanja_drzav$leto)

graf4c <- ggplot(data=primerjava_znanja_drzav, aes(x=drzava, y=delez, fill=leto)) +
          geom_col(position = "dodge2") + ggtitle('Primerjava znanja državljanov najboljših in najslabših držav EU') + 
          ylab('deleži v %') + xlab('država')

#######################################################################################################

graf5 <- ggplot(data=komunikacija_posameznikov_z_drzavo, aes(x=leto, y=delez, col=drzava)) + 
         geom_point() + geom_line() + 
         ylab('delež v %') + ggtitle('Komunikacija z državo') + scale_x_continuous(breaks = 1*2007:2019) + 
         labs(fill = "Država")

povprecni_delez_komunikacije <- komunikacija_posameznikov_z_drzavo %>% 
                                group_by(drzava) %>%
                                summarise(povprecje = mean(delez, na.rm = TRUE))
povprecni_delez_komunikacije$povprecje <- round(povprecni_delez_komunikacije$povprecje, 2)

graf5a <- ggplot(data=povprecni_delez_komunikacije, aes(x=drzava, y=povprecje, fill=drzava)) + 
          geom_col() + coord_cartesian(ylim = c(0, 100)) + 
          ggtitle('Povprečen delež komunikacije z državo') +
          ylab('povprečje v %') + xlab('Države') + theme(axis.text.x = element_blank())

najvecja_rast_komunikacije <- komunikacija_posameznikov_z_drzavo %>%
                              group_by(drzava) %>%
                              summarise(razlika = (max(delez, na.rm = TRUE) - min(delez, na.rm = TRUE)))
primerjava_rasti_komunikacije <- komunikacija_posameznikov_z_drzavo %>% 
                                 filter(drzava %in% c("Latvija", "Estonija", "Danska",
                                                      "Bolgarija", "Nemčija", "Italija")) 

graf5b <- ggplot(data=primerjava_rasti_komunikacije, aes(x=leto, y=delez, col=drzava)) + 
          geom_point() + geom_line() + 
          ylab('delež v %') + ggtitle('Primerjava rasti držav z največjo in najmanjšo rastjo komunikacije') +
          scale_x_continuous(breaks = 1*2007:2019) + labs(fill = "Država")

######################################################################################################

povprecen_delez_namena <- namen_uporabe_interneta_za_komunikacijo_z_drzavo %>%
                          group_by(namen.uporabe) %>%
                          summarise(mean(delez, na.rm = TRUE))
names(povprecen_delez_namena)[names(povprecen_delez_namena) == "mean(delez, na.rm = TRUE)"] <- "delez"
povprecen_delez_namena$delez <- round(povprecen_delez_namena$delez, 2)

graf6a <- ggplot(data=povprecen_delez_namena, aes(x=namen.uporabe, y=delez, fill=namen.uporabe)) + geom_col() +
          ggtitle('Povprečen delež posameznega namena komunikacije z državo') + 
          ylab('delež v %') + xlab('namen uporabe') + theme(axis.text.x = element_blank()) +
          theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=1))

nameni_po_letih <- namen_uporabe_interneta_za_komunikacijo_z_drzavo %>%
                   group_by(leto, namen.uporabe) %>%
                   summarise(mean(delez, na.rm = TRUE))
names(nameni_po_letih)[names(nameni_po_letih) == "mean(delez, na.rm = TRUE)"] <- "delez"
nameni_po_letih$delez <- round(nameni_po_letih$delez, 2)
nameni_po_letih$leto <- as.character(nameni_po_letih$leto)

graf6b <- ggplot(data=nameni_po_letih, aes(x=namen.uporabe)) + 
          geom_col(aes(y=delez, fill=leto), position = "dodge2") +
          ggtitle('Delež glavnih namenov za komunikacijo z državo') + 
          ylab('deleži v %') + xlab('namen uporabe') + 
          theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

nameni_po_drzavah_v_letu_2019 <-  namen_uporabe_interneta_za_komunikacijo_z_drzavo %>%
                                  group_by(drzava) %>%
                                  filter(leto == "2019") %>%
                                  summarise(max(delez, na.rm = TRUE), namen = which.max(delez))
names(nameni_po_drzavah_v_letu_2019)[names(nameni_po_drzavah_v_letu_2019) == "max(delez, na.rm = TRUE)"] <- "delez"
nameni_po_drzavah_v_letu_2019$namen[nameni_po_drzavah_v_letu_2019$namen == "1"] <- "Pridobivanje podatkov s spletnih strani"
nameni_po_drzavah_v_letu_2019$namen[nameni_po_drzavah_v_letu_2019$namen == "2"] <- "Prenos uradnih obrazcev"
nameni_po_drzavah_v_letu_2019$namen[nameni_po_drzavah_v_letu_2019$namen == "3"] <- "Oddajanje izpolnjenih obrazcev"


graf6c <- ggplot(data=nameni_po_drzavah_v_letu_2019 %>% mutate(drzava=slovar[drzava]), aes(x=namen)) + geom_bar(aes(fill=drzava)) +
          ggtitle('Najpogostejši nameni za komuniciranje z državo prek interneta') + 
          ylab('število držav') + xlab('namen') + 
          theme(axis.text.x=element_text(angle=90, vjust=0.5, hjust=1))

zemljevid3 <- tm_shape(merge(zemljevid,
                             nameni_po_drzavah_v_letu_2019 %>% group_by(drzava),
                             by.x="SOVEREIGNT", by.y="drzava"), xlim=c(-25,32), ylim=c(32,72)) +
              tm_polygons("namen", palette = "Pastel1") + 
              tm_layout(main.title = "Najpogostejši nameni za komuniciranje z državo")

################################################################################################
