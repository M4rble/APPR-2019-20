# 2. faza: Uvoz podatkov

dostop_do_interneta <- read_csv("podatki/dostop_do_interneta.csv", 
                                col_names=c("leto", "drzava", "UNIT" ,"HHTYP","delez"),
                                skip=1, na=":", locale=locale(encoding="Windows-1250"))
dostop_do_interneta$UNIT <- NULL
dostop_do_interneta$HHTYP <- NULL
dostop_do_interneta$drzava[dostop_do_interneta$drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
dostop_do_interneta <- dostop_do_interneta %>% mutate(drzava=slovar[drzava])



razlogi_za_ne_dostopanje_do_interneta <- read_csv("podatki/razlogi.csv", col_names=c("razlog", "drzava", 
                                                                                     "leto", "odstotki", "total", "delez"),
                                                  skip=1, na=":", locale=locale(encoding="Windows-1250"))
razlogi_za_ne_dostopanje_do_interneta$odstotki <- NULL
razlogi_za_ne_dostopanje_do_interneta$total <- NULL
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
razlogi_za_ne_dostopanje_do_interneta <- razlogi_za_ne_dostopanje_do_interneta %>% mutate(drzava=slovar[drzava])


internetne_aktivnosti <- read_csv("podatki/internetne_aktivnosti.csv",
                                  col_names=c("uporaba", "drzava", "leto", "UNIT", "IND_TYPE", "delez"),
                                  skip=1, na=":", locale=locale(encoding="Windows-1250"))
internetne_aktivnosti$UNIT <- NULL
internetne_aktivnosti$IND_TYPE <- NULL
internetne_aktivnosti$drzava[internetne_aktivnosti$drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: doing an online course (of any subject)"] <- "internetni seminar"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: sending/receiving e-mails"] <- "posiljanje/prejemanje e-poste"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: telephoning or video calls"] <- "videoklici"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: participating in social networks (creating user profile, posting messages or other contributions to facebook, twitter, etc.)"] <- "uporaba socialnih omrezij"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: uploading self-created content to any website to be shared"] <- "objavljanje avtorskega materiala na spletnih straneh"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: finding information about goods and services"] <- "pridobivanje informacij o proizvodih in storitvah"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: reading online news sites/newspapers/news magazines"] <- "branje novic"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: Internet banking"] <- "internetno bancnistvo"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: selling goods or services"] <- "prodajanje proizvodov in storitev"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: job search or sending an application"] <- "iskanje sluzbe in oddajanje prijav"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: seeking health information"] <- "iskanje zdravstvenih informacij"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: taking part in on-line consultations or voting to define civic or political issues (e.g. urban planning, signing a petition)"] <- "internetno politicno udejstvovanje (npr podpisovanje peticij)"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: posting opinions on civic or political issues via websites (e.g. blogs, social networks, etc."] <- "objavljanje politicnih mnenj na spletnih straneh"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: civic or political participation"] <- "splosno drzavljansko ali politicno sodelovanje"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: online learning material"] <- "dostop do snovi za ucenje"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: communicating with instructors or students using educational websites/portals"] <- "instrukcije"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: any of the learning activities i_iuolc, i_iuolm, i_iuocis"] <- "druge izobrazevalne aktivnosti"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: instant messaging, i.e. exchanging messages, for example, via Skype, Messenger, WhatsApp, Viber"] <- "uporaba aplikacij za izmenjavi sporocil (npr Messenger)"
internetne_aktivnosti$uporaba[internetne_aktivnosti$uporaba == "Internet use: listening to music (e.g. w eb radio, music streaming)"] <- "poslusanje glasbe"
internetne_aktivnosti <- internetne_aktivnosti %>% mutate(drzava=slovar[drzava])                                


digitalno_znanje <- read_csv("podatki/digitalno_znanje.csv", 
                             col_names=c("nivo.znanja", "drzava", "leto", "skupina", "UNIT", "delez"),
                             skip=1, na=":", locale=locale(encoding="Windows-1250"))
digitalno_znanje$UNIT <- NULL
digitalno_znanje$skupina[digitalno_znanje$skupina == "All Individuals"] <- "skupno posamezniki"
digitalno_znanje$skupina[digitalno_znanje$skupina == "Males, 16 to 74 years old"] <- "moski (16-74 let)"
digitalno_znanje$skupina[digitalno_znanje$skupina == "Females, 16 to 74 years old"] <- "zenske (16-74 let)"
digitalno_znanje$drzava[digitalno_znanje$drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
digitalno_znanje$`nivo.znanja`[digitalno_znanje$`nivo.znanja` == "Individuals who have low overall digital skills"] <- "nizek"
digitalno_znanje$`nivo.znanja`[digitalno_znanje$`nivo.znanja` == "Individuals who have basic overall digital skills"] <- "osnoven"
digitalno_znanje$`nivo.znanja`[digitalno_znanje$`nivo.znanja` == "Individuals who have above basic overall digital skills"] <- "nadpovprecen"
digitalno_znanje$`nivo.znanja`[digitalno_znanje$`nivo.znanja` == "Individuals who have no overall digital skills"] <- "brez znanja"
digitalno_znanje <- digitalno_znanje %>% mutate(drzava=slovar[drzava])


komunikacija_posameznikov_z_drzavo <- read_csv("podatki/komunikacija_posameznikov_z_drzavo.csv", 
                                               col_names=c("leto", "drzava", "INDIC_IS", "UNIT", 
                                                           "IND_TYPE","delez"),
                                               skip=1, na=":", locale=locale(encoding="Windows-1250"))
komunikacija_posameznikov_z_drzavo$INDIC_IS <- NULL
komunikacija_posameznikov_z_drzavo$UNIT <- NULL
komunikacija_posameznikov_z_drzavo$IND_TYPE <- NULL
komunikacija_posameznikov_z_drzavo$drzava[komunikacija_posameznikov_z_drzavo$drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
komunikacija_posameznikov_z_drzavo <- komunikacija_posameznikov_z_drzavo %>% mutate(drzava=slovar[drzava])


namen_uporabe_interneta_za_komunikacijo_z_drzavo <- read_csv("podatki/namen_uporabe_interneta_za_komunikacijo_z_drzavo.csv", 
                                                             col_names=c("namen.uporabe", "drzava", 
                                                                         "leto", "UNIT", "IND_TYPE", "delez"),
                                                             skip=1, na=":", locale=locale(encoding="Windows-1250"))
namen_uporabe_interneta_za_komunikacijo_z_drzavo$UNIT <- NULL
namen_uporabe_interneta_za_komunikacijo_z_drzavo$IND_TYPE <- NULL
namen_uporabe_interneta_za_komunikacijo_z_drzavo$drzava[namen_uporabe_interneta_za_komunikacijo_z_drzavo$drzava == "Germany (until 1990 former territory of the FRG)"] <- "Germany"
namen_uporabe_interneta_za_komunikacijo_z_drzavo$`namen.uporabe`[namen_uporabe_interneta_za_komunikacijo_z_drzavo$`namen.uporabe` == "Internet use: obtaining information from public authorities web sites (last 12 months)"] <- "Pridobivanje podatkov s spletnih strani"
namen_uporabe_interneta_za_komunikacijo_z_drzavo$`namen.uporabe`[namen_uporabe_interneta_za_komunikacijo_z_drzavo$`namen.uporabe` == "Internet use: downloading official forms (last 12 months)"] <- "Prenos uradnih obrazcev"
namen_uporabe_interneta_za_komunikacijo_z_drzavo$`namen.uporabe`[namen_uporabe_interneta_za_komunikacijo_z_drzavo$`namen.uporabe` == "Internet use: submitting completed forms (last 12 months)"] <- "Oddajanje izpolnjenih obrazcev"
namen_uporabe_interneta_za_komunikacijo_z_drzavo <- namen_uporabe_interneta_za_komunikacijo_z_drzavo %>% mutate(drzava=slovar[drzava])









#originalni podatki, če boš še za kej rabu, zbriši ko končaš
sl <- locale("sl", decimal_mark=",", grouping_mark=".")

# Funkcija, ki uvozi občine iz Wikipedije
uvozi.obcine <- function() {
  link <- "http://sl.wikipedia.org/wiki/Seznam_ob%C4%8Din_v_Sloveniji"
  stran <- html_session(link) %>% read_html()
  tabela <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
    .[[1]] %>% html_table(dec=",")
  for (i in 1:ncol(tabela)) {
    if (is.character(tabela[[i]])) {
      Encoding(tabela[[i]]) <- "UTF-8"
    }
  }
  colnames(tabela) <- c("obcina", "povrsina", "prebivalci", "gostota", "naselja",
                        "ustanovitev", "pokrajina", "regija", "odcepitev")
  tabela$obcina <- gsub("Slovenskih", "Slov.", tabela$obcina)
  tabela$obcina[tabela$obcina == "Kanal ob Soči"] <- "Kanal"
  tabela$obcina[tabela$obcina == "Loški potok"] <- "Loški Potok"
  for (col in c("povrsina", "prebivalci", "gostota", "naselja", "ustanovitev")) {
    if (is.character(tabela[[col]])) {
      tabela[[col]] <- parse_number(tabela[[col]], na="-", locale=sl)
    }
  }
  for (col in c("obcina", "pokrajina", "regija")) {
    tabela[[col]] <- factor(tabela[[col]])
  }
  return(tabela)
}

# Funkcija, ki uvozi podatke iz datoteke druzine.csv
uvozi.druzine <- function(obcine) {
  data <- read_csv2("podatki/druzine.csv", col_names=c("obcina", 1:4),
                    locale=locale(encoding="Windows-1250"))
  data$obcina <- data$obcina %>% strapplyc("^([^/]*)") %>% unlist() %>%
    strapplyc("([^ ]+)") %>% sapply(paste, collapse=" ") %>% unlist()
  data$obcina[data$obcina == "Sveti Jurij"] <- iconv("Sveti Jurij ob Ščavnici", to="UTF-8")
  data <- data %>% gather(`1`:`4`, key="velikost.druzine", value="stevilo.druzin")
  data$velikost.druzine <- parse_number(data$velikost.druzine)
  data$obcina <- parse_factor(data$obcina, levels=obcine)
  return(data)
}

# Zapišimo podatke v razpredelnico obcine
obcine <- uvozi.obcine()

# Zapišimo podatke v razpredelnico druzine.
druzine <- uvozi.druzine(levels(obcine$obcina))

# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
