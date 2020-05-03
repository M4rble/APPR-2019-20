library(shiny)



shinyServer(function(input, output) {
  output$dostop_do_interneta <- DT::renderDataTable({
    dostop_do_interneta %>% mutate(drzava=slovar[drzava]) %>%
      rename("Država"=drzava, "Delež v odstotkih"=delez, "Leto"=leto)
  })
})


