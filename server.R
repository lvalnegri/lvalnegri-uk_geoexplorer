##################################################
# Shiny App * UK Geographies Explorer - server.R #
##################################################

shinyServer(function(input, output, session) {
    
    # LOGIN (lgn)
    source(file.path("server", "srv_lgn.R"),  local = TRUE)$value
    
    # MAPPA (map)
    source(file.path("server", "srv_xpl.R"),  local = TRUE)$value
    
})

