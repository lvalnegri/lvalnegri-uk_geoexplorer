##############################################
# Shiny App * UK Geographies Explorer - ui.R #
##############################################

shinyUI(fluidPage(
    
    use_waiter(),
    waiter_show_on_load(spin_hourglass(), "#333e48"),

    includeCSS(file.path(fpath, 'styles.css')),
#    includeScript(file.path(fpath, 'scripts.js')),
    tags$head(
        tags$link(rel="shortcut icon", href="master-icon.ico"),
        tags$link(rel="stylesheet", href="https://use.fontawesome.com/releases/v5.8.1/css/all.css")
    ),
    
    navbarPageWithText(
        header = '',
        title = HTML('<div><img src="master-logo.png" class="logo"><span class = "title">Analisi Sovrapposizione Sportelli</span></div>'),
        windowTitle = 'MaSTeR Information - Analisi Sovrapposizione Sportelli', 
        id = 'mainNav',
        theme = shinytheme('united'), inverse = TRUE,
        
        # LOG IN (lgn)
        source(file.path("ui", "ui_lgn.R"),  local = TRUE)$value,
        
        # MAPPA (map)
        source(file.path("ui", "ui_xpl.R"),  local = TRUE)$value,
    
        text = '@2021 MaSTeR Information'

    ),

    useShinyjs()

))
