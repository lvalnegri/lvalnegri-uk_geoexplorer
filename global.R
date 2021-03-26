##################################################
# Shiny App * UK Geographies Explorer - global.R #
##################################################

# load packages
pkg <- c('popiFun',
    'Cairo', 'colourpicker', 'data.table', 'DT', 'fontawesome', 'fst', 'ggplot2', 'htmltools', 'kableExtra', 
    'leaflet', 'leaflet.extras',
    'shiny', 'bslib', 'shinyalert', 'shinycssloaders', 'shinyjs', 'shinythemes', 'shinyWidgets', 'waiter'
)
invisible(lapply(pkg, require, char = TRUE))

# settings
enableBookmarking(store = "url")
options(spinner.color = '#333399', spinner.size = 1, spinner.type = 4)
options(bitmapType = 'cairo', shiny.usecairo = TRUE)
options(knitr.table.format = 'html')

# constants
fpath <- file.path(app_path, 'uk_geoexplorer')

# load data
fns <- list('' = 'dati', 'diz' = 'dizdati', 'spo' = 'sportelli', 'szn' = 'sezioni', 'cmn' = 'comuni')
for(idx in seq(length(fns))) assign(names(fns[idx]), read_fst(file.path(fpath, fns[[idx]]), as.data.table = TRUE))
diz <- diz[attivo == 1]

# load boundaries

# lists
dts.lst <- create_input_list(
    diz[tipo == 'D'][order(capitolo, ordine)][, .(as.character(sigla), descrizione, capitolo)],
    TRUE
)
dts.pop <- create_input_list(diz[sel_etc >= 1, .(as.character(sigla), descrizione)])

ids.lst <- create_input_list(
    diz[tipo == 'I'][order(capitolo, ordine)][, .(as.character(sigla), descrizione, capitolo)],
    TRUE
)
ids.etc <- create_input_list(diz[sel_etc == 2, .(as.character(sigla), descrizione)])

grp.lst <- c('Bacino' = '1', 'Competitor Bacino' = '2', 'Competitor Comune' = '3')
bcn.lst <- c(  # se CMN di spoA non ammette UTB, scelta '0' va cancellata
    'UTB' = '0',
    'Distanza Lineare' = '1',
    'Dato' = '2'    # A questo deve seguire lista di possibili 
)
tmt.lst <- c(
    'Indice' = '1',       # mostra lista indici
    'Proporzione' = '2',  # mostra lista capitoli + num (den e' unico in ogni capitolo ?)
    'Metrica' = '3',      # mostra lista potenziali num e den 
    'Prevalenza' = '4',   # categorico / semi-ord
    'Cluster' = '5'       # categorico / ord
)

filiali <- list(
    'grp' = levels(spo$gruppo),
    'bnc' = unique(spo[, .(gruppo, banca)])[order(gruppo, banca)],
    'prv' = unique(spo[, .(gruppo, banca, PRVd)])[order(gruppo, banca, PRVd)]
)

# basemap
mp <- basemap()
mpv <- basemap(tiles = NULL, menu = FALSE, extras = 'draw') # , opt.draw = '')

### FUNZIONI

add_label_dot <- function(y, comp = TRUE){
    lapply(
        1:nrow(y),
        function(x)
            HTML(paste0(
                '<b>Gruppo</b>: ', y$gruppo[x], '<br>',
                '<b>Banca</b>: ', y$banca[x], '<br>',
                '<b>CAB</b>: ', y$cab[x], '<br>',
                '<b>Codice</b>: ', y$id[x], '<br>',
                '<b>Indirizzo</b>: ', y$indirizzo[x], ', ', y$cap[x], ', ', toupper(y$CMNd[x]), ', ', '<br>',
                '<b>Apertura</b>: ', format(y$apertura[x], '%d %B %Y'), '<br>',
                '<br>',
                ifelse(comp, '',
                    paste0(
                        '<b>Clienti</b>: ', 'n.d.', '<br>',
                        '<b>Depositi</b>: ', 'n.d.', '<br>',
                        '<b>Impieghi</b>: ', 'n.d.', '<br>'
                    )
                )
            ))
    )
}

add_label_poly <- function(y){
    lapply(
        1:nrow(y),
        function(x){
            yt1 <- paste0(
                '<b>Sezione</b>: ', y$id[x], '<br>', 
                '<b>Distanza</b>: ', format(round(y$distanza[x] / 1000, 3), nsmall = 3), ' km<br>', 
                '<b>kNN</b>: ', y$knn[x], '<br><br>'
            )
            yt2 <- NULL
            yx <- diz[sigla %in% names(y)[which(nchar(names(y)) > 2)]][order(ordine)][, .(capitolo, sigla)]
            for(idx in 1:nrow(yx)){
                ydx <- y[[yx[idx, sigla]]][x]
                yt2 <- paste0(yt2, '<b>', yx[idx, sigla], '</b>: ', ifelse(is.factor(ydx), as.character(ydx), format(round(ydx, 3), nsmall = 3)), '<br>')
                if(idx < nrow(yx)) if(yx[idx, capitolo] != yx[idx + 1, capitolo]) yt2 <- paste0(yt2, '<br>')
            }
            HTML(paste(yt1, yt2))
        }
    )
}

add_popup_poly <- function(x, ydbe){
    y <- ydbe[SZN == x]
    yk <- kable(y[, .(descrizione, valore, prop)]) %>%
            kable_styling(
                bootstrap_options = c('striped', 'hover', 'condensed', 'responsive'), 
                font_size = 10, 
                full_width = FALSE
            )
    for(cp in unique(y$capitolo))
        yk <- yk %>% 
                pack_rows(cp, 
                    min(y[capitolo == cp, which = TRUE]), 
                    max(y[capitolo == cp, which = TRUE]),
                    label_row_css = "background-color: #666; color: #fff;"
                )
    if(nrow(y) > 10) yk <- yk %>% scroll_box(height = '300px')
    yk
}

