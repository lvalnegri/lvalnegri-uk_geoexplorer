##############################################################################
# Shiny App * UK Geographies Explorer - ESPLORE (xpl) - server.R (srv_map.R) #
##############################################################################

# TOGGLES ---------------------------------------
onclick('tgl_xpl_rif', toggle(id = 'hdn_xpl_rif', anim = TRUE) )            		# riferimento
onclick('tgl_xpl_bcn', toggle(id = 'hdn_xpl_bcn', anim = TRUE) )            		# bacino
onclick('tgl_xpl_tmt', toggle(id = 'hdn_xpl_tmt', anim = TRUE) )            		# tematismo
onclick('tgl_xpl_opz', toggle(id = 'hdn_xpl_opz', anim = TRUE) )            		# opzioni
onclick('tgl_xpl_opz_spt', toggle(id = 'hdn_xpl_opz_spt', anim = TRUE) )            # opzioni / sportelli
onclick('tgl_xpl_opz_spt_fil', toggle(id = 'hdn_xpl_opz_spt_fil', anim = TRUE) )	# opzioni / sportelli / filiale
onclick('tgl_xpl_opz_spt_bcn', toggle(id = 'hdn_xpl_opz_spt_bcn', anim = TRUE) )	# opzioni / sportelli / bacino
onclick('tgl_xpl_opz_spt_cmn', toggle(id = 'hdn_xpl_opz_spt_cmn', anim = TRUE) )	# opzioni / sportelli / comune / provincia
onclick('tgl_xpl_opz_bcn', toggle(id = 'hdn_xpl_opz_bcn', anim = TRUE) )	        # opzioni / bacini
onclick('tgl_xpl_opz_bcn_gen', toggle(id = 'hdn_xpl_opz_bcn_gen', anim = TRUE) )	# opzioni / bacini / generale
onclick('tgl_xpl_opz_bcn_pgn', toggle(id = 'hdn_xpl_opz_bcn_pgn', anim = TRUE) )	# opzioni / bacini / poligoni
onclick('tgl_xpl_opz_bcn_brd', toggle(id = 'hdn_xpl_opz_bcn_brd', anim = TRUE) )    # opzioni / bacini / bordi
onclick('tgl_xpl_opz_arc', toggle(id = 'hdn_xpl_opz_arc', anim = TRUE) )	        # opzioni / aree circolari
onclick('tgl_xpl_dwn', toggle(id = 'hdn_xpl_dwn', anim = TRUE) )            		# download

# CONTROLLI DINAMICI ----------------------------

## RIQUADRO FILIALE -----------------------------

### BANCA (dopo GRUPPO) -------------------------
output$ui_xpl_bnc <- renderUI({
    if(is.null(input$cbo_xpl_grp)) return(NULL)
    if(input$cbo_xpl_grp == '0'){
        y <- c('Scegli un Gruppo...' = '-1')
    } else {
        y <- filiali[['bnc']][gruppo == input$cbo_xpl_grp, as.character(banca)]
        if(length(y) > 1) y <- c('Scegli una Banca...' = '0', y)
    }
    pickerInput('cbo_xpl_bnc', 'BANCA:', y, options = list( size = 12, `actions-box` = TRUE, `live-search` = TRUE))
})

### PROVINCIA (dopo BANCA) ----------------------
output$ui_xpl_prv <- renderUI({
    if(is.null(input$cbo_xpl_bnc)) return(NULL)
    switch(input$cbo_xpl_bnc,
        '-1' = y <- c('Scegli un Gruppo...' = '-2'),
         '0' = y <- c('Scegli una Banca...' = '-1'),
        {
            y <- filiali[['prv']][gruppo == input$cbo_xpl_grp & banca == input$cbo_xpl_bnc, as.character(PRVd)]
            if(length(y) > 1) y <- c('Scegli una Provincia...' = '0', y)
        }
    )
    pickerInput('cbo_xpl_prv', 'PROVINCIA:', y, options = list(size = 15, `actions-box` = TRUE, `live-search` = TRUE))
})

### SPORTELLO (dopo PROVINCIA) ------------------
output$ui_xpl_spt <- renderUI({
    if(is.null(input$cbo_xpl_prv)) return(NULL)
    switch(input$cbo_xpl_prv,
        '-2' = y <- c('Scegli un Gruppo...' = '0'),
        '-1' = y <- c('Scegli una Banca...' = '0'),
         '0' = y <- c('Scegli una Provincia...' = '0'),
        {
            y0 <- spo[banca == input$cbo_xpl_bnc & PRVd == input$cbo_xpl_prv, .(SPT, sportello = paste0(CMNd, ' - ', indirizzo))]
            setorder(y0, 'sportello')
            create_input_list(y0)
            if(nrow(y0) > 1) y <- c('Scegli uno sportello...' = '0', y)
        }
    )
    pickerInput('cbo_xpl_spt', 'SPORTELLO:', y, options = list(size = 20, `actions-box` = TRUE, `live-search` = TRUE))
})

### MAPPA SCELTA COORDINATE INIZIALI ------------
output$map_xpl_spm <- renderLeaflet({ mpv })

### SCELTA SPORTELLO / PUNTO --------------------
ys <- reactive({ 
    switch(input$rbo_xpl_rif,
        '1' = {
            if(is.null(input$cbo_xpl_spt)) return(NULL)
            if(input$cbo_xpl_spt == '0') return(NULL)
            spo[SPT == input$cbo_xpl_spt]
        },
        '2' = {
            
        },
        '3' = {
            
        }
    )
})

### DETERMINA COME / QUANTI SPORTELLI CARICARE ----



## RIQUADRO BACINO ------------------------------

### TIPO BACINO ---------------------------------
output$ui_xpl_bcn_tpe <- renderUI({
    y <- c('Distanza Lineare' = '1', 'Demografico' = '2', 'Banca' = '3', 'Vicini (kNN)' = '4')
    if(!is.null(ys()))
        if(!is.na(szn[SZN == ys()$SZN, UTB])) y <- c('UTB' = '0', y)
    pickerInput('cbo_xpl_bcn', 'METODO COSTRUZIONE:', y, selected = '1', options = list(size = 20, `actions-box` = TRUE, `live-search` = TRUE))
})

### PARAMETRO I COSTRUZIONE BACINO (dipende da TIPO BACINO)  default value depends on the location type: capluogo, grande entro, altri comuni
output$ui_xpl_bcn_pr1 <- renderUI({
    y <- ifelse(is.null(input$cbo_xpl_bcn), '1', input$cbo_xpl_bcn)
    switch(y,
        '1' = { # DISTANZA
            sliderInput('xxx_xpl_bcn', 'DISTANZA MASSIMA (km):', min = 0.20, max = 5, value = 0.80, step = 0.10)
        },
        '2' = { # DEMOGRAFICO
            pickerInput('xxx_xpl_bcn', 'DATO:', dts.lst, 'Popolazione', multiple = FALSE, options = list(size = 20, `actions-box` = TRUE, `live-search` = TRUE))
        },
        '3' = { # BANCA
            pickerInput('xxx_xpl_bcn', 'DATO:', c('Clienti', 'Depositi', 'Impieghi'), options = list(size = 20, `actions-box` = TRUE, `live-search` = TRUE))
        },
        '4' = { # VICINI
            radioButtons('xxx_xpl_bcn', 'RIFERIMENTO:', choices = c('Numero' = '1', 'Ordine' = '2'), inline = TRUE)
        }
    )
})

### PARAMETRO II COSTRUZIONE BACINO
output$ui_xpl_bcn_pr2 <- renderUI({
    if(is.null(input$xxx_xpl_bcn)) return(NULL)
    switch(input$cbo_xpl_bcn,
        '2' = { # DEMOGRAFICO
            radioButtons('xxx_xpl_bcn_pr2', 'RIFERIMENTO:', choices = c('Somma' = '1', 'Percentuale' = '2', 'Valore Minimo' = '3', 'Valore Massimo' = '4')) 
        },
        '3' = { # BANCA
            sliderInput('xxx_xpl_bcn_pr2', 'VALORE:', min = 50, max = 1000, value = 500, step = 50)
        },
        '4' = { # VICINI
            y <- c(10, 250, 50, 10)
            if(input$xxx_xpl_bcn == '2') y <- c(1, 5, 2, 1)
            sliderInput('xxx_xpl_bcn_pr2', 'VALORE:', min = y[1], max = y[2], value = y[3], step = y[4])
        }
    )
})

### PARAMETRO III COSTRUZIONE BACINO
output$ui_xpl_bcn_pr3 <- renderUI({
    if(is.null(input$xxx_xpl_bcn_pr2)) return(NULL)
    switch(input$cbo_xpl_bcn,
        '2' = { # DEMOGRAFICO
            y <- switch(input$xxx_xpl_bcn_pr2,
                    '1' = { # Somma
                        c(500, 10000, 2500, 500)
                    }, 
                    '2' = { # Percentuale
                        c(10, 90, 40, 5)
                    }, 
                    '3' = { # Valore Minimo
                        c(100, 1000, 500, 50)
                    }, 
                    '4' = { # Valore Massimo
                        c(100, 5000, 1000, 100)
                    }
            )
            sliderInput('xxx_xpl_bcn_pr3', 'VALORE:', min = y[1], max = y[2], value = y[3], step = y[4])
        }
    )
})


## RIQUADRO TEMATISMO ---------------------------
output$ui_xpl_tmt <- renderUI({
    switch(input$rbo_xpl_tmt_ref,
        '1' = {
            pickerInput('xxx_xpl_tmt_ref', 'DATO:', ids.lst, 'Privati', options = list(size = 20, `actions-box` = TRUE, `live-search` = TRUE))
        },
        '2' = {
            NULL
        },
        '3' = {
            NULL
        },
        '4' = {
            NULL
        },
        '5' = {
            NULL
        }
    )
})


# COSTRUZIONE BACINO ----------------------------

## carica confini provincia
bnd.prv <- reactive({
    if(is.null(ys())) return(NULL)
    readRDS(file.path(bndit_path, 'rds', 'SZN', 's05', str_pad(ys()$PRV, 3, 'left', '0')))
})

## carica sezioni+distanza+knn per sportello 
yz <- reactive({
    if(is.null(ys())) return(NULL)
    dst[spo %chin% ys()$SPT]
})

# filtra lista sezioni con metodo bacino
yzb <- reactive({
    if(is.null(yz())) return(NULL)
    switch(input$cbo_xpl_bcn,
        '0' = { # UTB
            yz()[SZN %in% szn[UTB == ys()$UTB, SZN]]
        },
        '1' = { # DISTANZA LINEARE
            if(is.null(input$xxx_xpl_bcn)) return(NULL)
            yz()[distanza <= 1000 * as.numeric(input$xxx_xpl_bcn)]
        },
        '2' = { # DEMOGRAFICO
            if(is.null(input$xxx_xpl_bcn_pr3)) return(NULL)
            y <- dts[dato == input$xxx_xpl_bcn, .(SZN, V = valore)][yz(), on = 'SZN'][is.na(V), V := 0]
            switch(input$xxx_xpl_bcn_pr2,
                '1' = {
                    y[, cV := cumsum(V)][cV <= as.numeric(input$xxx_xpl_bcn_pr3)]
                },
                '2' = {
                    y1 <- if(is.na(ys()$UTB)){ szn[CMN == ys()$CMN, as.character(SZN)] } else { szn[UTB == ys()$UTB, as.character(SZN)] }
                    y[SZN %in% y1, `:=`( cV = cumsum(V), tV = dts[dato == input$xxx_xpl_bcn & SZN %in% y1, sum(valore)] )][, P := cV / tV][P <= as.numeric(input$xxx_xpl_bcn_pr3) / 100]
                },
                '3' = {
                    y[V >= as.numeric(input$xxx_xpl_bcn_pr3)]
                },
                '4' = {
                    y[V <= as.numeric(input$xxx_xpl_bcn_pr3)]
                }
            )
        },
        '3' = { # BANCA
            if(is.null(input$xxx_xpl_bcn_pr2)) return(NULL)
            
        },
        '4' = { # VICINI (kNN)
            if(is.null(input$xxx_xpl_bcn_pr2)) return(NULL)
            if(input$xxx_xpl_bcn == '1'){
                yz()[knn <= as.numeric(input$xxx_xpl_bcn_pr2)]
            } else {
                NULL
            }
        }
    )
})

# filtra dati delle sezioni nel bacino
ydb <- reactive({
    req(yzb())
    dts[SZN %in% yzb()$SZN]
})

# filtra sportelli competitor in bacino
yscb <- reactive({
    req(yzb())
    spo[SZN %in% yzb()$SZN][SPT != ys()$SPT]
})

# filtra sportelli competitor in bacino
yscc <- reactive({
    req(yscb())
    spo[CMNd == ys()$CMNd][!SPT %chin% c(ys()$SPT, yscb()$SPT)]
})

# filtra confini provincia con lista sezioni bacino (yzb)
bnd.bcn <- reactive({
    req(yzb())
    merge(bnd.prv(), yzb(), by.x = 'id', by.y = 'SZN', all.x = FALSE)
})



# CREA MAPPA ------------------------------------

map_1st <- reactive({
    
    if(is.null(ys())) return(mp)

    # setta il viewport sullo sportello
    mps <- mp %>% 
        setView(lng = ys()$x_lon, lat = ys()$y_lat, zoom = 13)
    
    # aggiunge le aree circolari se richieste (debbono rimanere come layer)
    if(input$chk_xpl_bcn_crc){
        for(r in seq(input$sld_xpl_bcn_crc, input$sld_xpl_bcn_crm, input$sld_xpl_bcn_crc))
            mps <- mps %>% 
                addCircles( 
                    lng = ys()$x_lon,
                    lat = ys()$y_lat,
                    fillOpacity = 0,  # NON CAMBIARE QUESTO VALORE!!!
                    stroke = TRUE,    # NON CAMBIARE QUESTO VALORE!!!
                    weight = input$sld_xpl_opz_arc_sps,
                    color = input$col_xpl_opz_arc_col,
                    radius = r
                )
    }
    
    # aggiunge sportello focale (ys) come punto pulsante
    mps <- mps %>% 
        addPulseMarkers(
            lng = ys()$x_lon, 
            lat = ys()$y_lat,
            label = add_label_dot(ys(), FALSE),
            icon = makePulseIcon(
                    color = input$col_xpl_opz_spt_fil, 
                    iconSize = input$sld_xpl_opz_spt_fil_sze, 
                    animate = input$chk_xpl_opz_spt_fil_anm,
                    heartbeat = input$sld_xpl_opz_spt_fil_trb
            )
        )

    mps
    
})

map_2nd <- reactive({
    
    if(is.null(map_1st())) return(mp)

    # aggiunge sportelli competitor bacino (yscb) come icon
    mps <- map_1st() %>%
            addAwesomeMarkers(
                data = yscb(),
                lng = ~x_lon,
                lat = ~y_lat,
                group = grp.lst[2],
                icon = awesomeIcons( icon = 'euro', iconColor = input$col_xpl_opz_spt_bcn_icn, markerColor = input$col_xpl_opz_spt_bcn_mrk ),
                label = add_label_dot(yscb())
            )

    # aggiunge sportelli competitor comune (yscc) come icona o cerchio
    if(input$rbo_xpl_opz_spt_cmn == 'Icona'){
        mps <- mps %>%
            addAwesomeMarkers(
                data = yscc(),
                lng = ~x_lon,
                lat = ~y_lat,
                group = grp.lst[3],
                icon = awesomeIcons( icon = 'euro', iconColor = input$col_xpl_opz_spt_cmn_icn, markerColor = input$col_xpl_opz_spt_cmn_mrk),
                label = add_label_dot(yscc())
            )
    } else {
        mps <- mps %>%
            addCircleMarkers(
                data = yscc(),
                lng = ~x_lon,
                lat = ~y_lat,
                group = grp.lst[3],
                radius = as.numeric(input$sld_xpl_opz_spt_cmn_sze),
                color = input$col_xpl_opz_spt_cmb,
                opacity = 1 - as.numeric(input$sld_xpl_opz_spt_cmn_trb)/10,
                weight = as.numeric(input$sld_xpl_opz_spt_cmn_szb),
                fillColor = input$col_xpl_opz_spt_cmn,
                fillOpacity = 1 - as.numeric(input$sld_xpl_opz_spt_cmn_trp)/10,
                label = add_label_dot(yscc())
            )
   }
    
    mps
    
})

map_out <- reactive({
    
    if(is.null(map_1st())) return(mp)
    if(is.null(map_2nd())) return(map_1st())

    y <- as.vector(bnd.bcn()@bbox)
    mps <- map_2nd() %>% 
        fitBounds(y[1], y[2], y[3], y[4])
        
    # aggiungi metrica per tematismo
    yb <- merge(bnd.bcn(), ids[, .(SZN, X = get(input$xxx_xpl_tmt_ref))], by.x = 'id', by.y = 'SZN')

    # determina i limiti per la palette colori
    n_brks <- input$sld_xpl_tmt_brk
    if(input$cbo_xpl_tmt_cls == 'fixed'){
        fixed_brks <- fxd_brks_idx
        mX <- min(yb$X, na.rm = TRUE)
        MX <- max(yb$X, na.rm = TRUE)
        if(MX > max(fxd_brks)) fixed_brks <- c(fixed_brks, MX)
        if(mX < min(fxd_brks)) fixed_brks <- c(mX, fixed_brks)
        n_brks <- length(fixed_brks) - 1
    }

    brks_poly <-
        if(input$cbo_xpl_tmt_cls == 'fixed'){
            classIntervals(yb$X, n = n_brks, style = 'fixed', fixedBreaks = fixed_brks)
        } else {
            classIntervals(yb$X, n_brks, input$cbo_xpl_tmt_cls)
        }

    # Determine the color palette
    if(input$rdo_xpl_opz_bcn_plg_tcp == 'P'){
        br_pal <- input$cbo_xpl_opz_bcn_plg_pal
        col_codes <-
            if(n_brks > brewer.pal.info[br_pal, 'maxcolors']){
                colorRampPalette(brewer.pal(brewer.pal.info[br_pal, 'maxcolors'], br_pal))(n_brks)
            } else {
                brewer.pal(n_brks, br_pal)
            }
        if(input$swt_xpl_opz_bcn_plg_pal) col_codes <- rev(col_codes)
    } else {
        col_codes <- colorRampPalette(c(input$col_xpl_opz_bcn_plg_clw, input$col_xpl_opz_bcn_plg_cmd, input$col_xpl_opz_bcn_plg_chg))(n_brks)
    }

    # build the lookup between values and colours and add it to dataset
    pal_poly <- findColours(brks_poly, col_codes)

    # aggiungi potenziali per etichette
    yb <- merge(yb, ids[, c('SZN', input$cbo_xpl_tmt_etc), with = FALSE], by.x = 'id', by.y = 'SZN')

    # aggiungi poligoni bacino
    mps <- mps %>%
        addPolygons(
            data = yb,
            layerId = ~id,
            group = grp.lst[1],
            color = input$col_xpl_opz_bcn_brd_bcl,
            weight = input$sld_xpl_opz_bcn_brd_bsz / 5,
            opacity = 1 - input$sld_xpl_opz_bcn_brd_btp / 10,
            fillColor = ~pal_poly,
            fillOpacity = 1 - input$sld_xpl_opz_bcn_plg_trp / 10,
            smoothFactor = 0.2,
            highlightOptions = highlightOptions(
                weight = input$sld_xpl_opz_bcn_brd_hsz,
                color = input$col_xpl_opz_bcn_brd_hcl,
                opacity = 1,
                bringToFront = TRUE
            ),
            label = add_label_poly(yb@data),
            labelOptions = lbl.options
        )

    if(input$chk_xpl_opz_bcn_gen_lgn){
        tlgn <- switch(input$rbo_xpl_tmt_ref,
                '1' = diz[sigla == input$xxx_xpl_tmt_ref, descrizione]
        )
        mps <- mps %>% 
        	addLegend(
                group = grp.lst[1],
                colors = col_codes,
                labels = get_xpl_legend(yb$X, brks_poly$brks),
        		position = input$cbo_xpl_opz_bcn_gen_lgn,
                title = tlgn,
        		opacity = 1 - input$sld_xpl_opz_bcn_plg_trp / 10
        	)
    }

    # if(input$chk_xpl_opz_bcn_gen_ttl){
    #     mps <- mps %>% 
    #         addControl(
    #             tags$div(HTML(paste0(
    #                 '<p style="font-size:20px;padding:10px 5px 10px 10px;margin:0px;background-color:#FFD5C6;">',
    #                     ys$banca, '<br>',
    #                     ys$indirizzo, '<br><br>',
    #                     'Bacino: ', input$cbo_xpl_bcnm, ', ', input$xxx_xpl_bcn,
    #                 '</p>'
    #             ))),
    #             position = input$cbo_xpl_opz_bcn_gen_ttl
    #         )
    # }
    
    mps %>% 
        addLayersControl( 
            baseGroups = names(tiles.lst), 
            overlayGroups = grp.lst, 
            options = layersControlOptions(collapsed = FALSE) 
        ) 
    
})

output$outmap <- renderLeaflet({
    
    map_out()
        
})

ydbe <- reactive({
    req(ydb())
    y <- ydb()[dato %in% input$cbo_xpl_tmt_pop]
    y <- diz[, .(sigla, descrizione, capitolo, ordine)][y, on = c(sigla = 'dato')]
    ydb()[, .(bacino = sum(valore)), dato][y, on = c(dato = 'sigla')][, prop := round(100 * valore / bacino, 1)][order(ordine)][, c('dato', 'bacino', 'ordine') := NULL]
})

# Mostra riquadro passaggio mouse su area -------
observeEvent(input$outmap_shape_, {
    showModal( 
        modalDialog(
            HTML(add_popup_poly(input$outmap_shape_click$id, ydbe())), 
            title = paste('Sezione:', input$outmap_shape_click$id),
            size = 'm',
            footer = NULL,
            easyClose = TRUE
        ) 
    )
})
    


# Mostra popup click mouse su area --------------
observeEvent(input$outmap_shape_click, {
    showModal( 
        modalDialog(
            HTML(add_popup_poly(input$outmap_shape_click$id, ydbe())), 
            title = paste('Sezione:', input$outmap_shape_click$id),
            size = 'm',
            footer = NULL,
            easyClose = TRUE
        ) 
    )
})
    

# CREA GRAFICO ----------------------------------

plt_out <- reactive({
    y <- ydb()[dato %chin% diz[report == 'C1', sigla], .(valore = sum(valore)), dato][, ordine := 1:.N]
    y[, dato := gsub('Pop_|_anni|fino_', '', dato)]
    y[, dato := gsub('_e_', ' e ', dato)]
    y[, dato := gsub('_', ' > ', dato)]
    y[dato == '4', dato := 'fino a 4'] 
    ggplot(y, aes(reorder(dato, ordine), valore)) +
        geom_col() +
        geom_text(aes(label = dot_comma(valore)), color = 'white', size = 3, fontface = 'bold', hjust = 1.1) +
        scale_y_continuous(labels = dot_comma) +
        labs(x = '', y = '') +
        coord_flip() + 
        theme_minimal()
})

output$out_plt <- renderPlot({
    
    plt_out()
        
})


# CREA TABELLA ----------------------------------
tbl_out <- reactive({
    
    y <- ydb()[dato %chin% diz[report == 'T1', sigla], .(valore = sum(valore)), dato][, c('S', 'F') := tstrsplit(dato, '_', fixed = TRUE)]
    y <- dcast(y, S~F, value.var = 'valore', fill = 0)
    y[, Altra := SEO - SCO - SPO]
    y <- ec_sett.lst[y, on = 'S'][, Settore := paste(substring(S, 2), '-', S2)][, c('S', 'S2') := NULL]
    setcolorder(y, c('Settore', 'SPO', 'SCO', 'Altra', 'SEO'))
    setnames(y, c('Settore', 'Persone', 'Capitale', 'Altra', 'TOTALE'))
    y <- rbindlist(list( y, data.table('TOTALE', y[, lapply(.SD, sum), .SDcols = 2:5])), use.names = FALSE)
    y[, 2:5 := lapply(.SD, dot_comma), .SDcols = 2:5]
    kable(y, align = c('l', rep('r', 4))) %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), font_size = 12, full_width = F, position = "left") %>% 
        column_spec(1, bold = TRUE, border_right = TRUE) %>% 
        column_spec(5, bold = TRUE, border_left = TRUE) %>% 
        row_spec(nrow(y), bold = TRUE, align = 'c', font_size = 14)

})

output$out_tbl <- renderUI({
    
    HTML(tbl_out())
        
})




# DOWNLOAD --------------------------------------
output$dwn_map <- downloadHandler(
    filename <- 'test.html', # function(){ paste0(filename.clean(input$txt_xpl_dwn), '.', input$rdo_xpl_dwn) },
    content <- function(file){ 
        switch(input$rdo_xpl_dwn,
            'csv'  = {  },
            'html' = { saveWidget(map_out(), file) }
        )
    }
)

