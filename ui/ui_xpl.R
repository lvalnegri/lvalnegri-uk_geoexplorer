#########################################################################
# Shiny App * UK Geographies Explorer - ESPLORE (xpl) - ui.R (ui_xpl.R) #
#########################################################################

tabPanel('EXPLORE', icon = icon('rocket'), 

    # SIDEBAR PANEL -----------------------------
    sidebarPanel(

        ## GEOGRAPHY ----------------------------
        a(id = 'tgl_xpl_geo', 'Mostra/Nascondi GEOGRAPHY', class = 'toggle-choices'),  
        div(id = 'hdn_xpl_geo', class = 'toggle-choices-content',
            p(),

            radioButtons('rbo_xpl_rif', 'PARTI DA:', c('Sportello' = '1', 'Coordinate' = '2', 'Click su mappa' = '3')),

            conditionalPanel("input.rbo_xpl_rif === '1'",

                pickerInput('cbo_xpl_grp', 'GRUPPO:', 
                    c( 'Scegli un Gruppo...' = '0', filiali[['grp']] ),
                    options = list(size = 12, `actions-box` = TRUE, `live-search` = TRUE)
                ),

                uiOutput('ui_xpl_bnc'),

                uiOutput('ui_xpl_prv'),

                uiOutput('ui_xpl_cap'),

                uiOutput('ui_xpl_spt')
            
            ),

            conditionalPanel("input.rbo_xpl_rif === '2'",

                textInput('txt_xpl_spn', 'LONGITUDINE [EST < > OVEST]:', value = '', placeholder = paste('Inserire un valore fra', bbox.it[1, 1], 'e', bbox.it[1, 2]) ),

                textInput('txt_xpl_spd', 'LATITUDINE [NORD < > SUD]:', value = '', placeholder = paste('Inserire un valore fra', bbox.it[2, 1], 'e', bbox.it[2, 2]) )

            ),
                
            conditionalPanel("input.rbo_xpl_rif === '3'",

                leafletOutput('map_xpl_spm')

            )

        ## FINE DI FILIALE
        ), br(),
        
        ## BACINO --------------------------------
        a(id = 'tgl_xpl_bcn', 'Mostra/Nascondi BACINO', class = 'toggle-choices'),  
        div(id = 'hdn_xpl_bcn', class = 'toggle-choices-content',
            p(),

            uiOutput('ui_xpl_bcn_tpe'),

            uiOutput('ui_xpl_bcn_pr1'),

            uiOutput('ui_xpl_bcn_pr2'),

            uiOutput('ui_xpl_bcn_pr3')

        ## FINE DI BACINO
        ), br(),

        ## TEMATISMO -----------------------------
        a(id = 'tgl_xpl_tmt', 'Mostra/Nascondi TEMATISMO', class = 'toggle-choices'),  
        div(id = 'hdn_xpl_tmt', class = 'toggle-choices-content',
            p(),

            radioButtons('rbo_xpl_tmt_ref', 'TEMATISMO:', c('Potenziali' = '1', 'Prevalenza' = '2', 'Cluster' = '3', 'Percentuale' = '4', 'Rapporto' = '5')),

            uiOutput('ui_xpl_tmt'),

            pickerInput('cbo_xpl_tmt_cls', 'CLASSIFICAZIONE:', choices = class.lst),

# 		    conditionalPanel("input.cbo_xpl_tmt_cls === 'fixed'",
#                 # Choose Fixed Breaks, default taken from "fxd_brks" according its column "code" and "fxd_brks" in "metrics"
#                 uiOutput('ui_xpl_mtc_fbk'),
#                 # check how provided limits translate into breaks
#                 actionButton('btn_xpl_mtc_fbk', 'CHECK BREAKS'),
#                 br(), br()
# 		    ),

            conditionalPanel("input.cbo_xpl_tmt_cls !== 'fixed'",
                
                sliderInput('sld_xpl_tmt_brk', 'NUMERO DI CLASSI:', min = 3, max = 20, value = 11, step = 1, ticks = FALSE, sep = ''),

                sliderInput('sld_xpl_tmt_out', ' OUTLIERS:', min = 90, max = 100, value = 100, step = 0.5, post = '%', ticks = FALSE)

            ),

            pickerInput( 
                inputId = 'cbo_xpl_tmt_etc', 
                label = 'DATI ETICHETTE POLIGONI:', 
                choices = ids.lst,
                selected = ids.etc,
                multiple = TRUE, 
                options = list(
                    size = 20, 
                    maxOptions = 12, 
                    maxOptionsText = 'Massimo numero di dati raggiunto',
                    liveSearchPlaceholder = 'Cerca il dato...',
                    `actions-box` = TRUE, 
                    `live-search` = TRUE
                )
            ),

            pickerInput( 
                inputId = 'cbo_xpl_tmt_pop', 
                label = 'DATI TABELLA POPUP:', 
                choices = dts.lst,
                selected = dts.pop,
                multiple = TRUE, 
                options = list(
                    size = 20, 
                    maxOptions = 12, 
                    maxOptionsText = 'Massimo numero di dati raggiunto',
                    liveSearchPlaceholder = 'Cerca il dato...',
                    `actions-box` = TRUE, 
                    `live-search` = TRUE
                )
            ),

            checkboxInput('chk_xpl_bcn_crc', 'AGGIUNGI AREE CIRCOLARI'),

            conditionalPanel("input.chk_xpl_bcn_crc",

                sliderInput('sld_xpl_bcn_crc', 'DISTANZA FRA AREE (metri):', min = 100, max = 1000, value = 200, step = 100),

                sliderInput('sld_xpl_bcn_crm', 'MASSIMO CERCHIO (metri):', min = 400, max = 5000, value = 800, step = 200)

            )

        ## FINE DI TEMATISMO
        ), br(),

        ## OPZIONI -------------------------------
        a(id = 'tgl_xpl_opz', 'Mostra/Nascondi OPZIONI', class = 'toggle-choices'),  
        hidden( div(id = 'hdn_xpl_opz', class = 'toggle-choices-content',
            p(),

            ### OPZIONI SPORTELLI -----------------
            a(id = 'tgl_xpl_opz_spt', 'Show/Hide SPORTELLI', class = 'toggle-choices-sub'),  
            hidden( div(id = 'hdn_xpl_opz_spt', class = 'toggle-choices-subcontent',
                p(),
    		    
                #### OPZIONI SPORTELLI: FILIALE ----
                a(id = 'tgl_xpl_opz_spt_fil', 'Show/Hide FILIALE', class = 'toggle-choices-sub'),  
                hidden( div(id = 'hdn_xpl_opz_spt_fil', class = 'toggle-choices-subcontent',
                    p(),

                    colourpicker::colourInput('col_xpl_opz_spt_fil', 'COLORE:', value = '#000', showColour = 'background'),

                    sliderInput('sld_xpl_opz_spt_fil_sze', 'LARGHEZZA:', min = 6, max = 20, value = 14, step = 1, ticks = FALSE),

                    checkboxInput('chk_xpl_opz_spt_fil_anm', 'ANIMAZIONE:', TRUE),

                    conditionalPanel("input.chk_xpl_opz_spt_fil_anm == true",

                        sliderInput('sld_xpl_opz_spt_fil_trb', 'TEMPO (secondi):', min = 1, max = 10, value = 2, step = 1, ticks = FALSE)

                    )

                )), br(),

                #### OPZIONI SPORTELLI: BACINO -----
                a(id = 'tgl_xpl_opz_spt_bcn', 'Show/Hide BACINO', class = 'toggle-choices-sub'),  
                hidden( div(id = 'hdn_xpl_opz_spt_bcn', class = 'toggle-choices-subcontent',
                    p(),

                    colourpicker::colourInput('col_xpl_opz_spt_bcn_icn', 'COLORE ICONA:', value = '#FFF', showColour = 'background'),

                    colourpicker::colourInput(
                        inputId = 'col_xpl_opz_spt_bcn_mrk', 
                        label = 'COLORE MARKER:', 
                        value = 'red', 
                        showColour = 'both',
                        palette = 'limited',
                        allowedCols = marker_colours,
                        returnName = TRUE
                    )

        		)), br(),
                
                #### OPZIONI SPORTELLI: COMUNE -----
                a(id = 'tgl_xpl_opz_spt_cmn', 'Show/Hide COMUNE', class = 'toggle-choices-sub'),  
                hidden( div(id = 'hdn_xpl_opz_spt_cmn', class = 'toggle-choices-subcontent',
                    p(),
    
                    radioButtons('rbo_xpl_opz_spt_cmn', 'TIPO:', c('Punto', 'Icona')),

                    conditionalPanel("input.rbo_xpl_opz_spt_cmn == 'Icona'",

                        colourpicker::colourInput('col_xpl_opz_spt_cmn_icn', 'COLORE ICONA:', value = '#000', showColour = 'background'),

                        colourpicker::colourInput(
                            inputId = 'col_xpl_opz_spt_cmn_mrk', 
                            label = 'COLORE MARKER:', 
                            value = 'lightblue', 
                            showColour = 'both',
                            palette = 'limited',
                            allowedCols = marker_colours,
                            returnName = TRUE
                        )

                    ),
        		    
                    conditionalPanel("input.rbo_xpl_opz_spt_cmn == 'Punto'",

                        sliderInput('sld_xpl_opz_spt_cmn_sze', 'LARGHEZZA:', min = 1, max = 15, value = 6, step = 1, ticks = FALSE),

                        colourpicker::colourInput('col_xpl_opz_spt_cmn', 'COLORE:', value = 'blue', showColour = 'background'),

        			    sliderInput('sld_xpl_opz_spt_cmn_trp', 'TRASPARENZA:', min = 0, max = 10, value = 3, step = 1, ticks = FALSE),

        	    		sliderInput('sld_xpl_opz_spt_cmn_szb', 'SPESSORE BORDO:', min = 1, max = 5, value = 2, step = 0.5, ticks = FALSE),

                        colourpicker::colourInput('col_xpl_opz_spt_cmb', 'COLORE BORDO:', value = 'black', showColour = 'background'),

        			    sliderInput('sld_xpl_opz_spt_cmn_trb', 'TRASPARENZA BORDO:', min = 0, max = 10, value = 2, step = 1, ticks = FALSE)

                    ),
        		    
        		)), br()
                
            ### FINE DI OPZIONI SPORTELLI
            )), br(),
            
            ### OPZIONI BACINI ----------------------
    		a(id = 'tgl_xpl_opz_bcn', 'Show/Hide BACINI', class = 'toggle-choices-sub'),
    		hidden( div(id = 'hdn_xpl_opz_bcn', class = 'toggle-choices-subcontent',
                p(),

                #### OPZIONI BACINI: GENERALE ------
        		a(id = 'tgl_xpl_opz_bcn_gen', 'Show/Hide GENERALI', class = 'toggle-choices-sub'),  
        		hidden( div(id = 'hdn_xpl_opz_bcn_gen', class = 'toggle-choices-subcontent',
                    p(),
                    
                    checkboxInput('chk_xpl_opz_bcn_gen_lgn', 'AGGIUNGI LEGENDA', TRUE),
                    
                    conditionalPanel("input.chk_xpl_opz_bcn_gen_lgn",
                                     
                        selectInput('cbo_xpl_opz_bcn_gen_lgn', 'POSIZIONE:', choices = pos.lst, selected = 'bottomright'),
                        
                    ),
                    
                    checkboxInput('chk_xpl_opz_bcn_gen_ttl', 'AGGIUNGI TITOLO', TRUE),
                    
                    conditionalPanel("input.chk_xpl_opz_bcn_gen_ttl",
                                     
                        selectInput('cbo_xpl_opz_bcn_gen_ttl', 'POSIZIONE:', choices = pos.lst, selected = 'bottomleft')
                        
                    )
                    
    			)), br(),
        		
                #### OPZIONI BACINI: POLIGONI ------
        		a(id = 'tgl_xpl_opz_bcn_pgn', 'Show/Hide POLIGONI', class = 'toggle-choices-sub'),  
        		hidden( div(id = 'hdn_xpl_opz_bcn_pgn', class = 'toggle-choices-subcontent',
                    p(),
                    
                    awesomeRadio('rdo_xpl_opz_bcn_plg_tcp', 'TEMATISMO:', choices = c('COLOURS' = 'C', 'PALETTE' = 'P'), selected = 'P'),
                    
        		    conditionalPanel("input.rdo_xpl_opz_bcn_plg_tcp == 'P'",
        		                     
                        pickerInput('cbo_xpl_opz_bcn_plg_pal', 'PALETTE:', choices = palettes.lst, selected = 'RdYlBu'),
                        
                        prettySwitch('swt_xpl_opz_bcn_plg_pal', 'INVERTI COLORI', status = 'success', fill = TRUE )
                        
        		    ),
        		    
        		    conditionalPanel("input.rdo_xpl_opz_bcn_plg_tcp == 'C'",
        		                     
                        colourpicker::colourInput('col_xpl_opz_bcn_plg_clw', 'COLORE VALORI MINORI:', '#CC9710', returnName = FALSE),
                        
                        colourpicker::colourInput('col_xpl_opz_bcn_plg_cmd', 'COLORE VALORI MEDI:', '#ECFF59', returnName = FALSE),
                        
                        colourpicker::colourInput('col_xpl_opz_bcn_plg_chg', 'COLORE VALORI MASSIMI:', '#0AC75F', returnName = FALSE)
                        
        		    ),
        		    
        			sliderInput('sld_xpl_opz_bcn_plg_trp', 'TRASPARENZA:', min = 0, max = 10, value = 3, step = 1, ticks = FALSE)
        			
    			)), br(),
        			    
                #### OPZIONI BACINI: BORDI ---------
        		a(id = 'tgl_xpl_opz_bcn_brd', 'Show/Hide BORDI', class = 'toggle-choices-sub'),  
        		hidden( div(id = 'hdn_xpl_opz_bcn_brd', class = 'toggle-choices-subcontent',
                    p(),
                    
                    sliderInput('sld_xpl_opz_bcn_brd_bsz', 'SPESSORE:', min = 0, max = 20, value = 4, step = 1, ticks = FALSE),
                    
                    colourpicker::colourInput('col_xpl_opz_bcn_brd_bcl', 'COLORE:', 'black', showColour = 'background'),
                    
        			sliderInput('sld_xpl_opz_bcn_brd_btp', 'TRASPARENZA:', min = 0, max = 10, value = 3, step = 1, ticks = FALSE),
        			
                    sliderInput('sld_xpl_opz_bcn_brd_hsz', 'SPESSORE EVIDENZIATO:', min = 1, max = 20, value = 6, step = 1, ticks = FALSE),
        			
                    colourpicker::colourInput('col_xpl_opz_bcn_brd_hcl', 'COLORE EVIDENZIATO:', 'white', showColour = 'background')
        			
    			))

            ### FINE DI OPZIONI BACINI
    		)), br(),
                
            ### OPZIONI AREE CIRCOLARI ------------
 		    conditionalPanel("input.chk_xpl_bcn_crc",
 		                     
        		a(id = 'tgl_xpl_opz_arc', 'Show/Hide AREE CIRCOLARI', class = 'toggle-choices-sub'),
        		hidden( div(id = 'hdn_xpl_opz_arc', class = 'toggle-choices-subcontent',
                    p(),
    
                    sliderInput('sld_xpl_opz_arc_sps', 'SPESSORE:', min = 0, max = 10, value = 4, step = 1, ticks = FALSE),
                    
                    colourpicker::colourInput('col_xpl_opz_arc_col', 'COLORE:', 'black', showColour = 'background')
                    
        		)), br()
        		
            #### FINE DI OPZIONI AREE CIRCOLARI
 		    ),
                
        ### FINE DI OPZIONI
        )), br(), 

        ## DOWNLOAD ------------------------------
        a(id = 'tgl_xpl_dwn', 'Mostra/Nascondi DOWNLOAD', class = 'toggle-choices'),  
        hidden( div(id = 'hdn_xpl_dwn', class = 'toggle-choices-content',
            p(),

            radioButtons('rdo_xpl_dwn', 'TIPO:', c('Mappa Interattiva (HTML)' = 'html', 'Report (PDF)' = 'pdf', 'Tabella Dati (CSV)' = 'csv')),
            
            textInput('txt_xpl_dwn', 'NOME (max 50 caratteri)'), 
            
            downloadButton('dwn_map', 'SCARICA', class = 'btndwn')

        ## FINE DI DOWNLOAD
        )), br(), 

        ## SESSIONE ------------------------------
        a(id = 'tgl_sss', 'Show/Hide SESSIONS', class = 'toggle-choices'),  
        hidden( div(id = 'hdn_sss', class = 'toggle-choices-content',
            p(),
            
            textInput('txt_sss_sve', 'ENTER SESSION NAME (max 50 characters)', value = paste0('usrname', '_', as.character(Sys.time())) ), 
            actionButton('btn_sss_sve', 'SAVE CURRENT'), hr(),
            
            pickerInput('cbo_sss_lod', 'SAVED SESSIONS:', choices = c('one', 'two', 'three')),
            div(style = 'display:inline-block;text-align:center',
                actionButton('btn_sss_lod', 'LOAD'),
                actionButton('btn_sss_del', 'DELETE') 
            )
            
            ## FINE DI SESSIONE
        )),
        
        width = 3

    # FINE DI SIDEBAR PANEL
    
    ),

    # MAIN PANEL --------------------------------
    mainPanel(

        br(),
        fluidRow(
            column(
                width = 2,                     
                actionButton('btn_mps_upt', 'AGGIORNA MAPPA', icon = icon('refresh'), style = 'fill', color = 'danger')
            ),
            column(
                width = 10,                     
                progressBar(
                    'prg_mps_upt', 0, display_pct = TRUE, status = 'info', 
                    title = 'Controlla le opzioni, poi clicca il bottone per creare la mappa'
                )
            )
        ), 
        withSpinner( leafletOutput('outmap', height = 800) ),

        p(),

        fluidRow(

            column(6, 

                h4('Aziende per Settore e Forma Giuridica'),

                uiOutput('out_tbl')

            ),

            column(6, 

                h4('Popolazione per Classi di Età'),

                plotOutput('out_plt')

            )

        )

    )

    # FINE DI MAIN PANEL
    
)
