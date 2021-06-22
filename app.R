library(shiny)
library(eulerr)
library(tidyverse)
library(cowplot)
library(shinyWidgets)
library(rclipboard)
library(emo)

ui <- fluidPage(
    rclipboardSetup(),
    tags$head(HTML(paste0('<link rel="icon" href="data:image/svg+xml,<svg xmlns=%22http://www.w3.org/2000/svg%22 viewBox=%220 0 100 100%22><text y=%22.9em%22 font-size=%2290%22>🚀</text></svg>">'))),
    tags$title('RSP!'),
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
    tags$img(id = 'thumb', src = 'thumb.png'),
    tags$meta(property = 'og:image', src = 'thumb.png'),
    tags$div(id = 'main_container',
    sidebarLayout(
          sidebarPanel(
            tags$h1("ready,\nset,\nplot!",emo::ji("rocket")),
            tags$p("Welcome to the this easy set overlap tool. First set the desired amount of groups, then fill the box. Have a nice plot!"),
            
              fluidRow(
                  column(width = 6, actionButton("add_btn", "Add group", icon = icon('plus'))),
                  column(width = 6, actionButton("rm_btn", "Remove Group", icon = icon('minus')))
              ),
            tags$br(),
              uiOutput("textbox_ui")
          ),
          mainPanel(
              tabsetPanel(type = 'pills',
                  tabPanel(icon('image'),
                           tags$div(id = 'my_slider', tags$div(class = 'left_label',"Plot size"),
                                    tags$div(class = 'right_slider',
                           sliderInput(inputId = "plot_size",
                                       label = "", width = "100%",                            
                                       min = 0, max = 650, step = 1, value = 200, ticks = FALSE))),
                           # awesomeRadio(
                           #   inputId = "plot_type",
                           #   label = NULL, 
                           #   choices = list("Euler" = 1, "Venn" = 2), 
                           #   selected = 2,
                           #   inline = TRUE, 
                           #   checkbox = TRUE
                           # ),
                           radioGroupButtons(
                             inputId = "plot_type",
                             choices = list("Euler Plot" = 1, "Venn Diagram" = 2),
                             selected = 1,
                             status = "secundary",
                             width = "100%"
                           ),
                           # radioButtons(inputId = "plot_type", label = "",
                           #              choices = list("Euler" = 1, "Venn" = 2), 
                           #              selected = 1),
                           plotOutput("my_plot")
                           # 
                           )
                  ,
                  tabPanel(icon('table'),
                      tags$br(),
                      uiOutput("group_selector"),
                      DT::dataTableOutput("my_table"),
                      fluidRow(
                          column(width = 3,uiOutput("copy_button")),
                          column(width = 3,downloadButton("downloadData", "Download Full Table"))
                      )
                  )
              )
          )
    ),
    tags$div(id = "footer_container",
             "ready-set-plot v0.1 · plot package by",a(icon('github'),"jolars/eulerr",href="https://github.com/jolars/eulerr"), " · shinyapp by",a(icon('github'),"barreiro-r/ready-set-plot",href="https://github.com/barreiro-r/ready-set-plot"))
    )
)

'%!in%' <- function(x,y)!('%in%'(x,y))

do_eulerr  <- function(my_matrix) {
    euler(my_matrix)  
}

do_venn  <- function(my_matrix) {
  venn(my_matrix)  
}


make_matrix <- function(my_list, my_groups){
    total_df <- data.frame(genes=character())
    for (i in seq_len(length(my_list))) {
        parcial  <- data.frame(genes=as.character(my_list[[i]]), has=TRUE, group=my_groups[i], stringsAsFactors = FALSE)
        
        total_df <- rbind(total_df, parcial)
    } 
    total_df <- spread(data = total_df, key = "group", value = "has")
    total_df[is.na(total_df)] <- FALSE
    
    colnames_matrix <- colnames(total_df %>% select(-genes))
    rownames_matrix <- total_df %>% pull(genes)
    
    total_matrix <- as.matrix(total_df %>% select(-genes))
    colnames(total_matrix) <- colnames_matrix
    rownames(total_matrix) <- rownames_matrix
    
    total_matrix
}


server <- function(input, output, session) {

    counter <- reactiveValues(n = 2)
    observeEvent(input$add_btn, {counter$n <- counter$n + 1})
    observeEvent(input$rm_btn, {
        if (counter$n > 2) counter$n <- counter$n - 1
    })
    
    textboxes <- reactive({
        
        n <- counter$n
        
        if (n > 0) {
            lapply(seq_len(n), function(i) {
                list(
                    textInput(inputId = paste0("textin", i), label = paste0("Group ", sprintf("%02.0f", i)), value = paste0("G",i)),
                    textAreaInput(paste0("textin_text", i), label = NULL, rows = 3, value = paste0("a\nb\nc\n",i))
                )
            })
        }
        
    })
    
    output$textbox_ui <- renderUI({ textboxes() })
    
    # --------------------------------------------------------------------------
    
    groups_names <- reactive({
        n <- counter$n
        s <- c()
        for (i in seq_len(n)) {
            s <- c(s,input[[paste0("textin", i)]])
        }
        
        s
    })

    groups_itens <- reactive({
        n <- counter$n
        my_list <- list()
        for (i in seq_len(n)) {
            my_list[[i]] <- strsplit(input[[paste0("textin_text", i)]],"\n")[[1]]
        }
        
        my_list
    })
    
    my_matrix <- reactive({
        req(input$textin1,input$textin2,input$textin_text1,input$textin_text2)
        make_matrix(groups_itens(), groups_names())
    })
    
    display_table <- reactive({
        my_matrix <- my_matrix()
        rownames_matrix <- rownames(my_matrix)
        colnames_matrix <- colnames(my_matrix)
        
        n_col <- length(input$selected_groups)
        my_df  <- as.data.frame(my_matrix)
        
        colnames(my_df) <- colnames_matrix
        rownames(my_df) <- c()
        my_df$genes <- rownames_matrix
        my_df <- my_df %>% select(genes,everything())
        
        # my_df <- my_df[ ,]
        
        white_lines <- apply(my_df %>% select(input$selected_groups), MARGIN = 1, FUN = sum) == n_col
        my_df <- my_df[white_lines,]
        
        my_df
    })
    
    my_copy <- reactive({
        display_table() %>% pull(genes) %>% paste(collapse = "\n")
    })
    
    output$my_table <- DT::renderDataTable({
        req(input$textin1,input$textin2,input$textin_text1,input$textin_text2, input$selected_groups)
        display_table()
    })
    
    output$copy_button <- renderUI({
        rclipButton(inputId = "copy", label = "Copy Overlap", icon = icon("clipboard"), clipText = my_copy())
    })
    
    
    my_plot_size <- reactive({input$plot_size})
    
    output$my_plot <- renderPlot({
        req(input$textin1,input$textin2,input$textin_text1,input$textin_text2)
      if(input$plot_type == 1){
        plot(do_eulerr(my_matrix()),labels = TRUE, quantities = TRUE)
      } else {
        plot(do_venn(my_matrix()),labels = TRUE, quantities = TRUE)
      }
    }, height = my_plot_size, width = my_plot_size)
    
    teste <- reactive({
      print(input$plot_type)
    })
    
    output$group_selector <- renderUI({
        req(input$textin1,input$textin2)
        pickerInput(inputId  = "selected_groups",
                    label    = "Must be in",
                    choices  = groups_names(),
                    selected = groups_names(),
                    options  = list(`actions-box` = TRUE),
                    multiple = TRUE,
                    width    = '100%')
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste(Sys.Date(),'-eulerr', '.tsv', sep='')
      },
      content = function(file) {
        write_delim(display_table(), file,quote = FALSE, delim = "\t")
      }
    )
    
}

shinyApp(ui, server)