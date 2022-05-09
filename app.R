#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(shinythemes)
library(DT)
library(thematic)
library(waiter)
thematic_shiny()

on_server <- grepl("shiny-server", getwd())

if(on_server){
  g_result_dir <<- "../retreat2022/output/results"
  g_cache_dir <<- "cache"
  options(shiny.autoreload = TRUE)
} else{
  g_result_dir <- "data/from_server"
  g_cache_dir <<- "data/cache"
}

source("analysis.R")
source("plot_util.R")


setup_workspace(g_result_dir, g_cache_dir)

var_choices <- setdiff(names(master), c("p_id",
                                       "session.time_started", 
                                       "time_ended", 
                                       "pilot", 
                                       "session.complete", 
                                       "session.test_duration_min", 
                                       "num_restarts",
                                       names(free_text_items)
                                       ))
var_types <- c("categorial", "numeric")[1 + map_lgl(var_choices, ~{(master[[.x]] %>% class())[1] == "numeric"})]
var_data <- tibble(variable = var_choices, type = var_types)
var_data <- tibble(variable = var_choices, type = var_types)


theme_set(get_default_theme())

get_intro_text <- function(){
  div(h3("Welcome to the MPIAE Retreat 2022 Debrief Monitor App"), 
         p("This app allows you visualize and inspect the data from a Retreat Debriefing questionnaire",
           "carried out by the  Max Planck Institute for Empirical Aesthetics, Frankfurt/M., Germany"),
      p("Have fun!"),
      style = "width:50%;text-align:justify")
}

impressum <- function(){
    shiny::p(
        "MPIAE Retreat 2022 Debrief Monitor  v0.1", 
        shiny::tags$br(), 
        shiny::tags$br(), 
        "Author: Klaus Frieler", 
        shiny::tags$br(), 
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(), 
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        shiny::tags$br(),
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        style = "font-size: 10pt; display: block"
    )
    
}

input_width <- 300

ui_new <-   
    shiny::shinyUI(
        navbarPage(
          title = "MPIAE Retreat 2022 Debrief Monitor", 
            #theme = shinytheme("yeti"),
            theme = bslib::bs_theme(version = 4, 
                                    bootswatch = "sketchy", 
                                    base_font = bslib::font_google("Nunito")
                                    #base_font = bslib::font_google("Fira Mono")
                                    ),
            id = "tabs",
            tabPanel(
                "Home",
                sidebarLayout(
                    sidebarPanel(
                      impressum(),
                      downloadButton("download_all_data_xlsx", "Download data (Excel)", style = "margin: 5px;width:80%;font-size:12pt"),

                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        useWaiter(), 
                        waiterPreloader(html = spin_2(), color ="white"),                      
                        htmlOutput("introduction"),
                        h4("Summary"),
                        tableOutput("overall_stats")
                    )
                    
                )
            ),
            tabPanel(
                "Univariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("uv_variable", "Variable:", var_choices, selected = "overall_satisfaction", multiple = F), 
                        shiny::div(
                          checkboxInput("uv_group_by_status", "Group by Status", FALSE),
                          style = "margin-left:15px"),
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                      textOutput("item_wording"),
                      plotOutput("univariate_plot", width = "800px")
                    )
                    
                )
            ),            
            tabPanel(
                "Bivariate",
                sidebarLayout(
                    sidebarPanel(
                        selectizeInput("bv_variable1", "Variable X:", var_choices, selected = "overall_satisfaction", multiple = F), 
                        selectizeInput("bv_variable2", "Variable y:", var_choices, selected = "organization", multiple = F), 
                        actionButton("switch_axes", 
                                     label = "Switch axes", style = "margin-bottom: 10px"),
                        impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                      uiOutput("item_wording2"),
                      plotOutput("bivariate_plot", width = "800px"),
                      tableOutput("corr_tab")
                    )
                    
                )
            ),            
          tabPanel(
            "Free Text",
            sidebarLayout(
              sidebarPanel(
                selectizeInput("ft_variable", "Variable:", names(free_text_items), selected = "highlight", multiple = F), 
                impressum(),
                width = 2
              ),
              
              # Main panel for displaying outputs ----
              mainPanel(
                uiOutput("free_text"),
              )
              
            )
          ),            
          
            tabPanel(
                "Data",
                sidebarLayout(
                    sidebarPanel(
                      impressum(),
                        width = 2
                    ),
                    
                    # Main panel for displaying outputs ----
                    mainPanel(
                        DT::DTOutput("raw_data")
                    )
                    
                )
            )))

apply_filters <- function(data, input){
  # tabs <- input$tabs
  # #browser()
  # filter_val <- input[[sprintf("include_filter_%s", tolower(tabs))]] %>% tolower()
  # if(tabs != "Home"){
  #   filter_val <- input[[sprintf("include_filter_%s", tolower(tabs))]] %>% tolower()
  # }
  # else{
  #   return(data)
  # }
  # if(substr(filter_val, 1, 1)  != "-"){
  #   data <- data %>% filter(SEL.status == filter_val)
  # }
  data
}

# Define server logic required to draw a plot
server <- function(input, output, session) {

  message("*** STARTING APP***")
  #browser()
  check_data <- reactiveFileReader(5000, NULL, g_result_dir, setup_workspace, g_cache_dir)

  shiny::observeEvent(input$switch_axes, {
    x <- input$bv_variable1

    y <- input$bv_variable2
    updateSelectizeInput(session, inputId = "bv_variable1",
                         selected = y)
    updateSelectizeInput(session, inputId = "bv_variable2",
                          selected = x)
     
  })
   
  output$introduction <- renderUI({
    get_intro_text()
  })
   
  output$overall_stats <- renderTable({
    check_data()
    if(nrow(master) == 0){
      return()
    }
    p_id_stats <- master %>% 
      distinct(p_id, session.complete ) %>% 
      summarise(
                # n_female = sum(gender == "Female", na.rm = T), 
                # n_male = sum(gender == "Male", na.rm = T), 
                # n_other = sum(gender == "Other", na.rm = T), 
                # n_not_say = sum(gender == "Rather not say", na.rm = T), 
                n_unique = n(),
                n_complete = sum(session.complete, na.rm = T),
                .groups = "drop")
    p_id_stats %>% 
      select(n_unique, n_complete,  everything()) %>% 
      set_names("Total N", "Completed") 
    # select(n_unique, n_complete, starts_with("n"), everything()) %>% 
    #   set_names("Total N", "Completed", "Females", "Males", "Other", "Rather not say") 
  })
   
  output$raw_data <- renderDataTable({
    #check_data()
    if(nrow(master) == 0){
      return()
    }
    data <- apply_filters(master, input)

    data %>%
      select(-c(p_id)) %>%
      mutate_if(is.numeric, round, 2) %>%
      select(session.time_started, session.complete, everything())

   }, options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
    

  output$univariate_plot <- renderPlot({
    #check_data()
    #update_workspace(g_result_dir, g_cache_dir)
    if(nrow(master) == 0){
      return()
    }
    data <- apply_filters(master, input)
    group_by_status <- input$uv_group_by_status
    #data <- master
    var_info <- var_data %>% filter(variable == input$uv_variable)
    if(nrow(var_info) == 0){
      return()
    }
    if(var_info$type == "numeric"){
      q <- univariate_plot_numeric(data, input$uv_variable, remove_na = T)
      if(input$uv_variable %in% names(likert_items)){
        q <- univariate_plot_categorial(data %>% 
                                          mutate(!!sym(input$uv_variable) := factor(!!sym(input$uv_variable), levels = 1:5)),
                                        input$uv_variable,  remove_na = T, coord_flip = F)
        q <- q  + scale_x_discrete(drop = FALSE)
      }
    } 
    else if (var_info$type == "categorial"){
      if(input$uv_variable %in% expand_variables){
        data <- data %>% get_expanded_df(input$uv_variable)
      }
      coord_flip <- n_distinct(data[[input$uv_variable]]) > 3
      q <- univariate_plot_categorial(data, input$uv_variable,  remove_na = T, coord_flip = coord_flip)
      }
    else {
      return()
      }
    if(group_by_status ) {
      q + facet_wrap(~status)
    }
    else{
      q
    }
   })
  
  output$item_wording <- renderText({
    all_items[[input$uv_variable]]$prompt
  })
  
  output$item_wording2 <- renderUI({
    shiny::p(sprintf("X: %s", all_items[[input$bv_variable1]]$prompt)[[1]], 
             shiny::br(),
             sprintf("Y: %s", all_items[[input$bv_variable2]]$prompt)[[1]])
  })

  output$free_text <- renderUI({
    data <- apply_filters(master, input)
    browser()
    shiny::div(
      shiny::h4(free_text_items[[input$ft_variable]]),
      shiny::tags$ul(shiny::tagList(map(data[[input$ft_variable]], shiny::tags$li)))
    )
  })
  
  output$bivariate_plot <- renderPlot({
    #check_data()
    #update_workspace(result_dir, cache_dir)
    if(nrow(master) == 0){
      return()
    }
    
    data <- apply_filters(master, input)
    #data <- master
       
    #browser()
    if(input$bv_variable1 == input$bv_variable2){
      return()
    }
    bv1 <- input$bv_variable1
    bv2 <- input$bv_variable2
    if(bv1 %in% expand_variables && bv2 %in% expand_variables){
      return()
    }
    if(bv1 %in% expand_variables){
      data <- data %>% get_expanded_df(bv1, bv2)
    }
    if(bv2 %in% expand_variables){
      data <- data %>% get_expanded_df(bv2, bv1)
    }
    
    bivariate_plot_auto(data, input, var_data, remove_na = T)   
  })
   
  output$corr_tab <- renderTable({
    #check_data()
    if(nrow(master) == 0){
        return()
    }
      
    data <- apply_filters(master, input)
    vars <- get_parameters(data, input, var_data = var_data)
    if(vars$sub_type == "num-num" && input$bv_variable1 != input$bv_variable2) {
      get_correlations(data, input$bv_variable1, input$bv_variable2)   
    }
    },  caption = "Correlations", caption.placement = getOption("xtable.caption.placement", "top"))
   
    output$download_all_data_csv <- downloadHandler(
      filename = "debrief_data.csv",
      content = function(file) {
        #check_data()
        if(nrow(master) == 0){
          return()
        }
        dec <- ifelse(input$dec, ",", ".") 
        data <- apply_filters(master, input)
        write.table
        write.table(data %>% mutate_if(is.logical, as.integer), 
                    file, 
                    row.names = FALSE, 
                    dec = dec, 
                    sep = ";", 
                    quote = T, 
                    fileEncoding = "utf-8")
      }
    )

    output$download_all_data_xlsx <- downloadHandler(
      filename =  "debrief_data.xlsx",
      content = function(file) {
        check_data()
        if(nrow(master) == 0){
          return()
        }
        data <- apply_filters(master, input)
        #messagef("%d", nrow(data))
        writexl::write_xlsx(data %>% mutate_if(is.logical, as.integer), path = file)
        
      }
    )
    
}

# Run the application 
shinyApp(ui = ui_new, server = server)

