############################################################
##  PACKAGES AND SCRIPTS
############################################################
library(shiny)
library(shinyjs)
# library(bslib)
library(tidyverse)
library(ggforce)
library(readxl)
theme_set(theme_void())
library(googlesheets4)
options(gargle_oauth_cache = ".secrets")
gs4_auth(cache = ".secrets", 
         email = as.character(read.csv("access_email", header = F)[1]))
sheet_id <- as.character(read.csv("google_sheet_id", header = F)[1])

# Load Figtree font (replaces Montserrat)
library(showtext)             # For custom fonts
font_add_google("Figtree", "Figtree", db_cache = FALSE)
showtext_auto()
source("radar_plot.R")
source("process_file.R")
source("observeEvent.R")
# source("read_gdoc.R")
# source("input_form.R")


############################################################
##  CATEGORIES
############################################################
cats <- data.frame(abbr = c("", "DS", "CB", "PS", "IM", "AP", "SE"),
                   full = c("",
                            "Data-sharing partnerships & management tools",
                            "City/institutional buy-in, capacity building & training",
                            "Public support & engagement",
                            "Health and economic impact modelling",
                            "Estimating air pollution",
                            "Source & emission inventories"))

############################################################
##  DOMAINS and EQUITY THEMES
############################################################
dims <- data.frame(abbr = c("", "TC", "DI", "IP", "PE", "DE", "SO", "AI"),
                   full = c("",
                            "Technical Capacity",
                            "Data Integration",
                            "Internal Processes",
                            "Participation and Empowerment",
                            "Distributional Equity",
                            "Socioeconomic Resilience and Opportunity",
                            "Access and Inclusivity"))


############################################################
##  UI
############################################################
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  useShinyjs(),
  
  
  # Landing page
  div(id = "landing",
      div(style = "flex:1;", img(src = "equipd_logo.png", style = "width:800px; height:106px; margin-left:20px;")),
      div(style = "background-color: #F9DA5B; align-items: bottom; text-align:center; place-items:center;
          flex: 1; display: flex; align-items: center; flex-direction: column;", 
          div( passwordInput("access", label = "Please enter access code to begin using the EQuiPD tool:"),
               style = "margin-top:auto;"),
          div(actionButton("access_button", label = "Go"),
              style = "margin-bottom:auto;"),
      ),
      div(style = "justify-content: right; align-items:bottom; flex:1;",
          img(src = "c40-logo.svg", 
              style = "padding:5px; margin:5px; border-style:solid; border-width:3px; 
                   border-color:black; width: 100px; height:100px;
                  position: absolute; bottom: 0; right: 0;"))
  ),
  
  
  # Main content
  div(id = "content",
      div(img(src = "equipd_logo.png", style = "width:400px; height:53px;"), 
          style = "display:flex; align-items:left; margin:20px;"),
      sidebarLayout(
        sidebarPanel(width = 3,
                     
                     downloadButton("downloadInst", "Before you begin"), br(),
                     
                     # Download
                     h4("Step 1: Complete assessment"),
                     "a) Download: ",
                     downloadButton("downloadExcel", "Excel"),
                     downloadButton("downloadPDF", "PDF"),
                     
                     # Upload .xlsx
                     fileInput('upload', 'b) Upload assessment (Excel files (.xlsx) only)', 
                               accept = c(".xlsx"), multiple = FALSE),
                     
                     # Display results (radar plot)
                     h4("Step 2: Visualize results"),
                     selectInput("city", label = "a) Select a city", choices = ""),
                     selectInput("response1", label = "b) Select baseline", choices = ""),
                     selectInput("response2", label = "c) Select reassessment (if completed multiple times)", choices = ""),
                     
                     div( img(src='key.png', width = 225), align = "center"),
                     br(),
                     
                     # Scoring breakdown
                     # hr(style="border: none; height: 2px; background-color: black;"),
                     h4("Step 3: Identify Strengths and Opportunities"),
                     selectInput("category", label = "a) Select category", 
                                 choices = cats$full),
                     selectInput("dimension", label = "b) Select domain/theme", 
                                 choices = dims$full),
                     selectInput("comparison_city", label = "c) Select comparison", 
                                 choices = c(""))
        ),
        
        
        mainPanel(
          imageOutput("radar", height = 750, width = 900),
          htmlOutput("score_comparison"), br(),
          tableOutput("score_table")
        )
      ))
)

############################################################
##  SERVER
############################################################
server <- function(input, output, session) {
  observeEvent(input$access_button, {
    if (input$access == as.character(read.csv("password", header = F)[1])) {
      shinyjs::hide("landing")
      shinyjs::show("content")
    } else {
      showNotification(paste("Incorrect access code"), type = "error")
    }
  })
  
  
  # Radar plot values
  radar_data <- reactiveVal(data.frame())
  radar_data2 <- reactiveVal(data.frame())
  
  # Scoring breakdown
  score_cat <- reactiveVal("")
  score_dim <- reactiveVal("")
  question_dat <- reactiveVal(data.frame())
  compare_dat <- reactiveVal(data.frame())
  
  # Scoring data
  responses <- reactiveVal(
    read_sheet(sheet_id, sheet = "Summary"))
  
  observeEvent(responses(), {
    data <- responses()
    updateSelectInput(session, "city", 
                      choices = c("", unique(data$city)), 
                      selected = input$city)
    updateSelectInput(session, "response1",
                      choices = c("", unique(data$name_date[which(data$city == input$city)])),
                      selected = unique(data$name_date[which(data$city == input$city)])[1])
    updateSelectInput(session, "response2",
                      choices = c("", unique(data$name_date[which(data$city == input$city &
                                                                    data$name_date != input$response1)])),
                      selected = "")
  })
  
  
  # Initiate download
  output$downloadInst <- downloadHandler(
    filename = function() {
      return("EQuiPD_Webapp_Instructions.pdf")
    },
    content = function(file) {
      myfile <- srcpath <-  "./www/EQuiPD_Webapp_Instructions.pdf"
      file.copy(myfile, file)
    }
  )
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste("EQuiPD", ".xlsx", sep='')
    },
    content = function(file) {
      myfile <- srcpath <-  "./www/EQuiPD_260227.xlsx"
      file.copy(myfile, file)
    }
  )
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste("EQuiPD", ".pdf", sep='')
    },
    content = function(file) {
      myfile <- srcpath <-  "./www/EQuiPD_260227.pdf"
      file.copy(myfile, file)
    }
  )
  
  
  
  # Save uploaded assessment
  observeEvent(input$upload, {
    file <- read_xlsx(input$upload$datapath, sheet = "Self-Assessment", range = "A1:T300")
    
    # Check valid file (should have EQuiPD in row 1 col 1)
    if (colnames(file)[1] != "EQuiPD") {
      showNotification(paste("Invalid document uploaded"), type = "error")
      return()
    }
    
    # Read other sheets and process/record
    rec <- read_xlsx(input$upload$datapath, sheet = "Recommendations")
    org <- read_xlsx(input$upload$datapath, sheet = "org_detail")
    processed <- process_file(input$upload$datapath, file, rec, org, responses())
    # Check that there were no errors in processing file
    if (processed == TRUE) {
      showNotification(paste("File saved"), type = "message")
    } else {
      showNotification(paste(processed), type = "error")
    }
    
    # Update shiny app form responses
    responses(read_sheet(sheet_id, sheet = "Summary"))
  })
  
  
  # Change City input
  observeEvent(input$city, {
    dat <- responses()
    radar_data(data.frame())
    radar_data2(data.frame())
    updateSelectInput(session, "response1",
                      choices = c("", dat$name_date[which(dat$city == input$city)]),
                      selected = "")
    updateSelectInput(session, "response2",
                      choices = "",
                      selected = "")
    updateSelectInput(session, "category", choices = cats$full, selected = "")
    updateSelectInput(session, "dimension", choices = dims$full, selected = "")
    updateSelectInput(session, "comparison_city", choices = "", selected = "")
  })
  
  # Change baseline input
  observeEvent(input$response1, {
    dat <- responses()
    radar_data2(data.frame())
    if (input$city != "" & input$response1 != "") {
      radar_data(dat[which(dat$city == input$city & dat$name_date == input$response1), ])
      question_dat(read_sheet(sheet_id, 
                              sheet = radar_data()$filename[1]))
    }
    updateSelectInput(session, "response2",
                      choices = c("", dat$name_date[which(dat$city == input$city &
                                                            dat$name_date != input$response1)]),
                      selected = "")
    updateSelectInput(session, "category", choices = cats$full, selected = "")
    updateSelectInput(session, "dimension", choices = dims$full, selected = "")
    updateSelectInput(session, "comparison_city", choices = "", selected = "")
  })
  
  # Change reassessment input
  observeEvent(input$response2, {
    dat <- responses()
    if (input$response2 == "") {
      radar_data2(data.frame())
    } else {
      radar_data2(dat[which(dat$city == input$city & dat$name_date == input$response2), ])
      question_dat(read_sheet(sheet_id, 
                              sheet = radar_data2()$filename[1]))
    }
    updateSelectInput(session, "category", choices = cats$full, selected = "")
    updateSelectInput(session, "dimension", choices = dims$full, selected = "")
    updateSelectInput(session, "comparison_city", choices = "", selected = "")
  })
  
  # Change comparison city
  observeEvent(input$comparison_city, {
    dat <- responses()
    if (input$comparison_city == "") {
      compare_dat(data.frame())
    } else {
      id <- strsplit(input$comparison_city, ": ")[[1]]
      filename <- dat$filename[which(dat$city == id[1] & dat$name_date == id[2])][1]
      compare_dat(read_sheet(sheet_id, 
                             sheet = filename))
    }
  })
  
  
  # Change category/dimension input
  observeEvent(input$category, { 
    score_cat(cats[cats$full == input$category, "abbr"])
  })
  observeEvent(input$dimension, { 
    score_dim(dims[dims$full == input$dimension, "abbr"])
  })
  
  # Plot responses
  output$radar <- renderImage({
    req(radar_data())
    out <- list(src = "www/radar.png", alt = "EQuiPD Radar Plot")
    if (nrow(radar_data()) != 0) {
      outfile <- tempfile(pattern = "EQuiPD_plot", fileext = ".png")
      g <- build_geoms(radar_data())
      g2 <- build_geoms(radar_data2())
      png(outfile, width = 900, height = 750, res = 96)
      print(build_radar(g, g2))
      dev.off()
      out <-list(src = outfile, alt = "EQuiPD Radar Plot")
    }
    return(out)
  }, deleteFile = FALSE)
  
  # Create comparison diagram
  output$score_comparison <- renderUI({
    text <- ""
    if (score_cat() != "" & score_dim() != "") {
      score_dat <- radar_data()
      if (nrow(radar_data2()) != 0) {
        score_dat <- radar_data2()
      }
      q_idx <- which(score_dat$category == input$category & 
                       (score_dat$domain == input$dimension))
      score <- mean(score_dat$score[q_idx], na.rm = TRUE)
      
      text <- paste0("<h3>Score detail</h3>",
                     "<b>Category:</b> ", input$category,
                     "<br/><b>Domain/Theme:</b> ", input$dimension,
                     "<br/><b>Your score:</b> ", round(score, 2))
      
      other_cities <-
        responses() %>% filter(category == input$category,
                               domain == input$dimension,
                               city != input$city)  %>%
        group_by(city, name_date, filename) %>%
        summarize(Score = round(mean(score, na.rm = TRUE), 2)) %>%
        filter(Score > score)
      
      if (nrow(other_cities) > 0) {
        text <- paste0(text,
                       "<br/><b>Cities (score) exceeding in this area:</b><br/>&nbsp;&nbsp;",
                       paste(mutate(other_cities %>% arrange(desc(Score)), 
                                    new = paste0(city, " (", Score, ") [Assessment taken: ", name_date, "]"))$new, 
                             collapse = "<br/>&nbsp;&nbsp;"))
        updateSelectInput(session, "comparison_city", 
                          choices = c("", paste0(other_cities$city, ": ", other_cities$name_date)),
                          selected = "")
      }
    }
    HTML(text, "<br />")
  })
  
  # Show scores for category & domain/theme
  output$score_table <- renderTable({
    if (score_cat() != "" & score_dim() != "") {
      idx <- which(question_dat()[[score_cat()]] == 1 &
                     question_dat()[[score_dim()]] == 1)
      out <- question_dat()[idx, c("short_q", "Question", "Recommendations", "Response")]
      colnames(out) <- c("#", "Question text", "Recommendations", "Response")
      if (nrow(compare_dat()) != 0) {
        idx <- which(compare_dat()[[score_cat()]] == 1 &
                       compare_dat()[[score_dim()]] == 1)
        out2 <- compare_dat()[idx, c("Question", "Response")]
        colnames(out2) <- c("Question text", "Comparison")
        out <- left_join(out, out2, by = c("Question text")
        )[, c("#", "Question text", "Recommendations", "Response", "Comparison")]
      }
      out[order(out$Response, decreasing = FALSE, na.last = TRUE), ]
    } else {
      data.frame()
    }
  },
  striped = TRUE, hover = TRUE)
}

############################################################
##  RUN APP
############################################################
shinyApp(ui, server)