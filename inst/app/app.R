# ___________________________________ 
#                                     
#           PMT Report App            
#                                     
# ___________________________________ 
#
# Created:       29 January  2024
# Last Updated:  10 December 2025
#
#
# Author:        Brady Rippon
# 
# ___________________________________ 

# load in useful libraries

library(shiny)
library(shinythemes)
library(DT)

library(tidyverse)
library(readxl)
library(readr)
library(expss) # make labels for variables
library(gt) # allow for printable gt objects
library(gtsummary) # make nice looking tables
library(kableExtra) # additional tables styles


# --- UI ----

ui <- fluidPage(
  
  # define aesthetic theme for the app
  theme = shinytheme("united"),
  
  # ---- WCM branding + spacing tweaks ----
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f7f4f2;
      }

      /* Title + headings in WCM-ish red */
      h1, h2, h3, h4 {
        color: #b31b1b;
        font-weight: 600;
      }

      /* Buttons (fileInput, download buttons, etc.) */
      .btn,
      .btn-default,
      .btn-primary {
        background-color: #b31b1b;
        border-color: #b31b1b;
        color: #ffffff;
      }
      .btn:hover,
      .btn-default:hover,
      .btn-primary:hover {
        background-color: #8b1212;
        border-color: #8b1212;
        color: #ffffff;
      }

      /* Labels */
      .shiny-input-container label {
        color: #4a2b2b;
        font-weight: 500;
      }

      /* Reduce vertical whitespace between inputs */
      .shiny-input-container {
        margin-bottom: 10px;
      }

      /* Add breathing room at the very top of the page */
      .wcm-topspacer {
        margin-top: 14px;
      }

      /* Card-like panels */
      .wcm-panel {
        background-color: #ffffff;
        border: 1px solid #e0dad2;
        border-radius: 12px;
        padding: 15px 18px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
        margin-bottom: 15px;
      }

      .wcm-panel h3 {
        margin-top: 0;
      }

      /* Upload panels: make them clearly distinct (wins over .wcm-panel) */
      .wcm-panel.wcm-panel-upload {
        background-color: #e6e6e6 !important;  /* darker grey */
        border-color: #bfbfbf !important;
      }

      /* DataTable header styling */
      table.dataTable thead th {
        background-color: #000000;
        color: #ffffff;
      }

      /* Progress bar color (for upload progress etc.) */
      .progress-bar {
        background-color: #f5b800; /* WCM-ish yellow */
      }
    "))
  ),
  
  # --- Header ---
  div(
    class = "wcm-panel wcm-topspacer",
    h1("PMT Quarterly Report"),
    div(strong("Last updated:"), " ", em("12-10-2025")),
    p(
      "Upload the two REDCap exports below (Main + Follow-up)"
    )
  ),
  
  # --- Upload cards (clean) ---
  fluidRow(
    column(
      width = 6,
      div(
        class = "wcm-panel wcm-panel-upload",
        h3("Main Report"),
        fileInput(
          "file1", label = NULL,
          multiple = FALSE,
          accept = c(".csv"),
          placeholder = "Upload a file",
          buttonLabel = "Browse"
        ),
        p(
          "Download this data from the ",
          span("Vulnerable Elder Protection Team (VEPT)", style = "color:#b31b1b"),
          " REDCap project. The necessary data can be found in the ",
          span("All data", style = "color:#b31b1b"),
          " exports."
        ),
        tags$hr(),
        htmlOutput("Data1Note"),
        checkboxInput("show_preview_main", "Show preview (optional)", FALSE),
        conditionalPanel(
          condition = "input.show_preview_main",
          tags$hr(),
          h4("Main preview"),
          DT::dataTableOutput("data1")
        )
      )
    ),
    column(
      width = 6,
      div(
        class = "wcm-panel wcm-panel-upload",
        h3("Follow-up Report"),
        fileInput(
          "file2", label = NULL,
          multiple = FALSE,
          accept = c(".csv"),
          placeholder = "Upload a file",
          buttonLabel = "Browse"
        ),
        p(
          "Download this data from the ",
          span("VEPT Post Discharge Follow Up", style = "color:#b31b1b"),
          " REDCap project. The necessary data can be found in the ",
          span("All data", style = "color:#b31b1b"),
          " exports."
        ),
        tags$hr(),
        htmlOutput("Data2Note"),
        checkboxInput("show_preview_followup", "Show preview (optional)", FALSE),
        conditionalPanel(
          condition = "input.show_preview_followup",
          tags$hr(),
          h4("Follow-up preview"),
          DT::dataTableOutput("data2")
        )
      )
    )
  ),
  
  # --- Main layout: controls on left, printed tables on right ---
  sidebarLayout(
    
    sidebarPanel(
      class = "wcm-panel",
      
      p(
        strong("Note: "),
        "Please upload the Main and Follow-up exports above before generating the report below."
      ),
      tags$hr(),
      
      textInput(
        inputId = "fiscal_year",
        label = h3("Fiscal Year"),
        value = "",
        placeholder = "e.g., 2021-2022"
      ),
      htmlOutput("fy_help"),
      tags$hr(),
      
      selectInput(
        inputId = "select2", label = h3("Quarter"),
        choices = list("Q1" = "Q1", "Q2" = "Q2", "Q3" = "Q3", "Q4" = "Q4", "Annual" = "Annual")
      ),
      p("Choose a fiscal quarter for your report from the choices above."),
      p(strong("Q1: "), "10/01 - 12/31"),
      p(strong("Q2: "), "01/01 - 03/31"),
      p(strong("Q3: "), "04/01 - 06/30"),
      p(strong("Q4: "), "07/01 - 09/30"),
      p(strong("Annual: "), "10/01 - 09/30"),
      tags$hr(),
      
      h3("Q8 Projections"),
      checkboxInput("show_q8_proj", "Edit Q8 projections", FALSE),
      
      conditionalPanel(
        condition = "input.show_q8_proj",
        helpText("Adjust the expected (projection) counts for each Q8 item below."),
        
        h4("8A. Information & Referral"),
        numericInput("proj_8A",  "8A – Individuals who received services", 23, min = 0),
        numericInput("proj_8A1", "8A1 – Criminal justice info", 16, min = 0),
        numericInput("proj_8A2", "8A2 – Victims rights info", 2, min = 0),
        numericInput("proj_8A3", "8A3 – Referral to victim programs", 30, min = 0),
        numericInput("proj_8A4", "8A4 – Referral to other services", 37, min = 0),
        
        tags$hr(),
        
        h4("8B. Personal Advocacy/Accompaniment"),
        numericInput("proj_8B",   "8B – Individuals who received services", 36, min = 0),
        numericInput("proj_8B1",  "8B1 – Advocacy to emergency medical care", 7, min = 0),
        numericInput("proj_8B2",  "8B2 – Advocacy to forensic exam", 0, min = 0),
        numericInput("proj_8B3",  "8B3 – Law enforcement interview advocacy", 2, min = 0),
        numericInput("proj_8B4",  "8B4 – Individual advocacy", 114, min = 0),
        numericInput("proj_8B5",  "8B5 – Medical forensic exam / evidence collection", 25, min = 0),
        numericInput("proj_8B6",  "8B6 – Immigration assistance", 0, min = 0),
        numericInput("proj_8B7",  "8B7 – Employer/creditor/landlord/academic", 0, min = 0),
        numericInput("proj_8B8",  "8B8 – Child/dependent care assistance", 0, min = 0),
        numericInput("proj_8B9",  "8B9 – Transportation assistance", 0, min = 0),
        numericInput("proj_8B10", "8B10 – Interpreter services", 4, min = 0),
        
        tags$hr(),
        
        h4("8C. Emotional Support / Safety Services"),
        numericInput("proj_8C",  "8C – Individuals who received services", 35, min = 0),
        numericInput("proj_8C1", "8C1 – Crisis intervention (in-person)", 39, min = 0),
        numericInput("proj_8C2", "8C2 – Hotline/crisis line counseling", 0, min = 0),
        numericInput("proj_8C3", "8C3 – On-scene crisis response", 7, min = 0),
        numericInput("proj_8C4", "8C4 – Individual counseling", 74, min = 0),
        numericInput("proj_8C5", "8C5 – Support groups", 0, min = 0),
        numericInput("proj_8C6", "8C6 – Other therapy", 0, min = 0),
        
        tags$hr(),
        
        h4("8D. Shelter / Housing Services"),
        numericInput("proj_8D",  "8D – Individuals who received services", 6, min = 0),
        numericInput("proj_8D1", "8D1 – Emergency shelter or safe house", 0, min = 0),
        numericInput("proj_8D2", "8D2 – Transitional housing", 0, min = 0),
        numericInput("proj_8D3", "8D3 – Relocation assistance", 8, min = 0),
        
        tags$hr(),
        
        h4("8E. Criminal/Civil Justice System Assistance"),
        numericInput("proj_8E",   "8E – Individuals who received services", 5, min = 0),
        numericInput("proj_8E1",  "8E1 – Notification of criminal justice events", 0, min = 0),
        numericInput("proj_8E2",  "8E2 – Victim impact statement assistance", 0, min = 0),
        numericInput("proj_8E3",  "8E3 – Assistance with restitution", 0, min = 0),
        numericInput("proj_8E4",  "8E4 – Civil legal assistance (protection/restraining)", 5, min = 0),
        numericInput("proj_8E5",  "8E5 – Civil legal assistance with family law", 0, min = 0),
        numericInput("proj_8E6",  "8E6 – Other emergency justice-related assistance", 0, min = 0),
        numericInput("proj_8E7",  "8E7 – Immigration attorney assistance", 0, min = 0),
        numericInput("proj_8E8",  "8E8 – Prosecution interview advocacy/accompaniment", 0, min = 0),
        numericInput("proj_8E9",  "8E9 – Law enforcement interview advocacy/accompaniment", 0, min = 0),
        numericInput("proj_8E10", "8E10 – Criminal advocacy/accompaniment", 0, min = 0),
        numericInput("proj_8E11", "8E11 – Other legal advice and/or counsel", 0, min = 0)
      ),
      
      tags$hr(),
      
      downloadButton("downloadData", "Download Quarterly Report"),
      br(), br(),
      downloadButton("download_ssl", "Download SSL Report")
    ),
    
    # Non-conditional report block (always present, like the earlier version)
    mainPanel(
      div(
        class = "wcm-panel",
        
        h1("PMT Report"),
        htmlOutput("reportNote"),
        tags$hr(),
        
        h3("Q1. Record ID of individuals who received services"),
        htmlOutput("Table1Note"),
        DT::dataTableOutput(outputId = "table1"),
        tags$hr(),
        
        h3("Q3A. Record ID of repeat individuals"),
        htmlOutput("Table3ANote"),
        DT::dataTableOutput(outputId = "table3A"),
        p(""),
        h3("Q3B. Record ID of individuals who received new services"),
        htmlOutput("Table3BNote"),
        DT::dataTableOutput(outputId = "table3B"),
        tags$hr(),
        
        h3("Q4. Demographics (for NEW individuals identified in Question 3)"),
        DT::dataTableOutput(outputId = "table4"),
        br(),
        downloadButton("download_q4_ids", "Download IDs for Q4"),
        tags$hr(),
        
        h3("Q5. Types of Victimizations (for ALL individuals identified in Question 1 & 2)"),
        DT::dataTableOutput(outputId = "table5"),
        DT::dataTableOutput(outputId = "table5.other"),
        br(),
        downloadButton("download_q5_ids", "Download IDs for Q5"),
        tags$hr(),
        
        h3("Q8. Total number of individuals who received services AND number of times each service was provided during the reporting period:"),
        
        h4("A. INFORMATION & REFERRAL"),
        DT::dataTableOutput(outputId = "table8A"),
        br(),
        downloadButton("download_q8A_ids", "Download IDs for Q8A"),
        tags$hr(),
        
        h4("B. PERSONAL ADVOCACY/ACCOMPANIMENT"),
        DT::dataTableOutput(outputId = "table8B"),
        br(),
        downloadButton("download_q8B_ids", "Download IDs for Q8B"),
        tags$hr(),
        
        h4("C. EMOTIONAL SUPPORT OR SAFETY SERVICES"),
        DT::dataTableOutput(outputId = "table8C"),
        br(),
        downloadButton("download_q8C_ids", "Download IDs for Q8C"),
        tags$hr(),
        
        h4("D. SHELTER/HOUSING SERVICES"),
        DT::dataTableOutput(outputId = "table8D"),
        br(),
        downloadButton("download_q8D_ids", "Download IDs for Q8D"),
        tags$hr(),
        
        h4("E. CRIMINAL/CIVIL JUSTICE SYSTEM ASSISTANCE"),
        DT::dataTableOutput(outputId = "table8E"),
        br(),
        downloadButton("download_q8E_ids", "Download IDs for Q8E"),
        tags$hr()
      )
    )
  )
)





# --- SERVER ----

server <- function(input, output) {
  
  # Allow for server file uploads to be <30MB (5MB default size)
  options(shiny.maxRequestSize=30*1024^2)
  
  # ---- Helper: service provider mapping ----
  mapping_service_provider <- c(
    "1"  = "Tony Rosen",
    "2"  = "Michael Stern",
    "3"  = "Mary Mulcare",
    "4"  = "Morgan Pearman",
    "5"  = "Michelle Sullivan",
    "6"  = "Kelly Brissenden",
    "7"  = "Alyssa Elman",
    "8"  = "Elaine Gottesman",
    "9"  = "Kevin Jeng",
    "10" = "Gwen Hobson",
    "11" = "Amy Shaw",
    "12" = "Daniela Paladino",
    "14" = "Chloe Pino",
    "15" = "Surriya Ahmad",
    "16" = "Jennine McAuley",
    "17" = "Emily Benton",
    "18" = "Jaclyn Itzkowitz",
    "19" = "Mollie Howard",
    "20" = "Sarah Perelman"
  )
  
  # ---- Data previews ----
  output$data1 <- DT::renderDataTable(
    {
      inFile <- input$file1
      if (is.null(inFile)) 
        return(NULL)
      readr::read_csv(inFile$datapath)
    },
    options = list(
      scrollX = TRUE, pageLength = 5, lengthMenu = c(5, 10, 20, 50),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  output$data2 <- DT::renderDataTable(
    {
      inFile <- input$file2
      if (is.null(inFile)) 
        return(NULL)
      readr::read_csv(inFile$datapath)
    },
    options = list(
      scrollX = TRUE, pageLength = 5, lengthMenu = c(5, 10, 20, 50),
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  
  
  # Initialize data and variables ---- 
  
  # get datasets ---
  userDF.PMT <- reactive({
    inFile1 <- input$file1
    if (is.null(inFile1)) 
      return(NULL)
    readr::read_csv(inFile1$datapath)
  })
  
  userDF.SSL <- reactive({
    inFile2 <- input$file2
    if (is.null(inFile2)) 
      return(NULL)
    readr::read_csv(inFile2$datapath)
  })
  
  # Data 1 - Note ----
  output$Data1Note <- renderText({
    
    inFile1 <- input$file1
    if (is.null(inFile1)) {
      text <- paste0(
        "<b>Report</b>: This dataset has not yet been uploaded."
      )
    } else{
      text <- paste0(
        "<b>Report</b>: This dataset contains ",
        "<b>", ncol(userDF.PMT()), "</b>",
        " variables for ",
        "<b>", nrow(userDF.PMT()), "</b>",
        " visits."
      )
    }
    
    return(text)
  })
  
  # Data 2 - Note ----
  output$Data2Note <- renderText({
    
    inFile2 <- input$file2
    if (is.null(inFile2)) {
      text <- paste0(
        "<b>Report</b>: This dataset has not yet been uploaded."
      )
    } else{
      text <- paste0(
        "<b>Report</b>: This dataset contains ",
        "<b>", ncol(userDF.SSL()), "</b>",
        " variables for ",
        "<b>", nrow(userDF.SSL()), "</b>",
        " visits."
      )
    }
    
    return(text)
  })
  
  
  # ---- Fiscal year helpers ----
  
  # Help text / validation message
  output$fy_help <- renderText({
    fy <- input$fiscal_year
    if (is.null(fy) || fy == "") {
      return("Please enter a fiscal year in the proper format (e.g., 2021-2022).")
    }
    if (!grepl("^20[0-9]{2}-20[0-9]{2}$", fy)) {
      return("<span style='color:#b31b1b;'><b>Format error:</b> use YYYY-YYYY format for an expected range (e.g., 2021-2022).</span>")
    }
    return("")
  })
  
  # get inputs
  getYear <- reactive({
    fy <- input$fiscal_year
    req(!is.null(fy), fy != "", grepl("^20[0-9]{2}-20[0-9]{2}$", fy))
    fy
  })
  
  getQuarter <- reactive({input$select2})
  
  # generate custom values
  report.Year <- reactive({substr(getYear(), nchar(getYear())-3, nchar(getYear()))})
  report.LastYear <- reactive({as.character(as.numeric(report.Year()) - 1)})
  report.LastLastYear <- reactive({as.character(as.numeric(report.LastYear()) - 1)})
  
  # Q1
  Q1.low  <- "10-01"
  Q1.high <- "12-31"
  
  # Q2
  Q2.low  <- "01-01"
  Q2.high <- "03-31"
  
  # Q3
  Q3.low  <- "04-01"
  Q3.high <- "06-30"
  
  # Q4
  Q4.low  <- "07-01"
  Q4.high <- "09-30"
  
  date.QRange.L <- reactive({
    req(getQuarter(), report.Year(), report.LastYear())
    if (getQuarter() == "Q1") {text <- paste0(report.LastYear(), "-", Q1.low)}
    if (getQuarter() == "Q2") {text <- paste0(report.Year(), "-", Q2.low)}
    if (getQuarter() == "Q3") {text <- paste0(report.Year(), "-", Q3.low)}
    if (getQuarter() == "Q4") {text <- paste0(report.Year(), "-", Q4.low)}
    if (getQuarter() == "Annual") {text <- paste0(report.LastYear(), "-", Q1.low)}
    
    return(text)
  })
  
  date.QRange.H <- reactive({
    req(getQuarter(), report.Year(), report.LastYear())
    if (getQuarter() == "Q1") {text <- paste0(report.LastYear(), "-", Q1.high)}
    if (getQuarter() == "Q2") {text <- paste0(report.Year(), "-", Q2.high)}
    if (getQuarter() == "Q3") {text <- paste0(report.Year(), "-", Q3.high)}
    if (getQuarter() == "Q4") {text <- paste0(report.Year(), "-", Q4.high)}
    if (getQuarter() == "Annual") {text <- paste0(report.Year(), "-", Q4.high)}
    
    return(text)
  })
  
  date.lastQRange.L <- reactive({
    req(getQuarter(), report.Year(), report.LastYear(), report.LastLastYear())
    if (getQuarter() == "Q1") {text <- paste0(report.LastYear(), "-", Q4.low)}
    if (getQuarter() == "Q2") {text <- paste0(report.LastYear(), "-", Q1.low)}
    if (getQuarter() == "Q3") {text <- paste0(report.Year(), "-", Q2.low)}
    if (getQuarter() == "Q4") {text <- paste0(report.Year(), "-", Q3.low)}
    if (getQuarter() == "Annual") {text <- paste0(report.LastLastYear(), "-", Q1.low)}
    
    return(text)
  })
  
  date.lastQRange.H <- reactive({
    req(getQuarter(), report.Year(), report.LastYear(), report.LastLastYear())
    if (getQuarter() == "Q1") {text <- paste0(report.LastYear(), "-", Q4.high)}
    if (getQuarter() == "Q2") {text <- paste0(report.LastYear(), "-", Q1.high)}
    if (getQuarter() == "Q3") {text <- paste0(report.Year(), "-", Q2.high)}
    if (getQuarter() == "Q4") {text <- paste0(report.Year(), "-", Q3.high)}
    if (getQuarter() == "Annual") {text <- paste0(report.LastYear(), "-", Q1.high)}
    
    return(text)
  })
  
  # global cutoff for all data (must be after this date)
  date.cutoff <- "2021-10-01"
  year.cutoff <- reactive({paste0(report.LastYear(), "-", "10-01")})
  
  
  
  output$reportNote <- renderText({
    req(getYear(), getQuarter(), date.QRange.L(), date.QRange.H())
    paste0(
      "<b>Note:</b> This report has been generated for ",
      "<b>", getQuarter(), "</b>",
      " of ",
      "<b>", getYear(), "</b>",
      " (",
      "<b>", date.QRange.L(), "</b>",
      " to ",
      "<b>", date.QRange.H(), "</b>",
      ")"
    )
  })
  
  
  # Table 1 - Cleaning ----
  
  # self-neglect cases
  self.neglect <- reactive({
    req(userDF.PMT())
    userDF.PMT() %>% 
      filter(redcap_event_name == "baseline_arm_1") %>% 
      rowwise() %>% 
      mutate(abuse_sum = sum(c_across(abuse_type___1:abuse_type___99), na.rm = TRUE)) %>% 
      ungroup() %>% 
      filter(abuse_type___6 == 1 & abuse_sum == abuse_type___6) %>% 
      select(record_id, mrn, record_id, abuse_type___1:abuse_type___99, abuse_sum)
  })
  
  # PMT baseline records
  currentQ.dates1 <- reactive({
    req(userDF.PMT(), date.QRange.L(), date.QRange.H())
    userDF.PMT() %>% 
      filter(redcap_event_name == "baseline_arm_1") %>% 
      mutate(
        visit.DateTime = as.POSIXct(
          strptime(veptactivation_time, format = "%Y-%m-%d %H:%M")
        ),
        # convert visit and discharge time to proper format
        visit.date = as.Date(as.character(visit.DateTime)),
        discharge.date = as.Date(as.character(date_discharge))
      ) %>% 
      filter(visit.date >= date.QRange.L() & visit.date <= date.QRange.H()) %>% 
      select(record_id, mrn, visit.date) %>% 
      mutate(source = "PMT Baseline")
  })
  
  # PMT Report follow-ups
  currentQ.dates2 <- reactive({
    req(userDF.PMT(), date.QRange.L(), date.QRange.H())
    userDF.PMT() %>% 
      filter(date_service >= date.QRange.L() & date_service <= date.QRange.H()) %>% 
      select(record_id, date_service) %>% 
      left_join(userDF.PMT() %>% filter(!is.na(mrn)) %>% select(record_id, mrn), by = "record_id") %>% 
      relocate(mrn, .after = record_id) %>% 
      mutate(source = "PMT Follow-up")
  })
  
  # SSL Report follow-ups
  currentQ.dates3 <- reactive({
    req(userDF.SSL(), userDF.PMT(), date.QRange.L(), date.QRange.H())
    userDF.SSL() %>% 
      filter(date_service >= date.QRange.L() & date_service <= date.QRange.H()) %>% 
      select(record_id, date_service) %>% 
      left_join(userDF.PMT() %>% filter(!is.na(mrn)) %>% select(record_id, mrn), by = "record_id") %>% 
      relocate(mrn, .after = record_id) %>% 
      mutate(source = "SSL Follow-up")
  })
  
  # Combine all data sources
  tbl.q1.temp <- reactive({
    req(currentQ.dates1(), currentQ.dates2(), currentQ.dates3(), self.neglect())
    rbind(
      currentQ.dates1() %>% select(record_id, mrn, visit.date, source) %>% rename(date_service = visit.date),
      currentQ.dates2() %>% select(record_id, mrn, date_service, source),
      currentQ.dates3() %>% select(record_id, mrn, date_service, source)
    ) %>% 
      filter(!(record_id %in% self.neglect()$record_id)) %>% 
      arrange(mrn, date_service, source) 
  })
  
  tbl.q1 <- reactive({
    req(tbl.q1.temp())
    tbl.q1.temp() %>% 
      group_by(mrn) %>% 
      summarise(
        record_id = paste(record_id, collapse = ", "),
        visits = n(),
        date_service = paste(date_service, collapse = ", "),
        source = paste(source, collapse = ", ")
      ) %>% 
      mutate(
        source = case_when(
          str_detect(source, "PMT Follow-up|SSL Follow-up", negate = TRUE) ~ "PMT BL",
          str_detect(source, "PMT Baseline|PMT Follow-up", negate = TRUE) ~ "SSL FU",
          str_detect(source, "PMT Baseline|SSL Follow-up", negate = TRUE) ~ "PMT FU",
          str_detect(source, "SSL Follow-up", negate = TRUE) ~ "PMT BL/FU",
          str_detect(source, "PMT Follow-up", negate = TRUE) ~ "PMT BL, SSL FU",
          str_detect(source, "PMT Baseline", negate = TRUE) ~ "PMT/SSL FU",
          TRUE ~ "PMT BL, PMT/SSL FU"
        ),
        date_service = case_when(
          nchar(date_service) >10 ~ paste(
            substring(date_service, 1, 10), "to", 
            substring(date_service, nchar(date_service)-4, nchar(date_service))
          ),
          TRUE ~ date_service
        ),
        record_id = case_when(
          nchar(record_id) >3 & substring(record_id, 1, 1) %in% c("7", "8", "9") ~ paste(
            substring(record_id, 1, 3),
            str_replace_all(record_id, substring(record_id, 1, 3), "")
          ),
          nchar(record_id) >4 & substring(record_id, 1, 1) == "1" ~ paste(
            substring(record_id, 1, 4),
            str_replace_all(record_id, substring(record_id, 1, 4), "")
          ),
          TRUE ~ record_id
        ),
        record_id = case_when(
          # one record id <1000
          str_count(record_id, "[0-9]") <4 & substring(record_id, 1, 1) %in% c("7", "8", "9") ~ substring(record_id, 1, 3),
          # one record id 1000+
          str_count(record_id, "[0-9]") <5 & substring(record_id, 1, 1) == "1" ~ substring(record_id, 1, 4),
          # 2+ record id <1000
          substring(record_id, 1, 1) %in% c("7", "8", "9") ~ paste(substring(record_id, 1, 3), "/", str_extract(substring(record_id, 4), "[0-9]+")),
          # 2+ record id 1000+
          substring(record_id, 1, 1) == "1" ~ paste(substring(record_id, 1, 4), "/", str_extract(substring(record_id, 5), "[0-9]+"))
        )
      )
  })
  
  # Table 1 - Print ----
  output$Table1Note <- renderText({
    req(tbl.q1(), getQuarter(), getYear())
    paste0(
      "There were ",
      "<b>", nrow(tbl.q1()), "</b>",
      " individuals who received services in ",
      "<b>", getQuarter(), "</b>",
      ", ",
      "<b>", getYear(), "</b>",
      "."
    )
  })
  
  output$table1 <- DT::renderDataTable(
    {
      req(tbl.q1())
      tbl.q1() %>% 
        dplyr::rename(
          `Record ID` = record_id,
          `MRN` = mrn,
          `Visits` = visits,
          `Service Dates` = date_service,
          `Source` = source
        )
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  
  # Table 3A - Cleaning ----
  
  # PMT baseline records
  repeats.dates1 <- reactive({
    req(userDF.PMT(), year.cutoff(), date.lastQRange.H())
    userDF.PMT() %>% 
      filter(redcap_event_name == "baseline_arm_1") %>% 
      mutate(
        visit.DateTime = as.POSIXct(
          strptime(veptactivation_time, format = "%Y-%m-%d %H:%M")
        ),
        # convert visit and discharge time to proper format
        visit.date = as.Date(as.character(visit.DateTime)),
        discharge.date = as.Date(as.character(date_discharge))
      ) %>% 
      filter(visit.date >= year.cutoff() & visit.date <= date.lastQRange.H()) %>% 
      select(record_id, mrn, visit.date) %>% 
      mutate(source = "PMT Baseline")
  })
  
  # PMT Report follow-ups
  repeats.dates2 <- reactive({
    req(userDF.PMT(), year.cutoff(), date.lastQRange.H())
    userDF.PMT() %>% 
      filter(date_service >= year.cutoff() & date_service <= date.lastQRange.H()) %>% 
      select(record_id, date_service) %>% 
      left_join(userDF.PMT() %>% filter(!is.na(mrn)) %>% select(record_id, mrn), by = "record_id") %>% 
      relocate(mrn, .after = record_id) %>% 
      mutate(source = "PMT Follow-up")
  })
  
  # SSL Report follow-ups
  repeats.dates3 <- reactive({
    req(userDF.SSL(), userDF.PMT(), year.cutoff(), date.lastQRange.H())
    userDF.SSL() %>% 
      filter(date_service >= year.cutoff() & date_service <= date.lastQRange.H()) %>% 
      select(record_id, date_service) %>% 
      left_join(userDF.PMT() %>% filter(!is.na(mrn)) %>% select(record_id, mrn), by = "record_id") %>% 
      relocate(mrn, .after = record_id) %>% 
      mutate(source = "SSL Follow-up")
  })
  
  # Combine all data sources
  tbl.q3A.compare <- reactive({
    req(repeats.dates1(), repeats.dates2(), repeats.dates3(), self.neglect())
    rbind(
      repeats.dates1() %>% select(record_id, mrn, visit.date, source) %>% rename(date_service = visit.date),
      repeats.dates2() %>% select(record_id, mrn, date_service, source),
      repeats.dates3() %>% select(record_id, mrn, date_service, source)
    ) %>% 
      filter(!(record_id %in% self.neglect()$record_id)) %>% 
      arrange(mrn, date_service, source) 
  })
  
  tbl.q3A <- reactive({
    req(tbl.q3A.compare(), tbl.q1())
    tbl.q3A.compare() %>% 
      group_by(mrn) %>% 
      summarise(
        record_id2 = paste(record_id, collapse = ", "),
        date_service2 = paste(date_service, collapse = ", ")
      ) %>% 
      mutate(
        date_service2 = case_when(
          nchar(date_service2) >10 ~ paste(
            substring(date_service2, 1, 10), "to", 
            substring(date_service2, nchar(date_service2)-4, nchar(date_service2))
          ),
          TRUE ~ date_service2
        ),
        record_id2 = case_when(
          nchar(record_id2) >3 ~ paste(
            substring(record_id2, 1, 3),
            str_replace_all(record_id2, substring(record_id2, 1, 3), "")
          ),
          TRUE ~ record_id2
        ),
        record_id2 = case_when(
          str_count(record_id2, "[0-9]") <4 ~ substring(record_id2, 1, 3),
          TRUE ~ paste(substring(record_id2, 1, 3), "/", str_extract(substring(record_id2, 4), "[0-9]+"))
        )
      ) %>% 
      left_join(tbl.q1(), ., by = "mrn") %>% 
      filter(!is.na(record_id2)) %>% 
      mutate(
        record_id2 = case_when(
          record_id == record_id2 ~ "--",
          substring(record_id, 1, 3) == substring(record_id, nchar(record_id)-2, nchar(record_id)) &
            substring(record_id2, 1, 3) == substring(record_id2, nchar(record_id2)-2, nchar(record_id2))
          ~ record_id2,
          nchar(record_id2) == 3 & str_detect(record_id, record_id2) ~ "--",
          nchar(record_id2) >3 & str_detect(record_id2, record_id) ~ str_extract(str_replace(record_id2, record_id, "."), "[0-9]+")
        )
      ) %>% 
      select(-c(visits, source))
  })
  
  # Table 3A - Print ----
  output$Table3ANote <- renderText({
    req(tbl.q3A(), getQuarter(), getYear())
    paste0(
      "There were ",
      "<b>", nrow(tbl.q3A()), "</b>",
      " individuals who received <b>repeat</b> services in ",
      "<b>", getQuarter(), "</b>",
      ", ",
      "<b>", getYear(), "</b>",
      "."
    )
  })
  
  output$table3A <- DT::renderDataTable(
    {
      req(tbl.q3A())
      tbl.q3A() %>% 
        dplyr::rename(
          `Record ID` = record_id,
          `MRN` = mrn,
          `Service Dates` = date_service,
          `Other IDs` = record_id2,
          `Older Services` = date_service2
        )
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  
  # Table 3B - Cleaning ----
  
  # Subset repeat visits
  tbl.q3B <- reactive({
    req(tbl.q1(), tbl.q3A())
    tbl.q1() %>% filter(!(mrn %in% tbl.q3A()$mrn))
  })
  
  # Table 3B - Print ----
  output$Table3BNote <- renderText({
    req(tbl.q3B(), getQuarter(), getYear())
    paste0(
      "There were ",
      "<b>", nrow(tbl.q3B()), "</b>",
      " individuals who received <b>new</b> services in ",
      "<b>", getQuarter(), "</b>",
      ", ",
      "<b>", getYear(), "</b>",
      "."
    )
  })
  
  output$table3B <- DT::renderDataTable(
    {
      req(tbl.q3B())
      tbl.q3B() %>% 
        dplyr::rename(
          `Record ID` = record_id,
          `MRN` = mrn,
          `Visits` = visits,
          `Service Dates` = date_service,
          `Source` = source
        )
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  
  
  # Later Tables - Cleaning ----
  
  collect.id <- reactive({
    req(tbl.q1(), tbl.q3B())
    tbl.q1() %>% 
      separate(record_id, c("record_id1", "record_id2")) %>% 
      mutate(new.id = case_when(mrn %in% tbl.q3B()$mrn ~ 1, TRUE ~ 0))
  })
  
  # Filter for total IDs
  PMT.total <- reactive({
    req(userDF.PMT(), collect.id())
    userDF.PMT() %>% 
      fill(mrn) %>% 
      left_join(collect.id(), by = "mrn") %>% 
      filter(record_id == record_id1 | record_id == record_id2)
  }) 
  
  SSL.total <- reactive({
    req(userDF.SSL(), collect.id())
    userDF.SSL() %>% 
      fill(mrn) %>% 
      left_join(collect.id(), by = "mrn") %>% 
      filter(record_id == record_id1 | record_id == record_id2)
  }) 
  
  # Filter for new IDs
  PMT.new <- reactive({PMT.total() %>% filter(new.id == 1)})
  SSL.new <- reactive({SSL.total() %>% filter(new.id == 1)})
  
  
  # Create new total datasets
  DF.total <- reactive({
    req(PMT.total(), SSL.total(), self.neglect())
    bind_rows(PMT.total(), SSL.total()) %>% 
      mutate(
        visit.DateTime = as.POSIXct(
          strptime(veptactivation_time, format = "%Y-%m-%d %H:%M")
        ),
        # convert visit and discharge time to proper format
        visit.date = as.Date(as.character(visit.DateTime)),
        discharge.date = as.Date(as.character(date_discharge))
      ) %>% 
      filter(redcap_event_name == "baseline_arm_1") %>% 
      distinct(mrn, .keep_all = TRUE) %>% 
      filter(!(record_id %in% self.neglect()$record_id))
  })
  
  DF.new <- reactive({
    req(PMT.new(), SSL.new(), self.neglect())
    bind_rows(PMT.new(), SSL.new()) %>% 
      mutate(
        visit.DateTime = as.POSIXct(
          strptime(veptactivation_time, format = "%Y-%m-%d %H:%M")
        ),
        # convert visit and discharge time to proper format
        visit.date = as.Date(as.character(visit.DateTime)),
        discharge.date = as.Date(as.character(date_discharge))
      ) %>% 
      filter(redcap_event_name == "baseline_arm_1") %>% 
      distinct(mrn, .keep_all = TRUE) %>% 
      filter(!(record_id %in% self.neglect()$record_id))
  })
  
  
  
  # Table 4 - Cleaning ----
  
  tbl.q4.counts <- reactive({
    req(DF.new())
    
    DF.new() %>% 
      mutate(
        # race
        race1_level = case_when(race_pt___4 == 1 ~ 1, TRUE ~ 0),  # American Indian/ Alaska Native
        race2_level = case_when(race_pt___3 == 1 ~ 1, TRUE ~ 0),  # Asian
        race3_level = case_when(race_pt___2 == 1 ~ 1, TRUE ~ 0),  # Black/ African American
        race4_level = case_when(ethnicity_pt == 1 & race3_level == 0 ~ 1, TRUE ~ 0), # Hispanic or Latino
        race5_level = case_when(race_pt___5 == 1 ~ 1, TRUE ~ 0),  # Native Hawaiian & Other Pacific Islander
        race6_level = case_when(race_pt___1 == 1 & ethnicity_pt != 1 ~ 1, TRUE ~ 0),  # White Non-Latino/ Caucasian
        race7_level = case_when(
          race_pt___77 == 1 |
            (ethnicity_pt != 99 &
               (race1_level + race2_level + race3_level + race4_level + race5_level + race6_level) == 0) ~ 1, TRUE ~ 0
        ), # Some Other Race
        race8_level = case_when(
          race_pt___1 + race_pt___2 + race_pt___3 + race_pt___4 + race_pt___5 + race_pt___77 >1 ~ 1,
          TRUE ~ 0
        ), # Multiple Races
        race9_level = case_when(
          race_pt___99 == 1 & 
            ((race1_level + race2_level + race3_level + race4_level + race5_level + race6_level + race7_level) == 0) ~ 1, 
          TRUE ~ 0
        ), # Not Reported
        
        # sex
        sex1_level = case_when(sex_pt == 1 ~ 1, TRUE ~ 0),  # Male
        sex2_level = case_when(sex_pt == 2 ~ 1, TRUE ~ 0),  # Female
        sex3_level = case_when(sex_pt == 3 ~ 1, TRUE ~ 0),  # Other (Transgender)
        sex4_level = case_when(sex_pt == 99 ~ 1, TRUE ~ 0), # Not Reported
        
        # calculate age
        age = as.numeric((visit.date - as.Date(dob_pt, "%Y-%m-%d")) / 365.25),
        
        age1_level = case_when(age <13 ~ 1, TRUE ~ 0),  # 0-12
        age2_level = case_when(age <18 ~ 1, TRUE ~ 0),  # 13-17
        age3_level = case_when(age <25 ~ 1, TRUE ~ 0),  # 18-24
        age4_level = case_when(age <60 ~ 1, TRUE ~ 0),  # 25-59
        age5_level = case_when(age >=60 ~ 1, TRUE ~ 0), # 60 & Older
        age6_level = case_when(is.na(age) ~ 1, TRUE ~ 0), # Not Reported 
      ) %>% 
      
      # filter out the IDs just from the new individuals
      select(record_id, mrn, race1_level:age6_level) %>%
      distinct(mrn, .keep_all = TRUE)
    
  })
  
  tbl.q4 <- reactive({
    req(tbl.q4.counts())
    
    res <- cbind(
      # question categories
      c(
        rep("A. RACE/ETHNICITY", 10),
        "Race/Ethnicity Total",
        rep("B. GENDER IDENTITY", 5),
        "Gender Total",
        rep("C. AGE", 7),
        "Age Total"
      ),
      # question labels
      c(
        "American Indian/Alaska Native",
        "Asian",
        "Black/African American",
        "Hispanic/Latino",
        "Native Hawaiian & Other Pacific Islander",
        "White Non-Latino/Caucasian",
        "Some Other Race",
        "Multiple Races",
        "Not Reported",
        "Not Tracked",
        "",
        
        "Male",
        "Female",
        "Other",
        "Not Reported",
        "Not Tracked",
        "",
        
        "0-12",
        "13-17",
        "18-24",
        "25-59",
        "60 & Older",
        "Not Reported",
        "Not Tracked",
        ""
      ),
      # counts
      c(
        sum(tbl.q4.counts()$race1_level),
        sum(tbl.q4.counts()$race2_level),
        sum(tbl.q4.counts()$race3_level),
        sum(tbl.q4.counts()$race4_level),
        sum(tbl.q4.counts()$race5_level),
        sum(tbl.q4.counts()$race6_level),
        sum(tbl.q4.counts()$race7_level),
        sum(tbl.q4.counts()$race8_level),
        sum(tbl.q4.counts()$race9_level),
        0,
        "Race Total",
        
        sum(tbl.q4.counts()$sex1_level),
        sum(tbl.q4.counts()$sex2_level),
        sum(tbl.q4.counts()$sex3_level),
        sum(tbl.q4.counts()$sex4_level),
        0,
        "Sex Total",
        
        sum(tbl.q4.counts()$age1_level),
        sum(tbl.q4.counts()$age2_level),
        sum(tbl.q4.counts()$age3_level),
        sum(tbl.q4.counts()$age4_level),
        sum(tbl.q4.counts()$age5_level),
        sum(tbl.q4.counts()$age6_level),
        0,
        "Age Total"
      )
    ) %>% 
      as.data.frame()
    
    res$V3[11] <- sum(as.numeric(res$V3[2:10]))
    res$V3[17] <- sum(as.numeric(res$V3[12:16]))
    res$V3[25] <- sum(as.numeric(res$V3[18:24]))
    
    colnames(res) <- c("Category", "Population", "Number of NEW Individuals")
    rownames(res) <- NULL
    
    return(res)
    
  })
  
  # Table 4 - Print ----
  
  output$table4 <- DT::renderDataTable(
    {
      req(tbl.q4())
      tbl.q4()
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  # Download IDs for Q4 ----
  output$download_q4_ids <- downloadHandler(
    filename = function() {
      paste0("pmt_q4_ids_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(
        tbl.q4.counts() %>% arrange(record_id),
        file
      )
    }
  )
  
  
  
  # Table 5 - Cleaning ----
  
  tbl.q5.counts.temp <- reactive({
    
    DF.total() %>% 
      distinct(record_id, .keep_all = TRUE) %>% 
      mutate(
        vict1 = case_when(abuse_type___1 == 1 ~ 1, TRUE ~ 0),
        vict2 = case_when(abuse_type___4 == 1 ~ 1, TRUE ~ 0),
        vict3 = 0,
        vict4 = case_when(burgrobars_pt___3 == 1 ~ 1, TRUE ~ 0),
        vict5 = case_when(abuse_type___2 == 1 ~ 1, TRUE ~ 0),
        vict6 = case_when(burgrobars_pt___1 == 1 ~ 1, TRUE ~ 0),
        vict7 = 0,
        vict8 = 0,
        vict9 = 0,
        vict10 = case_when(
          living_sit_ab1 == 2 ~ 1, 
          living_sit_ab1 == 4 ~ 1,
          relation_ab1 %in% c(1:6, 10:14, 16:17) ~ 1,
          TRUE ~ 0
        ),
        vict11 = 0,
        vict12 = 1,
        vict13 = 0,
        vict14 = 0,
        vict15 = 0,
        vict16 = case_when(abuse_type___3 == 1 ~ 1, TRUE ~ 0),
        vict17 = 0,
        vict18 = 0,
        vict19 = 0,
        vict20 = 0,
        vict21 = case_when(burgrobars_pt___2 == 1 ~ 1, TRUE ~ 0),
        vict22 = 0,
        vict23 = 0,
        vict24 = 0,
        vict25 = 0,
        vict26 = case_when(abuse_type___77 == 1 ~ 1, TRUE ~ 0),
        # vict.total goes here
        mult.v = case_when(rowSums(across(c(vict1:vict26))) >1 ~ 1, TRUE ~ 0),
        
        class1 = case_when(deaf_pt == 1 ~ 1, TRUE ~ 0),
        class2 = case_when(
          living_sit_pt != 3 ~ 0,
          community_pt == 5 ~ 1, 
          TRUE ~ 0
        ),
        class3 = case_when(immig_pt == 1 ~ 1, TRUE ~ 0),
        class4 = case_when(sexuality_pt %in% c(2, 3) ~ 1, TRUE ~ 0),
        class5 = case_when(veteran_pt == 1 ~ 1, TRUE ~ 0),
        class6 = case_when(
          physdis_pt == 1 ~ 1,
          blind_pt == 1 ~ 1,
          dementia_pt == 1 ~ 1,
          devdel_pt == 1 ~ 1,
          TRUE ~ 0
        ),
        class6_pd = case_when(physdis_pt == 1 ~ 1, TRUE ~ 0),
        class6_bl = case_when(blind_pt == 1 ~ 1, TRUE ~ 0),
        class6_dem = case_when(dementia_pt == 1 ~ 1, TRUE ~ 0),
        class6_dev = case_when(devdel_pt == 1 ~ 1, TRUE ~ 0),
        class7 = case_when(engl_eval == 0 ~ 1, TRUE ~ 0),
        class8 = 0
      ) %>% 
      
      select(record_id, mrn, vict1:class8) %>%
      distinct(mrn, .keep_all = TRUE)
  })
  
  tbl.q5.counts <- reactive({
    tbl.q5.counts.temp() %>% 
      select(-c(class6_pd, class6_bl, class6_dem, class6_dev))
  })
  
  tbl.q5 <- reactive({
    req(tbl.q5.counts())
    
    res <- cbind(
      # question categories
      c(
        rep("A. Number Who Recieved Services", 26),
        "Victimization Total",
        "If other, please explain",
        "B. Multiple Victimizations",
        rep("C. Special Classifications", 8),
        "Special Classifications Total",
        "If other, please explain"
      ),
      # question labels
      c(
        "Adult Physical Assault Aggrivated & Simple Assault)",
        "Adult Sexual Assault",
        "Adults Sexually Abuse/Assaulted as Children",
        "Arson",
        "Bullying (Verbal, Cyber, or Physical)",
        "Burglary",
        "Child Physical Abuse or Neglect",
        "Child Pornography",
        "Child Sexual Abuse/Assault",
        "Domestic and/or Family Violence",
        "DUI/DWI Incidents",
        "Elder Abuse or Neglect",
        "Hate Crime",
        "Human Trafficking: Labor",
        "Human Trafficking: Sex",
        "Identity Theft/Fraud/Financial Crime",
        "Kidnapping (non-custodial)",
        "Kidnapping (custodial)",
        "Mass Violence",
        "Other Vehicular Victimization (e.g., Hit & Run)",
        "Robbery",
        "Stalking/Harassment",
        "Survivors of Homicide Victims",
        "Teen Dating Victimization",
        "Terrorism (Domestic/International)",
        "Other",
        "",
        "",
        "",
        
        "Deaf/Hard of Hearing",
        "Homeless",
        "Immigrants/Refugees/Asylum Seekers",
        "LGBTQ",
        "Veterans",
        "Victims with Disabilities",
        "Victims with Limited English Proficiency",
        "Other",
        "",
        ""
      ),
      # counts
      c(
        sum(tbl.q5.counts()$vict1),
        sum(tbl.q5.counts()$vict2),
        sum(tbl.q5.counts()$vict3),
        sum(tbl.q5.counts()$vict4),
        sum(tbl.q5.counts()$vict5),
        sum(tbl.q5.counts()$vict6),
        sum(tbl.q5.counts()$vict7),
        sum(tbl.q5.counts()$vict8),
        sum(tbl.q5.counts()$vict9),
        sum(tbl.q5.counts()$vict10),
        
        sum(tbl.q5.counts()$vict11),
        sum(tbl.q5.counts()$vict12),
        sum(tbl.q5.counts()$vict13),
        sum(tbl.q5.counts()$vict14),
        sum(tbl.q5.counts()$vict15),
        sum(tbl.q5.counts()$vict16),
        sum(tbl.q5.counts()$vict17),
        sum(tbl.q5.counts()$vict18),
        sum(tbl.q5.counts()$vict19),
        sum(tbl.q5.counts()$vict20),
        
        sum(tbl.q5.counts()$vict21),
        sum(tbl.q5.counts()$vict22),
        sum(tbl.q5.counts()$vict23),
        sum(tbl.q5.counts()$vict24),
        sum(tbl.q5.counts()$vict25),
        sum(tbl.q5.counts()$vict26),
        "Victimization Total",
        "",
        
        sum(tbl.q5.counts()$mult.v),
        
        sum(tbl.q5.counts()$class1),
        sum(tbl.q5.counts()$class2),
        sum(tbl.q5.counts()$class3),
        sum(tbl.q5.counts()$class4),
        sum(tbl.q5.counts()$class5),
        sum(tbl.q5.counts()$class6),
        sum(tbl.q5.counts()$class7),
        sum(tbl.q5.counts()$class8),
        "Classification Total",
        ""
      )
    ) %>% 
      as.data.frame()
    
    res$V3[27] <- sum(as.numeric(res$V3[1:26]))
    res$V3[38] <- sum(as.numeric(res$V3[30:37]))
    
    colnames(res) <- c("Category", "Victimization Type", "Number of Individuals")
    rownames(res) <- NULL
    
    return(res)
    
  })
  
  # Table 5 - Print ----
  
  output$table5 <- DT::renderDataTable(
    {
      req(tbl.q5())
      tbl.q5()
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  # Table 5 - Print (other victimizations) ----
  tbl.q5.counts.other <- reactive({
    
    DF.total() %>% 
      distinct(record_id, .keep_all = TRUE) %>% 
      filter(abuse_type___77 == 1) %>% 
      select(record_id, abuse_type___77, abuse_type_oth)
  })
  
  output$table5.other <- DT::renderDataTable(
    {
      req(tbl.q5.counts.other())
      tbl.q5.counts.other() |> 
        mutate(abuse_type___77 = case_when(abuse_type___77 == 1 ~ "Yes")) |> 
        dplyr::rename(
          `Record ID` = record_id,
          `Abuse Type: Other?` = abuse_type___77,
          `Please Specify` = abuse_type_oth
        )
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  # Download IDs for Q5 ----
  output$download_q5_ids <- downloadHandler(
    filename = function() {
      paste0("pmt_q5_ids_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(
        tbl.q5.counts() %>% arrange(record_id),
        file
      )
    }
  )
  
  
  # Table 8A - Cleaning ----
  ## create template data for all of Q8
  tbl.q8.temp <- reactive({
    req(userDF.PMT(), userDF.SSL(), self.neglect(), date.QRange.L(), date.QRange.H())
    
    rbind(
      userDF.PMT() %>% 
        select(
          record_id, date_service, 
          # Q8A
          serv_count_crim_just, serv_count_victim_rights, serv_count_oth_vict, serv_count_all_oth,
          # Q8B
          serv_count_advoc, serv_count_law_enforc, serv_count_indiv_advoc, serv_count_med_for_exam, serv_count_interp_serv,
          # Q8C
          serv_count_crisis_interv, serv_count_crisis_resp, serv_count_ind_couns, serv_count_other_serv,
          # Q8D
          serv_count_emerg_shelt, serv_count_reloc_assist,
          # Q8E
          serv_count_civ_leg_assist, serv_count_oth_emerg_just, serv_count_oth_legal,
          # provider
          service_provider
        ) %>% 
        mutate(source = "PMT Report"),
      
      userDF.SSL() %>% select(
        record_id, date_service, 
        # Q8A
        serv_count_crim_just, serv_count_victim_rights, serv_count_oth_vict, serv_count_all_oth,
        # Q8B
        serv_count_advoc, serv_count_law_enforc, serv_count_indiv_advoc, serv_count_med_for_exam, serv_count_interp_serv,
        # Q8C
        serv_count_crisis_interv, serv_count_crisis_resp, serv_count_ind_couns, serv_count_other_serv,
        # Q8D
        serv_count_emerg_shelt, serv_count_reloc_assist,
        # Q8E
        serv_count_civ_leg_assist, serv_count_oth_emerg_just, serv_count_oth_legal,
        # provider
        service_provider
      ) %>% 
        mutate(source = "SSL Report")
    ) %>% 
      
      filter(!(record_id %in% self.neglect()$record_id)) %>% 
      filter(date_service >= date.QRange.L() & date_service <= date.QRange.H()) %>% 
      mutate_if(is.logical, as.numeric) %>% 
      mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
      left_join(userDF.PMT() %>% filter(!is.na(mrn)) %>% select(record_id, mrn), by = "record_id")
    
  })
  
  tbl.q8A.counts <- reactive({
    req(tbl.q8.temp())
    tbl.q8.temp() %>% 
      mutate(
        q.a1 = serv_count_crim_just,
        q.a2 = serv_count_victim_rights,
        q.a3 = serv_count_oth_vict,
        q.a4 = serv_count_all_oth,
        
        q.a.sum = q.a1 + q.a2 + q.a3 + q.a4,
        q.a = case_when(q.a.sum >0 ~ 1, TRUE ~ 0)
      )
  })
  
  tbl.q8B.counts <- reactive({
    req(tbl.q8.temp())
    tbl.q8.temp() %>% 
      mutate(
        q.b1 = serv_count_advoc,
        q.b3 = serv_count_law_enforc,
        q.b4 = serv_count_indiv_advoc,
        q.b5 = serv_count_med_for_exam,
        q.b10 = serv_count_interp_serv,
        
        q.b.sum = q.b1 + q.b3 + q.b4 + q.b5 + q.b10,
        q.b = case_when(q.b.sum >0 ~ 1, TRUE ~ 0)
      )
  })
  
  tbl.q8C.counts <- reactive({
    req(tbl.q8.temp())
    tbl.q8.temp() %>% 
      mutate(
        q.c1 = serv_count_crisis_interv,
        q.c3 = serv_count_crisis_resp,
        q.c4 = serv_count_ind_couns,
        q.c6 = serv_count_other_serv,
        
        q.c.sum = q.c1 + q.c3 + q.c4 + q.c6,
        q.c = case_when(q.c.sum >0 ~ 1, TRUE ~ 0)
      )
  })
  
  tbl.q8D.counts <- reactive({
    req(tbl.q8.temp())
    tbl.q8.temp() %>% 
      mutate(
        q.d1 = serv_count_emerg_shelt,
        q.d3 = serv_count_reloc_assist,
        
        q.d.sum = q.d1 + q.d3,
        q.d = case_when(q.d.sum >0 ~ 1, TRUE ~ 0)
      )
  })
  
  tbl.q8E.counts <- reactive({
    req(tbl.q8.temp())
    tbl.q8.temp() %>% 
      mutate(
        q.e4 = serv_count_civ_leg_assist,
        q.e6 = serv_count_oth_emerg_just,
        q.e11 = serv_count_oth_legal,
        
        q.e.sum = q.e4 + q.e6 + q.e11,
        q.e = case_when(q.e.sum >0 ~ 1, TRUE ~ 0)
      )
  })
  
  # ---- Q8 projection helpers ----
  
  proj_8A_vec <- reactive({
    c(
      input$proj_8A,
      input$proj_8A1,
      input$proj_8A2,
      input$proj_8A3,
      input$proj_8A4
    )
  })
  
  proj_8B_vec <- reactive({
    c(
      input$proj_8B,
      input$proj_8B1,
      input$proj_8B2,
      input$proj_8B3,
      input$proj_8B4,
      input$proj_8B5,
      input$proj_8B6,
      input$proj_8B7,
      input$proj_8B8,
      input$proj_8B9,
      input$proj_8B10
    )
  })
  
  proj_8C_vec <- reactive({
    c(
      input$proj_8C,
      input$proj_8C1,
      input$proj_8C2,
      input$proj_8C3,
      input$proj_8C4,
      input$proj_8C5,
      input$proj_8C6
    )
  })
  
  proj_8D_vec <- reactive({
    c(
      input$proj_8D,
      input$proj_8D1,
      input$proj_8D2,
      input$proj_8D3
    )
  })
  
  proj_8E_vec <- reactive({
    c(
      input$proj_8E,
      input$proj_8E1,
      input$proj_8E2,
      input$proj_8E3,
      input$proj_8E4,
      input$proj_8E5,
      input$proj_8E6,
      input$proj_8E7,
      input$proj_8E8,
      input$proj_8E9,
      input$proj_8E10,
      input$proj_8E11
    )
  })
  
  
  # Table 8A ----
  tbl.q8A <- reactive({
    req(tbl.q8A.counts(), proj_8A_vec())
    
    res <- cbind(
      # question labels
      c(
        "8A. Individuals who received services",
        "8A1. Information about the criminal justice system",
        "8A2. Information about victims rights, how to obtain notifications, etc.",
        "8A3. Referral to other victim service programs",
        "8A4. Referral to other services, supports, and resources"
      ),
      # projections (editable)
      proj_8A_vec(),
      # observed counts
      c(
        nrow(
          tbl.q8A.counts() %>% 
            group_by(mrn) %>% 
            summarise(total = sum(q.a.sum)) %>% 
            filter(total >0)
        ),
        sum(tbl.q8A.counts()$q.a1),
        sum(tbl.q8A.counts()$q.a2),
        sum(tbl.q8A.counts()$q.a3),
        sum(tbl.q8A.counts()$q.a4)
      )
    ) %>% 
      as.data.frame() %>% 
      mutate(V4 = round((as.numeric(V3)/as.numeric(V2))*100, 1)) %>% 
      mutate(V4 = paste0(V4, "%")) %>% 
      mutate(V4 = case_when(str_detect(V4, "NaN|Inf") ~ "--", TRUE ~ V4))
    
    colnames(res) <- c("Question", "Projection", "Observed", "Percentage")
    rownames(res) <- NULL
    
    return(res)
    
  })
  
  output$table8A <- DT::renderDataTable(
    {
      req(tbl.q8A())
      tbl.q8A()
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END - 
  
  # Download IDs for Q8A ----
  output$download_q8A_ids <- downloadHandler(
    filename = function() {
      paste0("pmt_q8A_ids_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(
        tbl.q8A.counts() %>% arrange(record_id),
        file
      )
    }
  )
  
  
  # Table 8B ----
  tbl.q8B <- reactive({
    req(tbl.q8B.counts(), proj_8B_vec())
    
    res <- cbind(
      # question labels
      c(
        "8B. Individuals who received services",
        "8B1. Victim advocacy/accompaniment to emergency medical care",
        "8B2. Victim advocacy/accompaniment to medical forensic exam",
        "8B3. Law enforcement interview advocacy/accompaniment",
        "8B4. Individual advocacy",
        "8B5. Performance of medical forensic exam or interview, or medical evidence collection",
        "8B6. Immigration assistance",
        "8B7. Intervention with employer, creditor, landlord, or academic institution",
        "8B8. Child or dependent care assistence (includes coordination of services)",
        "8B9. Transportation assistance (includes coordination of services)",
        "8B10. Interpreter services"
      ),
      # projections (editable)
      proj_8B_vec(),
      # observed counts
      c(
        nrow(
          tbl.q8B.counts() %>% 
            group_by(mrn) %>% 
            summarise(total = sum(q.b.sum)) %>% 
            filter(total >0)
        ),
        sum(tbl.q8B.counts()$q.b1),
        0,
        sum(tbl.q8B.counts()$q.b3),
        sum(tbl.q8B.counts()$q.b4),
        sum(tbl.q8B.counts()$q.b5),
        0, 0, 0, 0,
        sum(tbl.q8B.counts()$q.b10)
      )
    ) %>%
      as.data.frame() %>%
      mutate(V4 = round((as.numeric(V3)/as.numeric(V2))*100, 1)) %>%
      mutate(V4 = paste0(V4, "%")) %>%
      mutate(V4 = case_when(str_detect(V4, "NaN|Inf") ~ "--", TRUE ~ V4))
    
    colnames(res) <- c("Question", "Projection", "Observed", "Percentage")
    rownames(res) <- NULL
    
    return(res)
    
  })
  
  output$table8B <- DT::renderDataTable(
    {
      req(tbl.q8B())
      tbl.q8B()
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END -
  
  # Download IDs for Q8B ----
  output$download_q8B_ids <- downloadHandler(
    filename = function() {
      paste0("pmt_q8B_ids_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(
        tbl.q8B.counts() %>% arrange(record_id),
        file
      )
    }
  )
  
  
  # Table 8C ----
  tbl.q8C <- reactive({
    req(tbl.q8C.counts(), proj_8C_vec())
    
    res <- cbind(
      # question labels
      c(
        "8C. Individuals who received services",
        "8C1. Crisis intervention (in-person, includes safety planning, etc.)",
        "8C2. Hotline/crisis line counseling",
        "8C3. On-scene crisis response (e.g., community crisis response)",
        "8C4. Individual counseling",
        "8C5. Support groups (facilitated or peer)",
        "8C6. Other therapy (traditional, cultural, etc.)"
      ),
      # projections (editable)
      proj_8C_vec(),
      # observed counts
      c(
        nrow(
          tbl.q8C.counts() %>% 
            group_by(mrn) %>% 
            summarise(total = sum(q.c.sum)) %>% 
            filter(total >0)
        ),
        sum(tbl.q8C.counts()$q.c1),
        0,
        sum(tbl.q8C.counts()$q.c3),
        sum(tbl.q8C.counts()$q.c4),
        0,
        sum(tbl.q8C.counts()$q.c6),
        0
      )
    ) %>%
      as.data.frame() %>%
      mutate(V4 = round((as.numeric(V3)/as.numeric(V2))*100, 1)) %>%
      mutate(V4 = paste0(V4, "%")) %>%
      mutate(V4 = case_when(str_detect(V4, "NaN|Inf") ~ "--", TRUE ~ V4))
    
    colnames(res) <- c("Question", "Projection", "Observed", "Percentage")
    rownames(res) <- NULL
    
    return(res)
    
  })
  
  output$table8C <- DT::renderDataTable(
    {
      req(tbl.q8C())
      tbl.q8C()
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END -
  
  # Download IDs for Q8C ----
  output$download_q8C_ids <- downloadHandler(
    filename = function() {
      paste0("pmt_q8C_ids_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(
        tbl.q8C.counts() %>% arrange(record_id),
        file
      )
    }
  )
  
  
  # Table 8D ----
  tbl.q8D <- reactive({
    req(tbl.q8D.counts(), proj_8D_vec())
    
    res <- cbind(
      # question labels
      c(
        "8D. Individuals who received services",
        "8D1. Emergency shelter or safe house",
        "8D2. Transitional housing",
        "8D3. Relocation assistance (includes assistance in obtaining housing)"
      ),
      # projections (editable)
      proj_8D_vec(),
      # observed counts
      c(
        nrow(
          tbl.q8D.counts() %>% 
            group_by(mrn) %>% 
            summarise(total = sum(q.d.sum)) %>% 
            filter(total >0)
        ),
        sum(tbl.q8D.counts()$q.d1),
        0,
        sum(tbl.q8D.counts()$q.d3)
      )
    ) %>%
      as.data.frame() %>%
      mutate(V4 = round((as.numeric(V3)/as.numeric(V2))*100, 1)) %>%
      mutate(V4 = paste0(V4, "%")) %>%
      mutate(V4 = case_when(str_detect(V4, "NaN|Inf") ~ "--", TRUE ~ V4))
    
    colnames(res) <- c("Question", "Projection", "Observed", "Percentage")
    rownames(res) <- NULL
    
    return(res)
    
  })
  
  output$table8D <- DT::renderDataTable(
    {
      req(tbl.q8D())
      tbl.q8D()
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END -
  
  # Download IDs for Q8D ----
  output$download_q8D_ids <- downloadHandler(
    filename = function() {
      paste0("pmt_q8D_ids_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(
        tbl.q8D.counts() %>% arrange(record_id),
        file
      )
    }
  )
  
  
  # Table 8E ----
  tbl.q8E <- reactive({
    req(tbl.q8E.counts(), proj_8E_vec())
    
    res <- cbind(
      # question labels
      c(
        "8E. Individuals who received services",
        "8E1. Notification of criminal justice events",
        "8E2. Victim impact statement assistance",
        "8E3. Assistance with restitution",
        "8E4. Civil legal assistance in obtaining protection or restraining order",
        "8E5. Civil legal assistance with family law issues",
        "8E6. Other emergency justice-related assistance",
        "8E7. Immigration attorney assistance",
        "8E8. Prosecution interview advocacy/accompaniment",
        "8E9. Law enforcement interview advocacy/accompaniment",
        "8E10. Criminal advocacy/accompaniment",
        "8E11. Other legal advice and/or counsel"
      ),
      # projections (editable)
      proj_8E_vec(),
      # observed counts
      c(
        nrow(
          tbl.q8E.counts() %>% 
            group_by(mrn) %>% 
            summarise(total = sum(q.e.sum)) %>% 
            filter(total >0)
        ),
        0, 0, 0,
        sum(tbl.q8E.counts()$q.e4),
        0,
        sum(tbl.q8E.counts()$q.e6),
        0, 0, 0, 0,
        sum(tbl.q8E.counts()$q.e11)
      )
    ) %>%
      as.data.frame() %>%
      mutate(V4 = round((as.numeric(V3)/as.numeric(V2))*100, 1)) %>%
      mutate(V4 = paste0(V4, "%")) %>%
      mutate(V4 = case_when(str_detect(V4, "NaN|Inf") ~ "--", TRUE ~ V4))
    
    colnames(res) <- c("Question", "Projection", "Observed", "Percentage")
    rownames(res) <- NULL
    
    return(res)
    
  })
  
  output$table8E <- DT::renderDataTable(
    {
      req(tbl.q8E())
      tbl.q8E()
    },
    options = list(
      paging = FALSE, scrollX = TRUE, scrollCollapse = TRUE, scrollY = '300px',
      initComplete = JS(
        "function(settings, json) {",
        "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
        "}")
    )
  ) # renderDataTable - END -
  
  # Download IDs for Q8E ----
  output$download_q8E_ids <- downloadHandler(
    filename = function() {
      paste0("pmt_q8E_ids_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(
        tbl.q8E.counts() %>% arrange(record_id),
        file
      )
    }
  )
  
  
  # ---- SSL Report (services-level export) ----
  
  # Build a per-MRN demographic table (race, age, gender, new)
  demo.ssl <- reactive({
    req(DF.total())
    
    DF.total() %>% 
      mutate(
        # race levels (same logic as Q4)
        race1_level = case_when(race_pt___4 == 1 ~ 1, TRUE ~ 0),  # American Indian/ Alaska Native
        race2_level = case_when(race_pt___3 == 1 ~ 1, TRUE ~ 0),  # Asian
        race3_level = case_when(race_pt___2 == 1 ~ 1, TRUE ~ 0),  # Black/ African American
        race4_level = case_when(ethnicity_pt == 1 & race3_level == 0 ~ 1, TRUE ~ 0), # Hispanic or Latino
        race5_level = case_when(race_pt___5 == 1 ~ 1, TRUE ~ 0),  # Native Hawaiian & Other Pacific Islander
        race6_level = case_when(race_pt___1 == 1 & ethnicity_pt != 1 ~ 1, TRUE ~ 0),  # White Non-Latino/ Caucasian
        race7_level = case_when(
          race_pt___77 == 1 |
            (ethnicity_pt != 99 &
               (race1_level + race2_level + race3_level + race4_level + race5_level + race6_level) == 0) ~ 1, TRUE ~ 0
        ), # Some Other Race
        race8_level = case_when(
          race_pt___1 + race_pt___2 + race_pt___3 + race_pt___4 + race_pt___5 + race_pt___77 >1 ~ 1,
          TRUE ~ 0
        ), # Multiple Races
        race9_level = case_when(
          race_pt___99 == 1 & 
            ((race1_level + race2_level + race3_level + race4_level + race5_level + race6_level + race7_level) == 0) ~ 1, 
          TRUE ~ 0
        ), # Not Reported
        
        race_label = case_when(
          race8_level == 1 ~ "Multiple Races",
          race1_level == 1 ~ "American Indian/Alaska Native",
          race2_level == 1 ~ "Asian",
          race3_level == 1 ~ "Black/African American",
          race4_level == 1 ~ "Hispanic/Latino",
          race5_level == 1 ~ "Native Hawaiian & Other Pacific Islander",
          race6_level == 1 ~ "White Non-Latino/Caucasian",
          race7_level == 1 ~ "Some Other Race",
          race9_level == 1 ~ "Not Reported",
          TRUE ~ NA_character_
        ),
        
        # sex
        gender_label = case_when(
          sex_pt == 1 ~ "Male",
          sex_pt == 2 ~ "Female",
          sex_pt == 3 ~ "Other",
          sex_pt == 99 ~ "Not Reported",
          TRUE ~ NA_character_
        ),
        
        # calculate age
        age = as.numeric((visit.date - as.Date(dob_pt, "%Y-%m-%d")) / 365.25),
        
        new_flag = case_when(
          new.id == 1 ~ "Yes",
          new.id == 0 ~ "No",
          TRUE ~ NA_character_
        )
      ) %>% 
      distinct(mrn, .keep_all = TRUE) %>% 
      transmute(
        mrn,
        race   = race_label,
        age    = age,
        gender = gender_label,
        new    = new_flag
      )
  })
  
  # Victimization + Special Classification codes (Table 5 logic) by MRN
  victclass.ssl <- reactive({
    req(tbl.q5.counts.temp())
    
    tbl.q5.counts.temp() %>%
      select(mrn, starts_with("vict"), starts_with("class")) %>%
      distinct(mrn, .keep_all = TRUE)
  })
  
  # Build SSL services report
  tbl.ssl.report <- reactive({
    req(tbl.q8.temp(), demo.ssl(), victclass.ssl())
    
    tbl.q8.temp() %>%
      # Add provider info
      mutate(
        service_provider_code = as.character(service_provider),
        service_provider_name = dplyr::recode(service_provider_code, !!!mapping_service_provider)
      ) %>%
      # Add demo
      left_join(demo.ssl(), by = "mrn") %>%
      # Add Table 5 flags (vict/class) by MRN
      left_join(victclass.ssl(), by = "mrn") %>%
      
      # Build the two new string fields
      mutate(
        `Victimization Category` = case_when(vict1 == 1 ~ "1", TRUE ~ ""),
        `Victimization Category` = case_when(vict2 == 1 ~ paste0(`Victimization Category`, ", 2"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict4 == 1 ~ paste0(`Victimization Category`, ", 4"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict5 == 1 ~ paste0(`Victimization Category`, ", 5"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict6 == 1 ~ paste0(`Victimization Category`, ", 6"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict10 == 1 ~ paste0(`Victimization Category`, ", 10"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict12 == 1 ~ paste0(`Victimization Category`, ", 12"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict16 == 1 ~ paste0(`Victimization Category`, ", 16"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict21 == 1 ~ paste0(`Victimization Category`, ", 21"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict22 == 1 ~ paste0(`Victimization Category`, ", 22"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(vict26 == 1 ~ paste0(`Victimization Category`, ", 26"), TRUE ~ `Victimization Category`),
        `Victimization Category` = case_when(
          substr(`Victimization Category`, 1, 2) == ", " ~ substr(`Victimization Category`, 3, nchar(`Victimization Category`)),
          TRUE ~ `Victimization Category`
        ),
        
        `Special Classification` = case_when(class1 == 1 ~ "D", TRUE ~ ""),
        `Special Classification` = case_when(class2 == 1 ~ paste0(`Special Classification`, ", H"), TRUE ~ `Special Classification`),
        `Special Classification` = case_when(class3 == 1 ~ paste0(`Special Classification`, ", I"), TRUE ~ `Special Classification`),
        `Special Classification` = case_when(class4 == 1 ~ paste0(`Special Classification`, ", LGBTQ"), TRUE ~ `Special Classification`),
        `Special Classification` = case_when(class5 == 1 ~ paste0(`Special Classification`, ", V"), TRUE ~ `Special Classification`),
        
        `Special Classification` = case_when(class6_pd == 1 ~ paste0(`Special Classification`, ", PD"), TRUE ~ `Special Classification`),
        `Special Classification` = case_when(class6_bl == 1 ~ paste0(`Special Classification`, ", BL"), TRUE ~ `Special Classification`),
        `Special Classification` = case_when(class6_dem == 1 ~ paste0(`Special Classification`, ", MD"), TRUE ~ `Special Classification`),
        `Special Classification` = case_when(class6_dev == 1 ~ paste0(`Special Classification`, ", CD"), TRUE ~ `Special Classification`),
        
        `Special Classification` = case_when(class7 == 1 ~ paste0(`Special Classification`, ", E"), TRUE ~ `Special Classification`),
        `Special Classification` = case_when(class8 == 1 ~ paste0(`Special Classification`, ", O"), TRUE ~ `Special Classification`),
        
        `Special Classification` = case_when(
          substr(`Special Classification`, 1, 2) == ", " ~ substr(`Special Classification`, 3, nchar(`Special Classification`)),
          TRUE ~ `Special Classification`
        )
      ) %>%
      
      # Q8 summary components (no sums or overall flags)
      mutate(
        q.a1  = serv_count_crim_just,
        q.a2  = serv_count_victim_rights,
        q.a3  = serv_count_oth_vict,
        q.a4  = serv_count_all_oth,
        
        q.b1  = serv_count_advoc,
        q.b3  = serv_count_law_enforc,
        q.b4  = serv_count_indiv_advoc,
        q.b5  = serv_count_med_for_exam,
        q.b10 = serv_count_interp_serv,
        
        q.c1  = serv_count_crisis_interv,
        q.c3  = serv_count_crisis_resp,
        q.c4  = serv_count_ind_couns,
        q.c6  = serv_count_other_serv,
        
        q.d1  = serv_count_emerg_shelt,
        q.d3  = serv_count_reloc_assist,
        
        q.e4  = serv_count_civ_leg_assist,
        q.e6  = serv_count_oth_emerg_just,
        q.e11 = serv_count_oth_legal
      ) %>%
      
      # Put the two new columns immediately after Date
      select(
        service_provider_name,
        service_provider_code,
        record_id,
        date_service,
        `Victimization Category`,
        `Special Classification`,
        race,
        age,
        gender,
        new,
        
        # Q8 components (only)
        q.a1, q.a2, q.a3, q.a4,
        q.b1, q.b3, q.b4, q.b5, q.b10,
        q.c1, q.c3, q.c4, q.c6,
        q.d1, q.d3,
        q.e4, q.e6, q.e11,
        
        # Original 0/1 service count columns at the end
        serv_count_crim_just, serv_count_victim_rights, serv_count_oth_vict, serv_count_all_oth,
        serv_count_advoc, serv_count_law_enforc, serv_count_indiv_advoc, serv_count_med_for_exam, serv_count_interp_serv,
        serv_count_crisis_interv, serv_count_crisis_resp, serv_count_ind_couns, serv_count_other_serv,
        serv_count_emerg_shelt, serv_count_reloc_assist,
        serv_count_civ_leg_assist, serv_count_oth_emerg_just, serv_count_oth_legal
      ) %>%
      arrange(service_provider_name, record_id, date_service) %>%
      dplyr::rename(
        `Service Provider`      = service_provider_name,
        `Service Provider Code` = service_provider_code,
        `Record ID`             = record_id,
        `Service Date`          = date_service,
        Race                    = race,
        Age                     = age,
        Gender                  = gender,
        New                     = new
      )
  })
  
  # Download SSL report
  output$download_ssl <- downloadHandler(
    filename = function() {
      paste0("pmt_ssl_services_", getQuarter(), "_", getYear(), ".csv")
    },
    content = function(file) {
      readr::write_csv(tbl.ssl.report(), file)
    }
  )
  
  
  # ---- Make a .csv file to download Quarterly dataset ----
  
  ## custom functions ----
  spacer <- function(text) { return(c(text, "", "", "")) }
  noNames <- function(df) {
    colnames(df) <- c("V1", "V2", "V3", "V4")
    return(df)
  }
  
  ## append all report data on top of each other ----
  report.csv <- reactive({
    req(getQuarter(), getYear(), tbl.q1(), tbl.q3B(), tbl.q4(), tbl.q5(),
        tbl.q8A(), tbl.q8B(), tbl.q8C(), tbl.q8D(), tbl.q8E())
    
    rbind(
      
      spacer(paste0("PMT QUARTERLY REPORT: ", getQuarter(), ", ", getYear())),
      "",
      "",
      
      spacer("Q1. Total number of individuals who recived services during the reporting period"),
      spacer(nrow(tbl.q1())),
      "",
      
      spacer("Q2. Total number of anonymous contacts receive during the reporting period"),
      "",
      
      spacer("Q3. Of the number of individuals entered in Q1, how many were NEW individuals who received servicse from your agency for the first time during the reporting period"),
      spacer(nrow(tbl.q3B())),
      "",
      "",
      
      spacer("Q4. Demographics (for NEW individuals identified in Question 3)"),
      c(colnames(tbl.q4()), ""),
      noNames(cbind(tbl.q4(), "")),
      "",
      
      spacer("Q5. Types of Victimizations (for ALL individuals identified in Question 1 & 2)"),
      c(colnames(tbl.q5()), ""),
      noNames(cbind(tbl.q5(), "")),
      "",
      
      spacer("Q8. Total number of individuals who received services AND number of times each service was provided during the reporting period:"),
      spacer("A. INFORMATION & REFERRAL"),
      colnames(tbl.q8A()),
      noNames(tbl.q8A()),
      "",
      spacer("B. PERSONAL ADVOCACY/ACCOMPANIMENT"),
      colnames(tbl.q8B()),
      noNames(tbl.q8B()),
      "",
      spacer("C. EMOTIONAL SUPPORT OR SAFETY SERVICES"),
      colnames(tbl.q8C()),
      noNames(tbl.q8C()),
      "",
      spacer("D. SHELTER/HOUSING SERVICES"),
      colnames(tbl.q8D()),
      noNames(tbl.q8D()),
      "",
      spacer("E. CRIMINAL/CIVIL JUSTICE SYSTEM ASSISTANCE"),
      colnames(tbl.q8E()),
      noNames(tbl.q8E()),
      ""
    )
    
  })
  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PMT_report_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(report.csv(), file, row.names = FALSE)
    }
  )
  
  
} # end of server





# Run the application 
shinyApp(ui = ui, server = server)
