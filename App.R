# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
#library(shinyBS)
library(ECharts2Shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(data.table)
library(shinyjs)
library(VennDiagram)
library(shinycssloaders)
library(DT)
library(shinycustomloader)
library(shinymeta)
library(shinyWidgets)
library(shinybusy)

library_code <- quote({
  library(dplyr)
  library(lubridate)})

eval(library_code)



#colors <- c('rgb(204,0,102)', 'rgb(0,153,76)','rgb(0,102,204)', 'rgb(128,133,133)', 'rgb(144,103,167)')

colors <- c('#E69F00', '#56B4E9','#009E73', '#F0E442', '#0072B2', '#D55E00', '#CC79A7')

#setwd("/Users/sycho/Dropbox/Columbia_PhD/Dissertation/Aim_3-Support_Tool/Shiny_App_Wearable_DQ_Tool/Data")

data_code <- quote({
  load("./Data/Fitness_Tracker_Data.Rdata")})

eval(data_code)

#extract phenotype data for subjects
mdatpheno2$BMI <- round(mdatpheno2$BMI,1)
followup <- mdatpheno2 %>% group_by(ID) %>% summarise(dur = max(lubridate::date(Time)) - min(lubridate::date(Time)) + 1)
ave_step <- mdatpheno2 %>% group_by(ID, lubridate::date(Time)) %>% summarize(step_sum = sum(Steps)) %>% group_by(ID) %>% summarize(ave_step = ceiling(mean(step_sum)))
ave_hr <- mdatpheno2 %>% group_by(ID) %>% summarize(hr_ave = mean(BPM, na.rm = T))

mdatpheno2$date <-substr(mdatpheno2$Time,1,10)
#mdatpheno2$hour <-substr(mdatpheno2$Time,12,13)

mdatpheno2$DayTime <- substr(mdatpheno2$Time,1,13)
mdatpheno2$Enrollment <-mdatpheno2$date
mdatpheno2$Gender <- recode(mdatpheno2$Gender, F = "Female", M = "Male") 
mdatpheno2$Ethnicity <- recode(mdatpheno2$Ethnicity, CN = "Chinese", IN = "Indian", Others = "Others", MY = "Malaysian")
pheno <- mdatpheno2 %>% group_by(ID) %>% filter(row_number(Ethnicity) == 1)
pheno$followup <-followup$dur
pheno$ave_step <- ave_step$ave_step
pheno$ave_hr <- ave_hr$hr_ave
stepdatperday <- mdatpheno2 %>% filter(Steps>0) %>% group_by(date) %>% summarise(count = n_distinct(ID))
heartdatperday <- mdatpheno2 %>% filter(BPM>0) %>% group_by(date) %>% summarise(count = n_distinct(ID))

func_code <- quote({
  
  #granularity of data 
  unit <- as.numeric(mdatpheno2$Time[2] - mdatpheno2$Time[1])
  
  # function for generating inactivity period threshold that fits the data (to use for the UI)
  no_rem <- function(x){
    if (x %% unit == 0) {
      return(x)
    } 
  }
  
  # function to calculate maximum number of consecutive days 
  max_consec <- function(x) {
    y <- c(unclass(diff(x)))  # c and unclass -- preparing it for rle
    r <- rle(y)
    if(any(r$values %in% 1)){with(r, max(lengths[values==1])) + 1} 
    else {1}
  }
  
  max_consec_month <- function(year, month){
    tm <- year * 12 + month - 1
    y <- diff(tm)
    r <- rle(y)
    if(any(r$values %in% 1)){with(r, max(lengths[values==1])) + 1} 
    else {1}
  }
})

eval(func_code)
# Application title
header <- dashboardHeader(title = tags$strong("DQ Characterization for Fitness Tracker Data"), titleWidth = 500)

# Sidebar with a slider input for number of bins 
sidebar <- dashboardSidebar(
  
  #    fileInput("file1", "Choose CSV File",
  #              multiple = TRUE,
  #              accept = c("text/csv",
  #                         "text/comma-separated-values,text/plain",
  #                         ".csv")),
  
  # Horizontal line ----
  #    tags$hr(),
  
  sidebarMenu( 
    menuItem("Demographics",
             
             useShinyjs(),
             div(id = 'demo',
                 
                 checkboxGroupInput(inputId = "Gender",
                                    label = "Select Gender",
                                    choices = unique(pheno$Gender), 
                                    selected = unique(pheno$Gender)), 
                 checkboxGroupInput(inputId = "Ethnicity",
                                    label = "Select Ethnicity",
                                    choices = sort(unique(pheno$Ethnicity)), 
                                    selected = unique(pheno$Ethnicity)),
                 sliderInput("Age",
                             "Select Age Range",
                             min = min(pheno$Age),
                             max = max(pheno$Age),
                             value = c(min(pheno$Age), max(pheno$Age)))
             ), actionButton("reset_demo", "Reset selection")
    ), 
    
    
    menuItem("Clinical Phenotype",
             
             useShinyjs(),
             div(id = 'pheno',
                 
                 sliderInput("BMI",
                             label = "Select BMI Range",
                             min = min(pheno$BMI),
                             max = max(pheno$BMI),
                             value = c(min(pheno$BMI),max(pheno$BMI))),
                 tags$hr(),
                 sliderInput("SBP", 
                             "Range of Systolic Blood Pressure",
                             min = min(pheno$SBP),
                             max = max(pheno$SBP),
                             value = c(min(pheno$SBP),max(pheno$SBP))),
                 sliderInput("DBP",
                             "Range of Diastolic Blood Pressure",
                             min = min(pheno$DBP),
                             max = max(pheno$DBP),
                             value = c(min(pheno$DBP),max(pheno$DBP))),
                 tags$hr(),
                 sliderInput("TotalChol",
                             "Range of Total Cholesterol",
                             min = min(pheno$TotalChol),
                             max = max(pheno$TotalChol),
                             value = c(min(pheno$TotalChol),max(pheno$TotalChol))),
                 sliderInput("HDL",
                             "Select HDL Range",
                             min = min(pheno$HDL),
                             max = max(pheno$HDL),
                             value = c(min(pheno$HDL),max(pheno$HDL))),
                 sliderInput("LDL",
                             "Select LDL Range",
                             min = min(pheno$LDL, na.rm = TRUE),
                             max = max(pheno$LDL, na.rm = TRUE),
                             value = c(min(pheno$LDL, na.rm = TRUE),max(pheno$LDL, na.rm = TRUE)))
             ), actionButton("reset_pheno", "Reset selection")
    ),
    
    menuItem("Metadata",
             
             useShinyjs(),
             div(id = 'meta',
                 
                 dateRangeInput("daterange",
                                "Select Date Range",
                                start = min(lubridate::date(pheno$Time)),   #daily_steps$date
                                end = max(lubridate::date(pheno$Time)), # daily_steps$date
                                format = "mm/dd/yyyy",
                                startview = "month",
                                weekstart = 0,
                                separator = " to "),
                 checkboxGroupInput(inputId = "DevType",
                                    label = "Select Type of Device",
                                    choices = unique(pheno$device),
                                    selected = unique(pheno$device))
             ), actionButton("reset_meta", "Reset selection")
    )
    
    #        menuItem("Fitness Data",
    #                 checkboxGroupInput(inputId = "fitness_dat", 
    #                                    label = "Select variable of interest",
    #                                    choices = c("Step", "Heart Rate"))
    
    #                uiOutput("fit_choices")
    
    #                 sliderInput("Avg_Step",
    #                             "Average Step count",
    #                             min = min(pheno$ave_step, na.rm = TRUE),
    #                             max = max(pheno$ave_step, na.rm = TRUE),
    #                             value = c(min(pheno$ave_step, na.rm = TRUE),max(pheno$ave_step, na.rm = TRUE))),
    #                 sliderInput("Avg_HR",
    #                             "Average Heart Rate",
    #                             min = min(pheno$ave_hr, na.rm = TRUE),
    #                             max = max(pheno$ave_hr, na.rm = TRUE),
    #                             value = c(min(pheno$ave_hr, na.rm = TRUE),max(pheno$ave_hr, na.rm = TRUE)))
    #        )
  )
)




body <- dashboardBody(
  tabsetPanel(
    #height = "2500px",
    #width = "600px",
    id = "Tabs",
    #Panel to describe the tool
    tabPanel("About",
             fluidRow(
               box(title = "Objective of the tool", status = "primary", width = 12, p("Lack of trust in data quality can be/has been identified as a barrier to use of digital health data. 
      Therefore, understanding the data and assessing its quality is important to promote the reuse of wearable device data."), 
                   p("However, assessing data quality is not easy: it is time-consuming, cumbersome, and resource-intensive. 
      A tool that supports researchers in determining the viability of using a dataset for their intended research is necessary."),
                   p("We wanted to build a", strong("tool that can characterize data in terms of measures that would truly help researchers determine fitness-for-use. 
      In this study, we focused on fitness for use data completeness measures on fitness tracker data such as step count and heart rate."))),
               box(title = "How to use the tool", status = "primary", width = 12, p(strong("Sidebar: "), "You can customize your cohort of interest by subsetting the cohort based on your input"),
                   p(strong("Overview: "), "This tab provides an overview of the data for your cohort of interest."),
                   p(strong("Explore Individual Data: "), "This tab provides a detailed view on individual's data. Individuals can be selected from your cohort of interest.."),
                   p(strong("Missing Data Analysis: "), "This tab allows you to view data missingness (NA & O's) of your cohort of interest. The figures show the distribution of the percentage of missingness in individual's data."),
                   p(strong("Define 'Data Completeness': "),"This tab allows you to define what data completeness means in your research. Based on current literature, we allow users to first define what is a valid day - a day that has enough data that can be included in the analysis. The users can then select how many valid days are needed in the research and whether they need to be consecutive. There are also advanced features for research comparing weekday vs. weekend or analyzing trends over months. Additional features will be added in the future version of this tool."),
                   p(strong("Summary of Cohort with Complete Data: "), "This tab shows how many individuals have complete data (based on completeness defined by users), and shows data summary once more on the cohort that has complete data.")
               )
             )),
    
    # Panel to show data summary
    tabPanel("Overview",
             fluidRow(
               #tags$h2(tags$strong("Data Summary")),
               
               valueBoxOutput("N_Participants"),
               valueBoxOutput("N_Measurements"),
               valueBoxOutput("N_Days"),
               valueBoxOutput("granularity"),
               valueBoxOutput("Part_Step"),
               valueBoxOutput("Part_Heart"),
               valueBoxOutput("N_variables")
               
             ),
             
             fluidRow(
               box(title = "Number of participants with data", status = "primary", withLoader(plotOutput("var_plot", height = 250), loader = "loader3"),width=4),
               box(title = "Device Type", status = "primary", withLoader(plotlyOutput("device_plot", height = 250), loader = "loader3"),width=4),
               box(title = "Duration of Data Collection", status = "primary", withLoader(plotlyOutput("duration_plot", height = 250), loader = "loader3"),width=4)
             ),
             
             fluidRow(
               box(title = "Gender Summary", status = "primary", withLoader(plotlyOutput("gen_plot", height = 250), loader = "loader3"),width=4),
               box(title = "Ethnicity Summary", status = "primary", withLoader(plotlyOutput("eth_plot", height = 250), loader = "loader3"),width=4),
               box(title = "Age Distribution", status = "primary", withLoader(plotlyOutput("age_plot", height = 250), loader = "loader3"),width=4)),
             
             fluidRow(
               box(title = "BMI Distribution", status = "primary", withLoader(plotlyOutput("bmi_plot", height = 250), loader = "loader3"),width=4),
               box(title = "Blood Pressure Distribution", status = "primary", withLoader(plotlyOutput("bp_plot", height = 250), loader = "loader3"),width=4),
               box(title = "Cholesterol Distribution", status = "primary", withLoader(plotlyOutput("chol_plot", height = 250), loader = "loader3"),width=4)),
             
             fluidRow(
               box(title = "Average Step Count (for each individual)", status = "primary", withLoader(plotlyOutput("step_plot", height = 250), loader = "loader3"), width=6),
               box(title = "Average Daily Heart Rate (for each individual)", status = "primary", withLoader(plotlyOutput("hr_plot", height = 250), loader = "loader3"),width=6)),
             
             fluidRow(
               box(title = "Participants with Step Data", status = "primary", withLoader(plotlyOutput("stepday_plot", height = 250), loader = "loader3"),width=6),
               box(title = "Participants with Heart Data", status = "primary", withLoader(plotlyOutput("heartday_plot", height = 250), loader = "loader3"),width=6)
             ),
             
             fluidRow(
               box(title = "Other Baseline Charactersitics", status = "primary", withLoader(DT::dataTableOutput("mytable1"), loader = "loader3"), width=12),
             )),
    
    
    tabPanel("Explore Invidiual Data", 
             fluidRow(
               box(title = tags$strong("Explore Fitness Tracker Data of Individual Participants"), width = 12, status = "primary",  
                   tags$head(tags$style(HTML(".selectize-input {height: 20px; width: 400px;}"))),
                   selectInput("Subject", "Select Subject ID", choices = NULL),
                   #numericInput(inputId = "Subject", "Subject #:", 426, min = 1, max = length(pheno$ID)),
                   tags$br(),
                   textOutput("textindv"),
                   tags$br(),
                   tags$br(),
                   plotlyOutput("allsteps_plot", height = 250),
                   tags$br(),
                   tags$br(),
                   plotlyOutput("allhearts_plot", height = 250),
                   tags$br(),
                   tags$br()#,
                   #plotOutput("ind_na_step_missing",height = 350)
                   
               )),
             fluidRow(
               box(title = tags$strong("Active status (at least one step per hour)"), width = 12, status = "primary",
                   plotOutput("ind_na_step_missing",height = 350)
               )),
             fluidRow(
               tabBox(title = tags$strong("Proportion of NA per day"), width = 6, side = "right",
                      tabPanel("Heart Rate",
                               #selectInput("Subject_1", "Select Subject ID", choices = pheno$ID, selected = pheno$ID[1]),
                               plotOutput("ind_na_HR")),
                      tabPanel("Step",
                               #selectInput("Subject_2", "Select Subject ID", choices = pheno$ID, selected = pheno$ID[1]),
                               plotOutput("ind_na_Step")
                      )
               ),
               tabBox(title = tags$strong("Proportion of 0's per day"), width = 6, side = "right",
                      tabPanel("Heart Rate",
                               #selectInput("Subject_3", "Select Subject ID", choices = pheno$ID, selected = pheno$ID[1]),
                               plotOutput("ind_zero_HR")
                      ),
                      tabPanel("Step",
                               #selectInput("Subject_4", "Select Subject ID", choices = pheno$ID, selected = pheno$ID[1]),
                               plotOutput("ind_zero_Step")
                      )
               )
             )
             
    ),
    
    tabPanel("Missing Data Analysis",
             fluidRow(
               tabBox(title = tags$strong("Percentage of Missingness (NA)"), width = 6, side = "right",
                      tabPanel("Step", plotlyOutput("percent_NA_step")),
                      tabPanel("Heart Rate", plotlyOutput("percent_NA_HR"))),
               tabBox(title = tags$strong("Percentage of zero (0) values"), width = 6, side = "right",
                      tabPanel("Step", plotlyOutput("prop_zero_step")),
                      tabPanel("Heart Rate", plotlyOutput("prop_zero_hr"))
               )
             )
    ),
    
    #Panel for completeness definition
    tabPanel("Define 'Data Completeness'", 
             fluidRow(
               box(title = tags$strong("Step 1: Define Valid Day"), width = 12, status = "primary", tags$br(), 
                   tags$head(tags$style(HTML(".selectize-input {height: 20px; width: 400px;}"))),
                   
                   selectInput(inputId = "valid_day",
                               label = tags$h4(tags$strong("Select your criteria to determine a valid day"), tags$p(tags$h5("(A valid day is a day with sufficient amount of data to be included in the analysis. There are various ways to determine a valid day: (1) based on the amount of wear hour per day (which can again be determined based on step variable or heart rate variable), and (2) based on the number of step count.)"))  
                                               #bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")
                               ),
                               choices = c("Number of hours with Heart Rate data", "Number of hours with Step data", "Step Count"),
                               selected = "Number of hours with Heart Rate data"),
                   #bsPopover(id = "q1", title = "What is a valid day?", content = "A valid day is a day with sufficient amount of data to be included in the analyses", placement = "right", trigger = "click", options = list(container = "body")), 
                   tags$br(),
                   uiOutput("inactivity"),
                   textOutput("text1")   
               )),
             
             
             fluidRow(
               box(title = tags$strong("Step 2: Do you need your valid days to be consecutive?"), status = "primary",  width = 12,
                   
                   radioButtons("choice1", "Choose one:", choices = c("No","Yes"), selected = "No")
               )),
             
             
             fluidRow(
               box(title = tags$strong("Optional: Advanced Definition"), status = "primary",  width = 12,
                   
                   radioButtons("choice2", "Do you need advanced completeness definition (e.g., compare weekday vs. weekend, analyze trend over months)?", choices = c("No","Yes"), selected = "No"),
                   
                   conditionalPanel(
                     condition = "input.choice2 == 'Yes'",
                     radioButtons("choice3", "I need to:", choices = c("compare weekday vs. weekend","analyze trends/change over months"))
                   ),
                   
                   conditionalPanel(
                     condition = "input.choice2 == 'Yes' && input.choice3 == 'compare weekday vs. weekend'",  
                     selectInput("num_day_per_weekday", "How many valid days are needed for a weekday to be valid?", c(1:5), 3),
                     selectInput("num_day_per_weekend", "How many valid days are needed for a weekend to be valid?", c(1,2), 2)
                   ),
                   conditionalPanel(
                     condition = "input.choice2 == 'Yes' && input.choice3 == 'analyze trends/change over months'",
                     numericInput("num_day_per_month", value=3, min = 1, max = 31, tags$h4(tags$strong("How many valid days is needed per month to be valid?"))),
                     radioButtons("choice3_1", "Do the months need to be consecutive?", choices = c("No","Yes"), selected = "No")
                   )
                   
               )
             ),
             
             fluidRow(
               box(title = tags$strong("Step 3: Define completeness for individual subject"), status = "primary",  width = 12,
                   conditionalPanel(
                     condition = "input.choice1 == 'Yes' && input.choice2 == 'No'",
                     numericInput("consec_day", "How many consecutive days are needed in your analysis?", min = 2, value = 3),
                     textOutput("comp_consec_day")
                   ),
                   conditionalPanel(
                     condition = "input.choice1 == 'No' && input.choice2 == 'No'",
                     numericInput("comp_day", tags$h4(tags$strong("How many days are needed in your analysis?")), min = 1, value = 3),
                     #numericInput("dura", tags$h4(tags$strong("How many days can the first and last day can be apart?")), min = 1, value = 7)
                   ),
                   
                   conditionalPanel(
                     condition = "input.choice2 == 'Yes' && input.choice3 == 'compare weekday vs. weekend'",
                     textOutput("text2")
                   ),
                   
                   conditionalPanel(
                     condition = "input.choice2 == 'Yes' && input.choice3 == 'analyze trends/change over months' && input.choice3_1 == 'Yes'", 
                     numericInput("num_month", "How many valid months are needed in your analyses?", min = 2, value = 2),
                     textOutput("text3")
                   ),
                   conditionalPanel(
                     condition = "input.choice2 == 'Yes' && input.choice3 == 'analyze trends/change over months' && input.choice3_1 == 'No'", 
                     numericInput("num_month", "How many valid months are needed in your analyses?", min = 2, value = 2),
                     #numericInput("duration_month", "What is the duration of data collection? Please answer in number of months. (e.g., Duration from January to December is 12 months)", value =12, min = 2), 
                     #radioButtons("even", "Do the months need to be evenly distributed?", choices = c("No", "Yes"), selected = "No"),
                     textOutput("text4")
                   )
               )),
             
             fluidRow(
               useSweetAlert(),  
               column(12, actionButton("go", "Generate Report"), align = "center")
               
             )
             
    ),
    # tabPanel(tags$strong("Cohort Summary"), 
    #          fluidRow(
    #            #valueBoxOutput("Cohort_Initial"),
    #            valueBoxOutput("Cohort_Final") %>% withSpinner(proxy.height = '100px'),
    #            valueBoxOutput("Cohort_Final_Step") %>% withSpinner(proxy.height = '100px'),
    #            valueBoxOutput("Cohort_Final_HR") %>% withSpinner(proxy.height = '100px'),
    #          ),
    #          fluidRow(
    #              actionButton(inputId = "show_r_code", label = "Show R Code")
    #            )
    #          )
    
    tabPanel("Summary of Cohort with Complete Data", 
             add_busy_spinner(spin = "fading-circle", color = "#000066", position = "full-page"),
             fluidRow(
               box(title = tags$strong('Summary of cohort that meets your completeness definition'), status = 'primary',
                   textOutput("text5"), tags$p(),
                   #withLoader(valueBoxOutput("Cohort_Final"), loader = "loader3"),
                   valueBoxOutput("Cohort_Final"),
                   valueBoxOutput("Cohort_Final_Step"),
                   valueBoxOutput("Cohort_Final_HR"),
                   width = 12 
               )
             ),
             
             fluidRow(
               box(title = "Gender Summary", status = "primary", plotlyOutput("comp_gen_plot", height = 250), width=4),
               box(title = "Ethnicity Summary", status = "primary", plotlyOutput("comp_eth_plot", height = 250), width=4),
               box(title = "Age Distribution", status = "primary", plotlyOutput("comp_age_plot", height = 250), width=4)),
             
             fluidRow(
               box(title = "BMI Distribution", status = "primary", plotlyOutput("comp_bmi_plot", height = 250), width=4),
               box(title = "Blood Pressure Distribution", status = "primary", plotlyOutput("comp_bp_plot", height = 250),width=4),
               box(title = "Cholesterol Distribution", status = "primary", plotlyOutput("comp_chol_plot", height = 250),width=4)),
             
             fluidRow(
               box(title = "Duration of Data Collection", status = "primary", plotlyOutput("comp_duration_plot", height = 250),width=4),  
               box(title = "Device Type", status = "primary", plotlyOutput("comp_device_plot", height = 250),width=4),
               box(title = "Average Step Count", status = "primary", plotlyOutput("comp_step_plot", height = 250), width=4)),
             
             fluidRow(
               box(title = "Average Heart Rate", status = "primary", plotlyOutput("comp_hr_plot", height = 250),width=4),
               box(title = "Participants with Step Data", status = "primary", plotlyOutput("comp_stepday_plot", height = 250), width=4),
               box(title = "Participants with Heart Data", status = "primary", plotlyOutput("comp_heartday_plot", height = 250), width=4)
             ),
             
             fluidRow(
               column(12, actionButton(inputId = "show_r_code", label = "Show R Code"), align = "center")
             )
             
    )
    
  ))


ui <- dashboardPage(header, sidebar, body)


server <- function(input, output,session) {
  
  
  #    output$fit_choices <- renderUI({
  #        if (input$DevType == "Flex" & ){
  #            checkboxGroupInput("fit_var", "Select Variable of Interest", choices = c("Step"), selected = "Step")
  #            
  #        } else {
  #           checkboxGroupInput("fit_var", "Select Variable of Interest", choices = c("Step","Heart Rate"), selected = c("Step", "Heart Rate"))
  #            
  #        }
  #    })
  
  observeEvent(input$reset_demo, {
    reset("demo")
  })
  
  observeEvent(input$reset_pheno, {
    reset("pheno")
  })
  
  observeEvent(input$reset_meta, {
    reset("meta")
  })
  
  cohort_pheno <- reactive({
    pheno %>% filter(Gender %in% input$Gender,
                     Ethnicity %in% input$Ethnicity,
                     Age >= input$Age[1],
                     Age <= input$Age[2],
                     BMI >= input$BMI[1],
                     BMI <= input$BMI[2],
                     SBP >= input$SBP[1],
                     SBP <= input$SBP[2],
                     DBP >= input$DBP[1],
                     DBP <= input$DBP[2],
                     TotalChol >= input$TotalChol[1],
                     TotalChol <= input$TotalChol[2],
                     HDL >= input$HDL[1],
                     HDL <= input$HDL[2],
                     LDL >= input$LDL[1],
                     LDL <= input$LDL[2],
                     lubridate::date(Time) >= input$daterange[1],
                     lubridate::date(Time) <= input$daterange[2],
                     device %in% input$DevType
                     
    )
  }) 
  
  cohort <- metaReactive(bindToReturn = TRUE,{
    mdatpheno2 %>% filter(Gender %in% !!input$Gender,
                          Ethnicity %in% !!input$Ethnicity,
                          Age >= !!input$Age[1],
                          Age <= !!input$Age[2],
                          BMI >= !!input$BMI[1],
                          BMI <= !!input$BMI[2],
                          SBP >= !!input$SBP[1],
                          SBP <= !!input$SBP[2],
                          DBP >= !!input$DBP[1],
                          DBP <= !!input$DBP[2],
                          TotalChol >= !!input$TotalChol[1],
                          TotalChol <= !!input$TotalChol[2],
                          HDL >= !!input$HDL[1],
                          HDL <= !!input$HDL[2],
                          LDL >= !!input$LDL[1],
                          LDL <= !!input$LDL[2],
                          lubridate::date(Time) >= !!input$daterange[1],
                          lubridate::date(Time) <= !!input$daterange[2],
                          device %in% !!input$DevType
                          
    )
  })
  
  
  nsubjs <- reactive({
    length(cohort_pheno()$ID)
  })
  
  nmeasure <- reactive({
    length(cohort()$ID)
  })
  
  ndays <- reactive({
    max(lubridate::date(cohort()$Time)) - min(lubridate::date(cohort()$Time)) + 1
  })
  
  nstepdata <- reactive({
    cohort() %>% filter(!is.na(Steps)) %>% distinct(ID) %>% count()
  }) 
  
  nheartdata <- reactive({
    cohort() %>% filter(!is.na(BPM)) %>% distinct(ID) %>% count()
  })
  
  nstephrdata <- reactive({
    cohort() %>% filter(!is.na(Steps)) %>% filter(!is.na(BPM)) %>% distinct(ID) %>% count()
  })
  
  nvars <- reactive({
    cohort() %>% distinct(device) %>% count()
  }) 
  
  
  indsubject <- reactive({
    subset(cohort(), cohort()$ID == input$Subject)
  })
  
  indsubjectpheno <- reactive({
    subset(cohort_pheno(), cohort_pheno()$ID == input$Subject)
  })
  
  indsubjectMissing <- reactive({
    subset(cohort(), cohort()$ID == input$Subject)%>% group_by(DayTime) %>% summarize(stepss = mean(Steps, na.rm = T)) %>% mutate(stepyn = ifelse(stepss>0,"Active","Inactive")) %>% mutate(date = substr(DayTime,1,10)) %>% mutate(hour = substr(DayTime,12,13)) %>% mutate (hour2 = ifelse(hour=='00','24',hour))
  })
  
  
  
  output$N_Participants <- renderValueBox({ valueBox(nsubjs(), tags$strong("Total number of participants"))})
  output$N_Measurements <- renderValueBox({ valueBox(nmeasure(), tags$strong("Number of Measurements"),color="yellow")})
  output$N_Days <- renderValueBox({ valueBox(ndays(),tags$strong("Number of days"),color="purple")})
  output$granularity <- renderValueBox({valueBox(paste0(unit, " minutes"), "Temporal Granularity", color = "blue")})
  output$Part_Step <- renderValueBox({ valueBox(nstepdata(),tags$strong(" Participants with Step Data"),color = "orange")})
  output$Part_Heart <- renderValueBox({ valueBox(nheartdata(),tags$strong(" Participants with Heart Rate Data"),color = "fuchsia")})
  output$N_variables <- renderValueBox({ valueBox(nvars(),tags$strong(" Device Types"),color = "navy")})
  
  # Draw plot of cohort summary    
  output$var_plot <- renderPlot({
    draw.pairwise.venn(
      area1 = as.numeric(nstepdata()),
      area2 = as.numeric(nheartdata()),
      cross.area = as.numeric(nstephrdata()),
      category = c("Steps", "Steps & Heart rate"),
      col = c("#fde725ff", "#440154ff"),
      fill = c(alpha("#fde725ff", 0.3),alpha("#440154ff", 0.3)),
      ext.text = FALSE,
      ext.percent = c(1,1,1),
      #ext.length = 0.4,
      ext.pos = c(0, 0),
      label.col = rep("gray10",3),
      lwd = 1,
      cex = 1,
      fontfamily = "sans",
      fontface = "plain",
      cat.cex = 1,
      cat.fontfamily = "sans",
      cat.fontface = "bold",
      cat.default.pos = ("text"),
      cat.pos = c(0, 0),
      cat.dist = c(0.055, 0.055),
      offset = 0.7,
      print.mode = c("raw", "percent")
    )
  })
  
  # Draw plot of cohort summary    
  output$gen_plot <- renderPlotly(
    plot1 <- plot_ly(cohort_pheno(), labels= ~(cohort_pheno()$Gender), type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent', 
                     insidetextfont = list(color = '#FFFFFF'),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1))
    ))
  
  
  output$eth_plot <- renderPlotly(
    plot2 <- plot_ly(cohort_pheno(),labels= ~(cohort_pheno()$Ethnicity), type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1))
    )) 
  
  output$age_plot <- renderPlotly(
    plot3 <- plot_ly(cohort_pheno(),x= ~(cohort_pheno()$Age), type = 'histogram', marker = list(colors = colors,
                                                                                                line = list(color = '#FFFFFF', width = 1))
    ) %>% layout(xaxis=list(title = "Age"),yaxis=list(title = "Number of participants"))) 
  
  output$bmi_plot <- renderPlotly(
    plot_ly(cohort_pheno(),x= ~(cohort_pheno()$BMI), type = 'histogram', marker = list(colors = colors,
                                                                                       line = list(color = '#FFFFFF', width = 1))
    ) %>% layout(xaxis=list(title = "BMI"),yaxis=list(title = "Number of participants"))) 
  
  output$bp_plot <- renderPlotly(
    plot4 <- plot_ly(cohort_pheno()) 
    %>% add_histogram(x= ~(cohort_pheno()$SBP),name = 'Systolic BP')
    %>% add_histogram(x= ~(cohort_pheno()$DBP),name = 'Diastolic BP')
    %>% layout(legend = list(font = list(size = 8),orientation = "h", xanchor = "center", x = 0.5,y=100),xaxis=list(title = "Blood Pressure (mmHg)"),yaxis=list(title = "Number of participants")))
  
  output$chol_plot <- renderPlotly(
    plot4 <- plot_ly(cohort_pheno())
    %>% add_histogram(x= ~(cohort_pheno()$TotalChol),name='Total Cholesterol') 
    %>% add_histogram(x = ~(cohort_pheno()$LDL),name='LDL') %>% add_histogram(x = ~(cohort_pheno()$HDL),name='HDL') 
    %>% layout(legend = list(font = list(size = 8),orientation = "h", xanchor = "center", x = 0.5,y=100),xaxis=list(title = "Cholesterol (mmol/L)"),yaxis=list(title = "Number of participants"))) 
  
  
  output$duration_plot <- renderPlotly(
    plot4 <- plot_ly(cohort_pheno(),x= ~(cohort_pheno()$followup),type = 'histogram', marker = list(colors = colors,
                                                                                                    line = list(color = '#FFFFFF', width = 1))
    ) %>% layout(xaxis=list(title = "Number of days"),yaxis=list(title = "Number of participants")))
  
  
  output$device_plot <- renderPlotly(
    plot5 <- plot_ly(cohort_pheno(),labels= ~(cohort_pheno()$device),type = 'pie', textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     marker = list(colors = colors,
                                   line = list(color = '#FFFFFF', width = 1))
    ))  
  
  output$step_plot <- renderPlotly(
    plot6 <- plot_ly(cohort_pheno(),x= ~(cohort_pheno()$ave_step),type = 'histogram', marker = list(colors = colors,
                                                                                                    line = list(color = '#FFFFFF', width = 1))
    ) %>% layout(xaxis=list(title = "Average Step Count"),yaxis=list(title = "Number of participants")))
  
  output$hr_plot <- renderPlotly(
    plot7 <- plot_ly(cohort_pheno(),x= ~(cohort_pheno()$ave_hr),type = 'histogram', marker = list(colors = colors,
                                                                                                  line = list(color = '#FFFFFF', width = 1))
    ) %>% layout(xaxis=list(title = "Average Heart Rate (BPM)"),yaxis=list(title = "Number of participants")))
  
  output$stepday_plot <- renderPlotly(
    plot8 <- plot_ly(stepdatperday,x= ~date, y= ~count, mode='line')
    %>% layout(xaxis=list(title = "Participants with recorded step count per day"), xaxis=list(title ="Date"), yaxis=list(title = "Number of participants"))
  )
  
  output$heartday_plot <- renderPlotly(
    plot9 <- plot_ly(heartdatperday,x= ~date, y= ~count, mode='line')
    %>% layout(xaxis=list(title = "Participants with recorded heart rate per day"), xaxis=list(title ="Date"), yaxis=list(title = "Number of participants"))
  )
  
  observeEvent(cohort_pheno(), {
    choices <- cohort_pheno()$ID
    updateSelectInput(session, "Subject", choices = choices)
  })
  
  output$allsteps_plot <- renderPlotly(
    plot10 <- plot_ly(indsubject(),x= ~indsubject()$Time, y= ~indsubject()$Steps, mode='line')
    %>% layout(title = paste("Step count of",input$Subject, "over Time"), xaxis=list(title ="Adjust the range slider to zoom into the the time period of interest"), yaxis=list(title = "Step Count")) %>% rangeslider()
    #layout(xaxis=list(title = paste("Step count of",input$Subject, "over Time")), xaxis=list(title ="Date"), yaxis=list(title = "Step Count")) %>% rangeslider()
  )
  output$allhearts_plot <- renderPlotly(
    plot11 <- plot_ly(indsubject(),x= ~indsubject()$Time, y= ~indsubject()$BPM, mode='line')
    %>% layout(title = paste("Heart rate of",input$Subject, "over Time"), xaxis=list(title ="Adjust the range slider to zoom into the the time period of interest"), yaxis=list(title = "Heart Rate (BPM)")) %>% rangeslider()
  )
  
  
  output$mytable1 <- DT::renderDataTable({
    DT::datatable(cohort_pheno()[, c(1,24,5:8,13:17), drop = FALSE])
  })
  
  ###########    
  
  percent_na_hr <- reactive({
    cohort() %>% group_by(ID) %>% summarise(per_na = sum(is.na(BPM))*100/n())
  })
  percent_na_step <- reactive({
    cohort() %>% group_by(ID) %>% summarise(per_na_step = sum(is.na(Steps))*100/n())
  })
  prop_zero_step <- reactive({
    cohort() %>% group_by(ID) %>% summarise(per_zero_step = sum(Steps == 0, na.rm = T)*100/n())
  })
  prop_zero_hr <- reactive({
    cohort() %>% group_by(ID) %>% summarise(per_zero_hr = sum(BPM == 0, na.rm = T)*100/n())
  })
  
  output$percent_NA_HR <- renderPlotly(
    plot_ly(percent_na_hr(), x = ~(percent_na_hr()$per_na), type = 'histogram', marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1))) %>% layout(xaxis=list(title = "Percentage (%) of Missingness (NA) in heart rate data", range = c(0,100)),yaxis=list(title = "Number of Participants"))
  )
  
  output$percent_NA_step <- renderPlotly(
    plot_ly(percent_na_step()) %>% add_trace(x = ~names(table(percent_na_step()$per_na_step)),
                                             y = ~(as.numeric(table(percent_na_step()$per_na_step))), type = 'bar', width=0.15) %>% layout(xaxis=list(title = "Percentage (%) of missingness in Step data"),yaxis=list(title = "Number of Participants"))
  )
  
  output$prop_zero_step <- renderPlotly(
    plot_ly(prop_zero_step(), x = ~(prop_zero_step()$per_zero_step), type = 'histogram', marker = list(colors = colors,line = list(color = '#FFFFFF', width = 1))) %>% layout(xaxis=list(title = "Percentage (%) of zero values in Step data", range = c(0,100)),yaxis=list(title = "Number of Participants"))
  )
  
  output$prop_zero_hr <- renderPlotly(
    plot_ly(prop_zero_hr()) %>% add_trace(x = ~names(table(prop_zero_hr()$per_zero_hr)),
                                          y = ~(as.numeric(table(prop_zero_hr()$per_zero_hr))), type = 'bar', width=0.15) %>% layout(xaxis=list(title = "Percentage (%) of zero values in heart rate data"),yaxis=list(title = "Number of Participants"))
  )
  
  ###############  
  ind_na_hr <- reactive({
    indsubject() %>% select(ID, Time, BPM) %>% group_by(date = lubridate::date(Time)) %>% summarise(prop_na = sum(is.na(BPM))/n())
  })
  
  ind_na_step <- reactive({
    indsubject() %>% select(ID, Time, Steps) %>% group_by(date = lubridate::date(Time)) %>% summarise(prop_na = sum(is.na(Steps))/n())
  })
  
  ind_zero_hr <- reactive({
    indsubject() %>% select(ID, Time, BPM) %>% group_by(date = lubridate::date(Time)) %>% summarise(prop_zero = sum(BPM == 0, na.rm = T)/n())
  })
  
  ind_zero_step <- reactive({
    indsubject() %>% select(ID, Time, Steps) %>% group_by(date = lubridate::date(Time)) %>% summarise(prop_zero = sum(Steps == 0, na.rm = T)/n())
  })
  
  
  output$ind_na_HR <- renderPlot({
    ggplot(ind_na_hr(),aes(x= date, y= prop_na)) + geom_bar(stat = 'identity') + xlab("Date") + ylab("Proportion of NA") + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  })
  
  output$ind_na_Step <- renderPlot({
    ggplot(ind_na_step(),aes(x= date, y= prop_na)) + geom_bar(stat = 'identity') + xlab("Date") + ylab("Proportion of NA") +  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  })
  
  output$ind_zero_HR <- renderPlot({
    ggplot(ind_zero_hr(),aes(x= date, y= prop_zero)) + geom_bar(stat = 'identity') + xlab("Date") + ylab("Proportion of 0's") + scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  })
  
  output$ind_zero_Step <- renderPlot({
    ggplot(ind_zero_step(),aes(x= date, y= prop_zero)) + geom_bar(stat = 'identity') + xlab("Date") + ylab("Proportion of 0's") +  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))
  })
  
  output$ind_na_step_missing <- renderPlot({
    ggplot(indsubjectMissing(),aes(x=date,y=hour2)) + ggtitle(paste("Active/Inactive status of",input$Subject, "over Time"))+
      geom_tile(aes(fill=as.factor(stepyn), width=0.7, height=0.7), size=0.5) + xlab("Date") + ylab("Hour") +
      theme(legend.title = element_blank(), plot.title = element_text(size = 15, face = "bold"), text = element_text(size = 15))  + scale_fill_manual(values=c('#56B4E9', '#E69F00'))
  })
  
  ##################  
  output$inactivity <- renderUI({
    
    
    if (input$valid_day == "Number of hours with Step data"){
      list (selectInput(inputId = "inactivity_threshold",
                        label = tags$h4(tags$strong("Select the threshold to determine inactivity vs. non-wear"), tags$p(tags$h5("(When there are consecutive zero values, it's hard to distinguish inactivity vs. non-wear. This threshold is used to distinguish the two. For example, if we choose the threshold as 60 minutes, consecutive zero values more than an hour would mean non-wear.)"))), 
                        choices = paste(unlist(sapply(c(10, 15, 20, 30, 60, 120), no_rem)), "minutes"),
                        selected = paste(unlist(sapply(c(10, 15, 20, 30, 60, 120), no_rem)), "minutes")[length(unlist(sapply(c(10, 15, 20, 30, 60, 120), no_rem)))]), 
            sliderInput(inputId = "valid_day_threshold", 
                        label = tags$h4(tags$strong("Select threshold for minimum wear hours needed for a valid day."), tags$p(tags$h5("(Example: A day is valid if there is at least 8 hours of data (if device was worn for at least 8 hours).)"))),  
                        min = 0, 
                        max = 24, 
                        value =8,width = 600))
      
    } else if (input$valid_day == "Number of hours with Heart Rate data"){
      sliderInput(inputId = "valid_day_threshold", 
                  label = tags$h4(tags$strong("Select threshold for minimum wear hour needed for a valid day."), tags$p(tags$h5("(Example: A day is valid if there is at least 8 hours of data (if device was worn for at least 8 hours).)"))),  
                  min = 0, 
                  max = 24, 
                  value =8,width = 600)
    } else {
      numericInput(inputId = "step", tags$h4(tags$strong("Step Count Threshold")), value = 500,width=200)
    }
  })
  
  output$text1 <- renderText({
    if (input$valid_day == "Number of hours with Step data"){
      Out = paste("Note: If a person did not move for more than", input$inactivity_threshold," minutes, then we assume that the person was not wearing the device. Also, a day is valid if wear time per day is greater than ", input$valid_day_threshold," hours.")
    }
    if (input$valid_day == "Number of hours with Heart Rate data"){
      Out = paste("Note: A day is valid if wear time per day is greater than ", input$valid_day_threshold," hours")
    }
    if (input$valid_day == "Step Count"){
      Out = paste("Note: A day is valid if step count is greater than ", input$step," steps")
    }
    Out
    
  })
  
  output$text2 <- renderText({
    paste("You defined a subject with complete data to have", input$num_day_per_weekday, " valid days in weekday and ", input$num_day_per_weekend, " valid days in weekend.")
  }) 
  
  output$text3 <- renderText({
    paste("You defined a subject with complete data to have", input$num_month, " consecutive valid months.")
  })
  
  output$text4 <- renderText({
    # if (input$even == "Yes"){
    #   paste("You defined a subject with complete data to have", input$num_month, " valid months evenly distributed over ", input$duration_month, " months.")
    # } else if (input$even == "No"){
    paste("You defined a subject with complete data to have", input$num_month, " valid months") #distributed over ", input$duration_month, " months.")
    #}
    
  })
  
  output$text5 <- renderText({
    paste('Please define completeness in the Completeness tab to see the summary of your cohort that fits your use.')
  })
  
  output$textindv <- renderText({
    
    paste("This participant is a ",indsubjectpheno()$Gender," ",indsubjectpheno()$Ethnicity,". Participant is ",indsubjectpheno()$Age," years old and wears ", indsubjectpheno()$device,". Data is available from", min(lubridate::date(indsubject()$Time))," to",  max(lubridate::date(indsubject()$Time)),". Participant's BMI is ", indsubjectpheno()$BMI, ". Participant's systolic blood pressure is ", indsubjectpheno()$SBP, "mmHg and diastolic blood pressure is ", indsubjectpheno()$DBP, "mmHg. Participant's total cholesterol is ", indsubjectpheno()$TotalChol, "mmol/L, HDL is ", indsubjectpheno()$HDL, "mmol/L, and LDL is ", indsubjectpheno()$LDL, "mmol/L.")
    
    
  })
  
  
  output$comp_consec_day <- renderText({
    paste("A subject has complete data if they have ", input$consec_day, " days.")
  })
  
  
  #defining a valid day
  # find_valid_day <- reactive({
  #   input$go
  #   isolate({
  #   if(input$valid_day == "Step Count"){
  #     cohort() %>% group_by(ID, date = lubridate::date(Time)) %>% summarise(sum_step = sum(Steps, na.rm = TRUE)) %>% filter(sum_step >= input$step)
  #   } else if (input$valid_day == "Wear Hour (Step)"){
  #     cohort() %>% group_by(ID, date = lubridate::date(Time)) %>% mutate(inact = unlist(lapply(1:length(rle(Steps)$length), function(i) rep(rle(Steps)$length[i], rle(Steps)$length[i])))* unit) %>% mutate(wearstat = ifelse(Steps == 0 & inact > input$inactivity_threshold, 0, 1)) %>% summarise(wear_hr = sum(wearstat)*unit/60) %>% filter(wear_hr >= input$valid_day_threshold) 
  #   } else if (input$valid_day == "Wear Hour (Heart Rate)"){
  #     cohort() %>% group_by(ID, date = lubridate::date(Time)) %>% filter(!is.na(BPM)) %>% summarise(wear_hr = n() * unit/60) %>% filter(wear_hr >= input$valid_day_threshold)
  #   }
  #   })
  # })
  # 
  # defining a valid day
  find_valid_day <- metaReactive2({
    input$go
    
    
    isolate({
      if(input$valid_day == "Step Count"){
        metaExpr(bindToReturn = TRUE,{cohort() %>% group_by(ID, date = lubridate::date(Time)) %>% summarise(sum_step = sum(Steps, na.rm = TRUE)) %>% filter(sum_step >= !!input$step)})
      } else if (input$valid_day == "Number of hours with Step data"){
        metaExpr(bindToReturn = TRUE,{cohort() %>% group_by(ID, date = lubridate::date(Time)) %>% mutate(inact = unlist(lapply(1:length(rle(Steps)$length), function(i) rep(rle(Steps)$length[i], rle(Steps)$length[i])))* unit) %>% mutate(wearstat = ifelse(Steps == 0 & inact > !!input$inactivity_threshold, 0, 1)) %>% summarise(wear_hr = sum(wearstat)*unit/60) %>% filter(wear_hr >= !!input$valid_day_threshold)}) 
      } else if (input$valid_day == "Number of hours with Heart Rate data"){
        metaExpr(bindToReturn = TRUE,{cohort() %>% group_by(ID, date = lubridate::date(Time)) %>% filter(!is.na(BPM)) %>% summarise(wear_hr = n() * unit/60) %>% filter(wear_hr >= !!input$valid_day_threshold)})
      }
    })
  })
  
  
  # comp_cohort <- reactive({
  #   input$go
  #   isolate({
  #   if (input$choice1 == "No" & input$choice2 == "No"){
  #     find_valid_day() %>% filter(n() >= input$comp_day) %>% distinct(ID)
  #   } else if (input$choice1 == "Yes" & input$choice2 == "No"){
  #     find_valid_day() %>% summarise(max.consecutive = max_consec(date)) %>% filter(max.consecutive >= input$consec_day) %>% distinct(ID)
  #   } else if (input$choice1 == "No" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "No"){
  #     find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% filter(n() >= input$num_day_per_month) %>% distinct(ID, month, year) %>%  group_by(ID) %>% summarise(num_mth = n()) %>% filter(num_mth >= input$num_month) %>% select(ID)
  #   } else if (input$choice1 == "No" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "Yes"){
  #     find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% filter(n() >= input$num_day_per_month) %>% distinct(ID, month, year) %>%  group_by(ID) %>% summarise(max.consec = max_consec_month(year, month)) %>% filter(max.consec >= input$num_month) %>% select(ID)
  #   } else if (input$choice1 == "No" & input$choice2 == "Yes" & input$choice3 == "compare weekday vs. weekend"){
  #     find_valid_day() %>% mutate(day_of_week = wday(date)) %>% mutate(wkend = ifelse(day_of_week == 7|day_of_week ==1,1, 0), wkday = ifelse(day_of_week != 7 & day_of_week != 1, 1, 0)) %>% group_by(ID) %>% summarise(n_wkend = sum(wkend), n_wkday = sum(wkday)) %>% filter(n_wkend >= input$num_day_per_weekend & n_wkday >= input$num_day_per_weekday) %>% select(ID)
  #   } else if (input$choice1 == "Yes" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "No"){
  #     find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% summarise(max.consecutive = max_consec(date)) %>% filter(max.consecutive >= input$num_day_per_month) %>% group_by(ID) %>% summarise(num_mth = n()) %>% filter(num_mth >= input$num_month) %>% select(ID)
  #   } else if (input$choice1 == "Yes" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "Yes"){
  #     find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% summarise(max.consec.day = max_consec(date)) %>% filter(max.consec.day >= input$num_day_per_month) %>% group_by(ID) %>% arrange(ID, year) %>% summarise(max.consec.mth = max_consec_month(year, month)) %>% filter(max.consec.mth >= input$num_month) %>% select(ID)
  #   } else if (input$choice1 == "Yes" & input$choice2 == "Yes" & input$choice3 == "compare weekday vs. weekend"){
  #     find_valid_day() %>% mutate(day_of_week = wday(date)) %>% mutate(wkend = ifelse(day_of_week == 7|day_of_week ==1,1, 0), wkday = ifelse(day_of_week != 7 & day_of_week != 1, 1, 0)) %>% group_by(ID) %>% summarise(max_consec_wkend = max_consec(date[wkend == 1]), max_consec_wkday = max_consec(date[wkday == 1])) %>% filter(max_consec_wkend >= input$num_day_per_weekend & max_consec_wkday >= input$num_day_per_weekday) %>% select(ID)
  #   }
  #   })
  # })
  # 
  
  #comp_cohort <- reactiveValues()
  #n_final_cohort_hr <- reactiveValues()
  #n_final_cohort_step <- reactiveValues()
  
  
  comp_cohort <- metaReactive2({
    input$go
    isolate({
      if (input$choice1 == "No" & input$choice2 == "No"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% filter(n() >= input$comp_day) %>% distinct(ID)})
      } else if (input$choice1 == "Yes" & input$choice2 == "No"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% summarise(max.consecutive = max_consec(date)) %>% filter(max.consecutive >= !!input$consec_day) %>% distinct(ID)})
      } else if (input$choice1 == "No" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "No"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% filter(n() >= !!input$num_day_per_month) %>% distinct(ID, month, year) %>%  group_by(ID) %>% summarise(num_mth = n()) %>% filter(num_mth >= !!input$num_month) %>% select(ID)})
      } else if (input$choice1 == "No" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "Yes"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% filter(n() >= !!input$num_day_per_month) %>% distinct(ID, month, year) %>%  group_by(ID) %>% summarise(max.consec = max_consec_month(year, month)) %>% filter(max.consec >= !!input$num_month) %>% select(ID)})
      } else if (input$choice1 == "No" & input$choice2 == "Yes" & input$choice3 == "compare weekday vs. weekend"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% mutate(day_of_week = wday(date)) %>% mutate(wkend = ifelse(day_of_week == 7|day_of_week ==1,1, 0), wkday = ifelse(day_of_week != 7 & day_of_week != 1, 1, 0)) %>% group_by(ID) %>% summarise(n_wkend = sum(wkend), n_wkday = sum(wkday)) %>% filter(n_wkend >= !!input$num_day_per_weekend & n_wkday >= !!input$num_day_per_weekday) %>% select(ID)})
      } else if (input$choice1 == "Yes" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "No"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% summarise(max.consecutive = max_consec(date)) %>% filter(max.consecutive >= !!input$num_day_per_month) %>% group_by(ID) %>% summarise(num_mth = n()) %>% filter(num_mth >= !!input$num_month) %>% select(ID)})
      } else if (input$choice1 == "Yes" & input$choice2 == "Yes" & input$choice3 == "analyze trends/change over months" & input$choice3_1 == "Yes"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% mutate(month = month(date), year = year(date)) %>% group_by(ID, month, year) %>% summarise(max.consec.day = max_consec(date)) %>% filter(max.consec.day >= !!input$num_day_per_month) %>% group_by(ID) %>% arrange(ID, year) %>% summarise(max.consec.mth = max_consec_month(year, month)) %>% filter(max.consec.mth >= !!input$num_month) %>% select(ID)})
      } else if (input$choice1 == "Yes" & input$choice2 == "Yes" & input$choice3 == "compare weekday vs. weekend"){
        metaExpr(bindToReturn = TRUE,{find_valid_day() %>% mutate(day_of_week = wday(date)) %>% mutate(wkend = ifelse(day_of_week == 7|day_of_week ==1,1, 0), wkday = ifelse(day_of_week != 7 & day_of_week != 1, 1, 0)) %>% group_by(ID) %>% summarise(max_consec_wkend = max_consec(date[wkend == 1]), max_consec_wkday = max_consec(date[wkday == 1])) %>% filter(max_consec_wkend >= !!input$num_day_per_weekend & max_consec_wkday >= !!input$num_day_per_weekday) %>% select(ID)})
      }
    })
  })
  
  #output$Cohort_Initial <- renderValueBox({valueBox(nsubjs(), tags$strong("Total number of participants in your cohort"))})
  
  n_final_cohort_hr <- reactive({
    input$go
    isolate({
      cohort() %>% filter(ID %in% comp_cohort()$ID) %>% filter(!is.na(BPM)) %>% distinct(ID) %>% nrow()
    })
  })
  
  n_final_cohort_step <- reactive({
    input$go
    isolate({
      cohort() %>% filter(ID %in% comp_cohort()$ID) %>% filter(!is.na(Steps)) %>% distinct(ID) %>% nrow()
    })
  })
  
  observeEvent(input$go, {
    updateTabsetPanel(session = session, inputId = "Tabs", selected = "Summary of Cohort with Complete Data")
  })
  
  observeEvent(input$go, {
    
    output$Cohort_Final <- renderValueBox({valueBox(nrow(comp_cohort()), tags$strong("Number of participants with complete data"))})
    output$Cohort_Final_HR <- renderValueBox({valueBox(n_final_cohort_hr(), tags$strong("Number of participants with heart rate data"), color = 'orange')})
    output$Cohort_Final_Step <- renderValueBox({valueBox(n_final_cohort_step(), tags$strong("Number of participants with step data"), color = 'green')})
    
  })
  
#  observeEvent(input$go,{
#    output$Cohort_Final_spinner <- renderUI({withSpinner(valueBoxOutput("Cohort_Final"))})
#  })
  
  comp_cohort_pheno <- reactive({
    input$go
    isolate({
      cohort_pheno() %>% filter(ID %in% comp_cohort()$ID) 
    })
  })
  
  comp_stepdatperday <- reactive({
    input$go
    isolate({
      cohort() %>% filter(ID %in% comp_cohort()$ID) %>% filter(Steps>0) %>% group_by(date) %>% summarise(count = n_distinct(ID))
    })
  })
  
  comp_heartdatperday <- reactive({
    input$go
    isolate({
      mdatpheno2 %>% filter(ID %in% comp_cohort()$ID) %>% filter(BPM>0) %>% group_by(date) %>% summarise(count = n_distinct(ID))
    })
  })
  
  observeEvent(input$go, {
    progressSweetAlert(
      session = session, id = "myprogress",
      title = "Loading",
      display_pct = TRUE, value = 0
    )
    for (i in seq_len(50)) {
      Sys.sleep(0.1)
      updateProgressBar(
        session = session,
        id = "myprogress",
        value = i*2
      )
    }
    closeSweetAlert(session = session)
    #      sendSweetAlert(
    #        session = session,
    #        title =" Cohort summary completed !",
    #        type = "success"
    #      )
    
   
    
    # Draw plot of cohort summary    
    output$comp_gen_plot <- renderPlotly({
      
      plot_ly(comp_cohort_pheno(), labels= ~(comp_cohort_pheno()$Gender), type = 'pie', textposition = 'inside',
              textinfo = 'label+percent', 
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1))
              )
      
      })
    
    
    output$comp_eth_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno(),labels= ~(comp_cohort_pheno()$Ethnicity), type = 'pie', textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1))
      )) 
    
    output$comp_age_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno(),x= ~(comp_cohort_pheno()$Age), type = 'histogram', marker = list(colors = colors,
                                                                                                   line = list(color = '#FFFFFF', width = 1))
      ) %>% layout(xaxis=list(title = "Age"),yaxis=list(title = "Number of participants"))) 
    
    output$comp_bmi_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno(),x= ~(comp_cohort_pheno()$BMI), type = 'histogram', marker = list(colors = colors,
                                                                                                   line = list(color = '#FFFFFF', width = 1))
      ) %>% layout(xaxis=list(title = "BMI"),yaxis=list(title = "Number of participants"))) 
    
    output$comp_bp_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno()) 
      %>% add_histogram(x= ~(comp_cohort_pheno()$SBP),name = 'Systolic BP')
      %>% add_histogram(x= ~(comp_cohort_pheno()$DBP),name = 'Diastolic BP')
      %>% layout(legend = list(font = list(size = 8),orientation = "h", xanchor = "center", x = 0.5,y=100),xaxis=list(title = "Blood Pressure (mmHg)"),yaxis=list(title = "Number of participants")))
    
    
    output$comp_chol_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno())
      %>% add_histogram(x= ~(comp_cohort_pheno()$TotalChol),name='Total Cholesterol') 
      %>% add_histogram(x = ~(comp_cohort_pheno()$LDL),name='LDL') %>% add_histogram(x = ~(comp_cohort_pheno()$HDL),name='HDL') 
      %>% layout(legend = list(font = list(size = 8),orientation = "h", xanchor = "center", x = 0.5,y=100),xaxis=list(title = "Cholesterol (mmol/L)"),yaxis=list(title = "Number of participants"))) 
    
    
    output$comp_duration_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno(),x= ~(comp_cohort_pheno()$followup),type = 'histogram', marker = list(colors = colors,
                                                                                                       line = list(color = '#FFFFFF', width = 1))
      ) %>% layout(xaxis=list(title = "Number of days"),yaxis=list(title = "Number of participants")))
    
    
    output$comp_device_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno(),labels= ~(comp_cohort_pheno()$device),type = 'pie', textposition = 'inside',
              textinfo = 'label+percent',
              insidetextfont = list(color = '#FFFFFF'),
              marker = list(colors = colors,
                            line = list(color = '#FFFFFF', width = 1))
      ))  
    
    output$comp_step_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno(),x= ~(comp_cohort_pheno()$ave_step),type = 'histogram', marker = list(colors = colors,
                                                                                                       line = list(color = '#FFFFFF', width = 1))
      ) %>% layout(xaxis=list(title = "Average Step Count"),yaxis=list(title = "Number of participants")))
    
    output$comp_hr_plot <- renderPlotly(
      plot_ly(comp_cohort_pheno(),x= ~(comp_cohort_pheno()$ave_hr),type = 'histogram', marker = list(colors = colors,
                                                                                                     line = list(color = '#FFFFFF', width = 1))
      ) %>% layout(xaxis=list(title = "Average Heart Rate (BPM)"),yaxis=list(title = "Number of participants")))
    
    output$comp_stepday_plot <- renderPlotly(
      plot_ly(comp_stepdatperday(),x= ~date, y= ~count, mode='line')
      %>% layout(xaxis=list(title = "Participants with recorded step count per day"), xaxis=list(title ="Date"), yaxis=list(title = "Number of participants"))
    )
    
    output$comp_heartday_plot <- renderPlotly(
      plot_ly(comp_heartdatperday(),x= ~date, y= ~count, mode='line')
      %>% layout(xaxis=list(title = "Participants with recorded heart rate per day"), xaxis=list(title ="Date"), yaxis=list(title = "Number of participants"))
    )
    
  })
  
  
  # Show Code 
  observeEvent(input$show_r_code, {
    showModal(modalDialog(
      title = "R Code",
      tags$pre(
        id = "r_code",
        expandChain(
          library_code,
          data_code,
          func_code,
          cohort(),
          find_valid_day(),
          comp_cohort()
        ) %>% formatCode() %>% paste(collapse = "\n")
      ),
      footer = tagList(
        actionButton("copyRCode", "Copy to Clipboard", `data-clipboard-target` = "#r_code"),
        modalButton("Dismiss")
      ),
      size = "l",
      easyClose = TRUE
    ))
  })
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)
