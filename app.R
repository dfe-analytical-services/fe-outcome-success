# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("DT")
# install.packages("dplyr")
# install.packages("markdown")
# install.packages("reshape2")
# install.packages("stringr")

# load packages
library(shiny)
library(ggplot2)
library(DT)
library(dplyr)
library(markdown)
library(reshape2)
library(stringr)
# read in data - obsm is the data displayed in the tables, summ is the data used to create the graphs on the summary page, 
# levs is factor levels of learner category, thresholds is thresholds of quintiles
obsm<-read.csv("obsm.csv",
               check.names=F,stringsAsFactors=F)
summ<-read.csv("summary.csv",
               check.names=F,stringsAsFactors=F)
levs<-read.csv("learner_cat_levels.csv",
               check.names=F,stringsAsFactors=F)
thresholds<-read.csv("thresholds.csv",
                     check.names=F,stringsAsFactors=F)

# combining ukprn and provider name into one variable, then removing those variables and replacing with new variable
# relevelling factor levels of learner category
obsm$Provider<-paste(obsm$UKPRN,"-",obsm$`Provider Name`)
obsm<-obsm %>% select(c(16,3:15))
obsm$`Learner Category`<-factor(obsm$`Learner Category`,levels=levs[,1])
summ$Provider<-paste(summ$UKPRN,"-",summ$`Provider Name`)
summ<-summ %>% select(c(17,3:16)) %>% filter(`Learner type`=="All learners")

# adding percent sign to SPDR values that are numbers instead of x or . 
obsm$`Sustained Positive Destination Rate 13/14 BL`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 13/14 BL`)))==F,
                                                          paste0(obsm$`Sustained Positive Destination Rate 13/14 BL`,'%'),
                                                          as.character(obsm$`Sustained Positive Destination Rate 13/14 BL`))
obsm$`Sustained Positive Destination Rate 13/14 All`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 13/14 All`)))==F,
                                                          paste0(obsm$`Sustained Positive Destination Rate 13/14 All`,'%'),
                                                          as.character(obsm$`Sustained Positive Destination Rate 13/14 All`))
obsm$`Sustained Positive Destination Rate 14/15 BL`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 14/15 BL`)))==F,
                                                          paste0(obsm$`Sustained Positive Destination Rate 14/15 BL`,'%'),
                                                          as.character(obsm$`Sustained Positive Destination Rate 14/15 BL`))
obsm$`Sustained Positive Destination Rate 14/15 All`<-ifelse(is.na(as.numeric(as.character(obsm$`Sustained Positive Destination Rate 14/15 All`)))==F,
                                                          paste0(obsm$`Sustained Positive Destination Rate 14/15 All`,'%'),
                                                          as.character(obsm$`Sustained Positive Destination Rate 14/15 All`))

# user interface
ui <-
  # comes in handy later when changing tabs
  tabsetPanel(id="tabs",
  # first tab            
  tabPanel("Summary Page",
           mainPanel(
           
           # start of description
           fluidPage(
             # title, h3 defines size
             h3(strong(titlePanel("Further Education Outcome Based Success Measures - Interactive Tool"))),
             br("This tool is aimed at enabling users to further understand provider outcomes, split by learner categories."), 
             br("Learner categories are factors such as a learner's characteristics and the type of training completed. 
                They are designed to help users understand how providers' outcomes compare when measured against similar provision. 
                The interactive table shows the cohort size, sustained positive destination rate, and the quintile for each learner category, 
                and allows users to understand how the headline measure in the summary chart has been constructed."), 
             hr(),
             strong("Definitions"),
             br(),
             br("Completions - the number of completers in each learner category. 
                Cohorts are only included in the measure where there are ten or more learners per category, 
                therefore totals may not match to provider numbers published elsewhere in the main report."),
             br("Sustained Positive Destination Rate - see main report for all Outcome Based Success Measure definitions and headline results"),
             br("Quintile - identified by comparing the Sustained Positive Destination Rate against the distributions listed in reference thresholds tab:"),
             tags$div(tags$ul(
               tags$li("Bottom quintile - outcomes are below the 20th percentile;"),
               tags$li("Second quintile - outcomes are between the 20th and 40th percentile;"),
               tags$li("Third quintile - outcomes are between the 40th and 60th percentile;"),
               tags$li("Fourth quintile - outcomes are between the 60th and 80th percentile;"),
               tags$li("Top quintile - outcomes are above the 80th percentile."))),
             hr(),
             strong("Notes"),
             br(),
             br("1) The data relate to learners completing all Apprenticeships, all Traineeships, and Adult (19+) FE and Skills learners that completed an ESFA funded aim 
                in the relevant academic year. Please see the", a("publication page", href="https://www.gov.uk/government/collections/statistics-outcome-based-success-measures", target="_blank"), 
                " for full technical details, guidance on use, and other measures that have been developed."),
             br("2) For all data, totals are rounded to the nearest 10 learners and percentages are rounded and reported to the nearest percentage point. 
                Totals may not sum due to rounding."),
             br("3) For data where there were zero completing learners the cell is marked with '.' to show no data available. 
                Outcomes rates for a destination are suppressed and marked with 'x' where:"),
             tags$div(tags$ul(
               tags$li("there were fewer than 11 completers"),
               tags$li("the rate is based on 1 or 2 learners"),
               tags$li("the rate rounds to zero")))
             ,
             br("4) Cohorts are only included in the Learner Category measure where there are ten or more learners per category, therefore totals may not match to provider 
                numbers published elsewhere in the main report. For more details please see the technical note."),
             br("5) Where a learner completes more than one aim in the academic year, outcomes are reported against the aim that was completed at the highest level."),
             br("6) The Learner Category 'Other provision below Full Level 2' does not include ESOL, English & Maths, Traineeships and Learners with Learning Difficulty 
                and/or Disability (LLDD)."),
             hr(),
             strong("Contact"),
             br(),
             br("Email us at ", a("FE.OUTCOMESDATA@education.gov.uk", href="mailto:FE.OUTCOMESDATA@education.gov.uk", target="_blank"), 
                " or write to us at FE Outcomes Data, Department for Education, Sanctuary Buildings, Great Smith Street, London SW1P 3BT")
             # end of description
             
           )
           ),
           # create sidebar 
           sidebarPanel(
            # create input to select provider
            selectInput("provider1",
                                  "Provider",
                                  c("",unique(as.character(obsm$Provider)))),
            # create output area for plot
            plotOutput("plot"),
            br(),
            # create text output
            uiOutput("con"),
            br(),
            # button to press to move to next tab
            actionButton(inputId = "submit",label="Click here to see the underlying data in more detail")
           )
           ),
  # create table tab
  tabPanel("Interactive Tables",
  fluidPage(
  titlePanel("Outcome Based Success Measures"),
  fluidRow(
    # column(x,...) defines width of following arguments, numbers of all columns must add to 12 or less
    # column(2,
    #        selectInput("provider",
    #                    "Provider",
    #                    c("",unique(as.character(obsm$Provider))))
    # ),
    column(2,
           selectInput("learner_cat",
                       "Learner Category",
                       c("All",
                         levs[,1]))
    ),
    column(2,
           selectInput("year",
                       "Academic Year",
                       c("All",
                         "14/15","13/14"))
    ),
    column(2,
           selectInput("learner_type",
                       "Learner Type",
                       c("","All","Benefit Learner"))
    ),
    column(3,
           br("Key:"),
           br("BL - Benefit Learners"),
           br("All - All learners excl. benefit learners")
           ),
    column(1,
           downloadButton("downloadData", "Download")
    )
  ),

  # output for table
  fluidRow(
    dataTableOutput("table")
  )
)
),
# create thresholds tab
  tabPanel("Thresholds",
         fluidPage(
           titlePanel("Thresholds"),
           # output for table
           fluidRow(
             dataTableOutput("thresholds")
           )
         )
  )
)

# server
server <- function(input,output,session){
  # input$submit is the 'Go' button so when it is clicked, the tab changes
  observeEvent(input$submit,{
    updateTabsetPanel(session=session,inputId="tabs",selected="Interactive Tables")
  })
  # plot
  output$plot<-renderPlot({
    plotdata<-summ %>%
      # filter for provider
      filter(Provider==input$provider1) %>%
      # select relevant columns
      select(c(1,5:9,11:15)) %>% 
      # melt puts data into format we want
      melt("Provider")
      # name of title, changes as provider changes
      title_lab<-unique(plotdata$Provider)
      # first 5 rows are 13/14, second 5 are 14/15
      plotdata[1:5,1]<-"13/14"
      plotdata[6:10,1]<-"14/15"
      # removes 13/14 and 14/15 from values to make plot nicer
      plotdata$variable<-substr(plotdata$variable,1,str_length(plotdata$variable)-6)
      # relevel factor levels
      plotdata$variable<-factor(plotdata$variable,
                                levels=rev(c("Bottom Quintile","Second Quintile","Third Quintile","Fourth Quintile","Top Quintile")))
      # ggplot of data
      ggplot(plotdata,aes(as.character(Provider),as.numeric(value)*100,fill=variable)) + 
      geom_bar(stat="identity") + 
      coord_flip() + 
      labs(y="Percentage",x="Academic Year",title=title_lab) + 
      scale_fill_manual(values=blues9[3:7]) +
      guides(fill=guide_legend(title=""))
  })
  output$con <- renderUI({
    conditionalPanel("input.provider1 != ''",
    textOutput("text"),
    textOutput("text1")
    )
  })
  # text showing completions
  output$text <- renderText({
    text<-summ %>% 
      filter(Provider==input$provider1) %>%
      select(`Completions 14/15`)
    paste("Completions 14/15:",text)
  })
  output$text1 <- renderText({
    text<-summ %>% 
      filter(Provider==input$provider1) %>%
      select(`Completions 13/14`)
    paste("Completions 13/14:",text)
  })
  # subsetting data for table, mostly self-explanatory
  dataset <- reactive({
    data <- obsm
    if (input$provider1 != "") {
      data <- data[data$Provider == input$provider1,]
    }
    # else if (input$provider == "" && input$provider1 != "") {
    #   data <- data[data$Provider == input$provider1,]
    # }
    if (input$learner_cat != "All") {
      data <- data[data$Learner == input$learner_cat,]
    }
    # selecting columns that contain certain values in the name
    if (input$year != "All") {
      if (input$year == "13/14") {
        data <- select(data,c(1:2,names(data)[grepl("13/14",names(data))]))
      }
      if (input$year == "14/15") {
        data <- select(data,c(1:2,names(data)[grepl("14/15",names(data))]))
      }
    }
    if (input$learner_type != "") {
      if (input$learner_type == "Benefit Learner") {
        data <- select(data,c(1:2,names(data)[grepl("BL",names(data))]))
      }
      if (input$learner_type == "All") {
        data <- select(data,c(1:2,names(data)[grepl("All",names(data))]))
      }
    }
    data
  })
  # table creation
  output$table <- renderDataTable(datatable({
    # note the () after dataset - doesn't work without this as dataset() is reactive
    dataset()
    # these options are for the table - self-explanatory
  }, options = list(searching = FALSE,pageLength=100),rownames=F
  ) %>%   
  # colour coding for each quintile
  formatStyle(columns=as.character(names(dataset())[grepl("Quin",names(dataset()))]), 
              background=styleEqual(c("top quintile","fourth quintile","third quintile",
                                      "second quintile","bottom quintile"), 
                                    blues9[c(3,4,5,6,7)]))
  )
  # download button
  output$downloadData <- downloadHandler(
    filename = "obsm_download.csv",
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  output$thresholds <- renderDataTable(datatable({
    thresholds
  }, options = list(pageLength=100),rownames=F))
  
}
# defines app
shinyApp(ui = ui, server = server)