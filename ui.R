source("R/codefile.R")

# user interface
shinyUI(
  fluidPage(
    useShinyjs(),
    tags$head(tags$style(
      HTML('
           #sidebar {
           background-color: #ffffff;
           border-color:ffffff;
           border-width:0;
           outline: 0;
           }'))),
  navbarPage(
    "Further Education Outcome Based Success Measures (Pilot)",
    theme = "shiny.css",
    id = "navbar", 
  # first tab            
  tabPanel("Summary Page",
           sidebarLayout(
           sidebarPanel(id = "sidebar",
           # start of description
             # title, h3 defines size
              wellPanel(style = "background: ",
             h2("Further Education Outcome Based Success Measures - Interactive Tool (Pilot)"),
             h3("Summary"),
             p("This tool is aimed at enabling users to further understand provider outcomes, split by learner categories."), 
             p("Learner categories are factors such as a learner's characteristics and the type of training completed. 
                They are designed to help users understand how providers' outcomes compare when measured against similar provision. 
                The interactive table shows the cohort size, sustained positive destination rate, and the quintile for each learner category, 
                and allows users to understand how the headline measure in the summary chart has been constructed."), 
             br(),
             h3("Definitions"),
             p("Completions - the number of completers in each learner category. 
                Cohorts are only included in the measure where there are ten or more learners per category, 
                therefore totals may not match to provider numbers published elsewhere in the main report."),
             p("Sustained Positive Destination Rate - see main report for all Outcome Based Success Measure definitions and headline results"),
             p("Quintile - identified by comparing the Sustained Positive Destination Rate against the distributions listed in reference thresholds tab:"),
             tags$div(tags$ul(
               tags$li("Top quintile - outcomes are above the 80th percentile."),
               tags$li("Fourth quintile - outcomes are between the 60th and 80th percentile;"),
               tags$li("Third quintile - outcomes are between the 40th and 60th percentile;"),
               tags$li("Second quintile - outcomes are between the 20th and 40th percentile;") ,
               tags$li("Bottom quintile - outcomes are below the 20th percentile;"))),
             br(),
             h3("Notes"),
             p("1) The data relate to learners completing all Apprenticeships, all Traineeships, and Adult (19+) FE and Skills learners that completed an ESFA funded aim 
                in the relevant academic year. Please see the", a("publication page", href="https://www.gov.uk/government/collections/statistics-outcome-based-success-measures", target="_blank"), 
                " for full technical details, guidance on use, and other measures that have been developed."),
             p("2) For all data, totals are rounded to the nearest 10 learners and percentages are rounded and reported to the nearest percentage point. 
                Totals may not sum due to rounding."),
             p("3) For data where there were zero completing learners the cell is marked with '.' to show no data available. 
                Outcomes rates for a destination are suppressed and marked with 'x' where:"),
             tags$div(tags$ul(
               tags$li("there were fewer than 11 completers"),
               tags$li("the rate is based on 1 or 2 learners"),
               tags$li("the rate rounds to zero")))
             ,
             p("4) Cohorts are only included in the Learner Category measure where there are ten or more learners per category, therefore totals may not match to provider 
                numbers published elsewhere in the main report. For more details please see the technical note."),
             p("5) Where a learner completes more than one aim in the academic year, outcomes are reported against the aim that was completed at the highest level."),
             p("6) The Learner Category 'Other provision below Full Level 2' does not include ESOL, English & Maths, Traineeships and Learners with Learning Difficulty 
                and/or Disability (LLDD)."),
             br(),
             h3("Contact"),
             p(),
             p("Email us at ", a("FE.OUTCOMESDATA@education.gov.uk", href="mailto:FE.OUTCOMESDATA@education.gov.uk", target="_blank"), 
                " or write to us at FE Outcomes Data, Department for Education, Sanctuary Buildings, Great Smith Street, London SW1P 3BT"))
             # end of description
             
           , width = 7),
           # create sidebar 
           sidebarPanel(id = "sidebar",
            # create input to select provider
            selectInput("provider1",
                                  "Search Provider Number (UKPRN) or Provider Name",
                                  c("",unique(as.character(obsm$Provider)))),
            # create output area for plot
            plotOutput("plot"),
            br(),
            # create text output
            uiOutput("con"),
            br(),
            # button to press to move to next tab
            actionButton(inputId = "submit",label="Click here to see the underlying data in more detail")
            , width = 5)
           )
           ),
  # create table tab
  tabPanel("Interactive Tables",
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
                         "15/16", "14/15","13/14"))
    ),
    column(2,
           selectInput("learner_type",
                       "Learner Type",
                       c("","All","Benefit Learner"))
    ),
    column(3,
           style='border: 2px solid black',
           br("Key: Learner Type"),
           br("BL - Benefit Learners"),
           br("All - All learners excl. benefit learners"),
           br()
           ),
    column(1,
           downloadButton("downloadData", "Download")
    ),

  # output for table
  fluidRow(
    column(12,dataTableOutput("table"))
  )
)
),
# create thresholds tab
  tabPanel("Thresholds",
           titlePanel("Thresholds"),
           # output for table
           fluidRow(
             dataTableOutput("thresholds")
           )
  )
)
)
)
