source("R/codefile.R")

# server
shinyServer(function(input,output,session){
  # Hide tabPanels
  hideTab("navbar", target = "Interactive Tables")
  hideTab("navbar", target = "Thresholds")
  # When a school is selected, the 'see more data' button is enabled
  observe({ 
    shinyjs::toggleState("submit", 
      !is.null(input$provider1) && input$provider1 != "")  })
  # input$submit is the 'Go' button so when it is clicked, the tab changes
  observeEvent(input$submit,{
    updateTabsetPanel(session=session,inputId="navbar",selected="Interactive Tables")
  }) 
  #Reveal the futher tabs when the user clicks through to see the data in more detail
  observeEvent(input$submit,{
    showTab("navbar", target = "Interactive Tables")
    showTab("navbar", target = "Thresholds")
  }) 
  # plot
  output$plot<-renderPlotly({
    if(!is.na(input$provider1) & input$provider1 != ""){
      plotdata<-summ %>%
        # filter for provider
        filter(Provider== input$provider1) %>%
        # select relevant columns
        select(c(1, 4:8, 10:14, 16:20)) %>% 
        # melt puts data into format we want
        melt("Provider")
      # name of title, changes as provider changes
      title_lab<-unique(plotdata$Provider)
      # first 5 rows are 13/14, second 5 are 14/15
      plotdata[1:5,1]<-"13/14"
      plotdata[6:10,1]<-"14/15"
      plotdata[11:15,1]<-"15/16"
      # removes 13/14 and 14/15 from values to make plot nicer
      plotdata$variable<-substr(plotdata$variable,1,str_length(plotdata$variable)-6)
      # relevel factor levels
      plotdata$variable<-factor(plotdata$variable,
                                levels=rev(c("Bottom Quintile","Second Quintile","Third Quintile","Fourth Quintile","Top Quintile")))
      colnames(plotdata) <- c("Year", "Quintile", "value")
      
      
      plotdata <- plotdata %>% dcast(Year ~ Quintile) 
      
      plotdata <- plotdata %>% mutate(Year = as.factor(Year),
                                      `Top Quintile` = as.numeric(`Top Quintile`),
                                      `Fourth Quintile` = as.numeric(`Fourth Quintile`),
                                      `Third Quintile` = as.numeric(`Third Quintile`),
                                      `Second Quintile` = as.numeric(`Second Quintile`),
                                      `Bottom Quintile` = as.numeric(`Bottom Quintile`))
      
      plot_ly(plotdata, x = ~`Bottom Quintile`, y = ~Year, type = 'bar', orientation = 'h', name = 'Bottom quintile',
              marker = list(color = 'rgba(16, 79, 117, 0.6)',
                            line = list(color = 'rgba(16, 79, 117, 1)',
                                        width = 3) ) ) %>% 
        add_trace(x = ~`Second Quintile`, name = 'Second quintile',
                  marker = list(color = 'rgba(64, 114, 145, 0.6)', 
                                line = list(color = 'rgba(64, 114, 145, 1)', width = 3))) %>%
        add_trace(x = ~`Third Quintile`, name = 'Third quintile',
                  marker = list(color = 'rgba(112, 149, 172, 0.6)', 
                                line = list(color = 'rgba(112, 149, 172, 1)', width = 3))) %>%
        add_trace(x = ~`Fourth Quintile`, name = 'Fourth quintile',
                  marker = list(color = 'rgba(159, 185, 200, 0.6)', 
                                line = list(color = 'rgba(159, 185, 200, 1)', width = 3))) %>%
        add_trace(x = ~`Top Quintile`, name = 'Top quintile',
                  marker = list(color = 'rgba(207, 220, 227, 0.6)', 
                                line = list(color = 'rgba(207, 220, 227, 1)', width = 3))) %>%
        layout(barmode = 'stack', xaxis = list(title = "Percentage of students"), yaxis = list(title = "Year of study"))
    }
    
  })
  output$con <- renderUI({
    conditionalPanel("input.provider1 != ''",
                     textOutput("text"),
                     textOutput("text1"),
                     textOutput("text2")
    )
  })
  # text showing completions
  output$text <- renderText({
    text<-summ %>% 
      filter(Provider==input$provider1) %>%
      select(`Completions 15/16`)
    paste("Completions 15/16:", prettyNum(text, big.mark = ","))
  })
  output$text1 <- renderText({
    text<-summ %>% 
      filter(Provider==input$provider1) %>%
      select(`Completions 14/15`)
    paste("Completions 14/15:", prettyNum(text, big.mark = ","))
  })
  output$text2 <- renderText({
    text<-summ %>% 
      filter(Provider==input$provider1) %>%
      select(`Completions 13/14`)
    paste("Completions 13/14:", prettyNum(text, big.mark = ","))
  })
  # subsetting data for table, mostly self-explanatory
  dataset <- reactive({
    data <- obsm %>% 
      melt(id.vars = c("Provider", "Learner Category")) %>% 
      mutate(Measure = case_when(grepl("Completions", variable) ~ "Completions",
                                 grepl("Quintile", variable) ~ "Quintile",
                                 grepl("Sustained Positive Destination Rate", variable) ~ "Sustained Positive Destination Rate"),
             `Academic year` = case_when(grepl("13/14", variable) ~ "13/14",
                                         grepl("14/15", variable) ~ "14/15",
                                         grepl("15/16", variable) ~ "15/16"),
             `Benefit learner` = ifelse(grepl("BL", variable), "Benefit Learner", "All learners excl. benefit learners")) %>% 
      select(-variable) %>% 
      select(Provider, `Learner Category`, Measure, `Academic year`, `Benefit learner`, Value = value) %>% 
      dcast(Provider + `Learner Category` + `Academic year` + `Benefit learner` ~ Measure, value.var = "Value")
    
    if (input$provider1 != "") {
      outputdata <- data %>% filter(Provider == input$provider1)
    }
    # else if (input$provider == "" && input$provider1 != "") {
    #   data <- data[data$Provider == input$provider1,]
    # }
    if (input$learner_cat != "All") {
      outputdata <- outputdata %>% filter(`Learner Category` == input$learner_cat)
    }
    # selecting columns that contain certain values in the name
    if (input$year != "All") {
      outputdata <- outputdata %>% filter(`Academic year` == input$year)
    }
    if (input$learner_type != "") {
      if(input$learner_type != "All"){
        outputdata <- outputdata %>% filter(`Benefit learner` == input$learner_type)  
      }
      
    }
    outputdata
  })
  # table creation
  output$table <- renderDataTable(datatable({
    # note the () after dataset - doesn't work without this as dataset() is reactive
    dataset()
    # these options are for the table - self-explanatory
  }, extensions = 'FixedColumns',
  options = list(searching = FALSE,pageLength=999, dom = 't',
                    scrollX = TRUE, scrollY = "500px", fixedColumns = list(leftColumns = 2),
                    autoWidth=FALSE,
                    columnDefs = list(list(width = '200px', targets = c(0,1)) ) ), rownames=F
  ) %>%   
    # colour coding for each quintile
    formatStyle(columns=as.character(names(dataset())[grepl("Quin",names(dataset()))]), 
                background=styleEqual(c("top quintile","fourth quintile","third quintile",
                                        "second quintile","bottom quintile"), 
                                      blues9[c(3,4,5,6,7)]))
  %>% formatRound (grepl("Completions",names(dataset())), digits = 0, mark = ",")
  )
  # download button
  output$downloadData <- downloadHandler(
    filename = "obsm_download.csv",
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  output$thresholds <- renderDataTable(datatable({
    thresholds %>% 
      melt(id.vars = c("Learner Category", "Learner Type")) %>% 
      mutate(Year = case_when(grepl("13/14", variable) ~ "13/14",
                              grepl("14/15", variable) ~ "14/15",
                              grepl("15/16", variable) ~ "15/16"),
             variable = substr(variable, 0, nchar(as.character(variable))-6)) %>%
      select(`Learner Category`, `Learner Type`, Year, variable, value) %>%
      dcast(`Learner Category` + `Learner Type` + Year ~ variable)
  }, options = list(pageLength=100),rownames=F))
  
})
