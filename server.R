source("R/codefile.R")

# server
shinyServer(function(input,output,session){
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
  
})