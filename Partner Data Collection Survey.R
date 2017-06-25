library(shiny)
library(rdrop2)
library(lubridate)
library(tidyverse)
library(shinyjs)
library(shinythemes)
library(DT)
library(digest)


#Dropbox Token 

token <- readRDS("droptoken.rds")

outputDir <- "responses2"

#Load the data

partner <- read.csv("Data/Data.csv", header = TRUE)

#Format parnter data

partner$Date.ended <- as.character(partner$Date.ended)
partner$Date.ended <- as.Date(partner$Date.ended, format="%d/%m/%Y")

# Define the fields we want to save from the form
fields <- c("password", "date","filter", "attended", "engaged")

# Shiny app with 3 fields that the user can submit data for
shinyApp(
  ui = bootstrapPage(theme=shinytheme("flatly"),
                     navbarPage("Partner Data Portal",
                                tabPanel("Data entry",
                                         sidebarLayout(   
                                           sidebarPanel(
                                             p("Welcome to the Partner Zone Data Portal!"),
                                             p(""),
                                             p("This is where you can enter attendance and engagement data about a session."),
                                             p("Please enter your password and the session date to reveal students' names."),
                                             passwordInput("password", "Please enter your password", ""),
                                             dateInput("date", "What was the date of the session?"),
                                             uiOutput("filter"),
                                             p("Need help? Have any questions?"),
                                             p("Please contact Bridget Suthersan, Senior Data Analyst, on xxx@gmail.com"),
                                             actionButton("submit", "Submit Data"),
                                             p(""),
                                             p(""),
                                             useShinyjs(),
                                             shinyjs::hidden(
                                               div(id = "thankyou", "Thank you, your data has been received!"))),
                                           mainPanel(
                                             img(src='Logo.jpg', align = "right", width=250, height=250),
                                             div(id="myapp",
                                                 uiOutput("attended"),
                                                 uiOutput("engaged"))))),
                                tabPanel("Help and FAQ",
                                         sidebarLayout(   
                                           sidebarPanel(
                                             p("Use this part of the data portal for help or to browse frequently asked questions"),
                                             p(""),
                                             p(strong("How does the data portal work?")),
                                             p(""),
                                             p(strong("I've made a mistake - what do I do?")),
                                             p(""),
                                             p(strong("I've forgotten my password - what do I do?"))),
                                           mainPanel( 
                                             img(src='Logo.jpg', align = "right", width=250, height=250),
                                             p(strong("How does the data portal work?")),
                                             p(""),
                                             p("The data portal is where you can enter attendance and engagement data about our students. We use this portal to collect data for all our students from partners."),
                                             p(strong("I've made a mistake in my data entry! What do I do?")),
                                             p(""),
                                             p("That's okay! Just re-enter your data, and drop Bridget a line at xxx@gmail.com, letting her know what to look out for."),
                                             p(""),
                                             p(strong("I've forgotten my password - what do I do?")),
                                             p(""),
                                             p("You can request a password reset by emailing Bridget Suthersan on xxx@gmail.com"))
                                         ))
                     )),
  
  server = function(input, output, session) {
    
  
    #Filter list of students by partner and date into reactive dataset  
    filterdata <- reactive({
      if(input$password=="buckingham") { 
        partner %>%
          filter(Partner=="Buckingham Palace",
                 Date.ended<=input$date)
      }
      else{
        if(input$password=="kensington") {
          partner %>%
            filter(Partner=="Kensington Palace", 
                   (is.na(Date.ended) | Date.ended<=input$date),
                   Filter %in% input$filter1)
        }
      }
    })
    
    
    #Filt
    output$attended <- renderUI({
      if(input$password=="buckingham" | input$password=="kensington") {
        new2 <- filterdata()
        checkboxGroupInput("attended", "Please tick if the student attended the session", choices=new2$Name)
      }
      else{NULL}
    })
    
    #Partners will only see students who they have ticked as attending for the engaged list
    output$engaged <- renderUI({
      if(input$password=="buckingham" | input$password=="kensington") {
        new3 <- new()
        checkboxGroupInput("engaged", "Please tick if the student was engaged in the session", choices=c(input$attended))
      }
      else{NULL}
    })
  
    #Kensington has two sessions - Monday sessions and Wednesday sessions   
    output$filter <- renderUI({
      if(input$password=="kensington") {
        selectInput("filter1", label="Please select the session", choices=c("Monday session","Wednesday session"))
      }
      else{NULL}
    })
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data <- sapply(fields, function(x) input[[x]])
      data
    })
    
    # When the Submit button is clicked, save the form data, and give a thank you message
    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::show(id="thankyou")
    })
    
    
    saveData <- function(data) {
      data <- t(data)
      # Create a unique file name
      fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
      write.csv(data, fileName, row.names = TRUE, quote = TRUE)
      token <- readRDS("droptoken.rds")
      drop_acc(dtoken = token)    
      drop_upload(fileName, dest = outputDir, dtoken=token)
    }
  }
)


