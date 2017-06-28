library(shiny)
library(shinythemes)
library(markdown)

popData <- read.csv("Population_data_country.csv")
totalpop <- popData[ , 1:4]
under15 <- popData[ , c( 1, 5:7)]
over60 <- popData[ , c( 1, 8:10)]


ui <- fluidPage(
         
         theme = shinytheme("yeti"),
         titlePanel("Population Data By country: 2013-2015"),
         sidebarLayout(
           sidebarPanel(
             radioButtons("year", "Select a year:",
                                        c("2013" = "thirteen",
                                          "2014" = "fourteen",
                                          "2015" = "fifteen"))
           ),
           
           mainPanel(
              tabsetPanel(type = "tabs", 
                          tabPanel("Total Population", plotOutput("plotTotal"), verbatimTextOutput("summaryTotal")), 
                          tabPanel("Population Under 15", plotOutput("plotUnder"), verbatimTextOutput("summaryUnder")), 
                          tabPanel("Population Over 60", plotOutput("plotOver"), verbatimTextOutput("summaryOver")))
        )
    )
)

server <- function( input, output ) {
  
  data <- reactive({
    year <- switch(input$year,
                   thirteen = "2013",
                   fourteen = "2014",
                   fifteen = "2015")
  })
  
  output$plotTotal <- renderPlot({
    
    if (data() == "2013") {
          hist( totalpop$Population..in.thousands..total.2013,
          main = "Countries' Total Population: 2013", col = "turquoise3", 
          ylim = c(0, 200), xlab = "Total Population in Thousands")}
    
    if (data() == "2014") {
          hist( totalpop$Population..in.thousands..total.2014,
          main = "Countries' Total Population: 2014", col = "turquoise3", 
          ylim = c(0, 200), xlab = "Total Population in Thousands")}
    
    if (data() == "2015") {
          hist( totalpop$Population..in.thousands..total.2015,
          main = "Countries' Total Population: 2015", col = "turquoise3",
          ylim = c(0, 200), xlab = "Total Population in Thousands")}
    
  })
  
  output$plotUnder <- renderPlot({
    
    if (data() == "2013") {
      hist( under15$Population.proportion.under.15.....2013,
            main = "Countries' Population Under 15: 2013", col = "plum2", 
            ylim = c(0, 40), xlab = "Proportion of Population Under 15")}
    
    if (data() == "2014") {
      hist( under15$Population.proportion.under.15.....2014,
            main = "Countries' Population Under 15: 2014", col = "plum2", 
            ylim = c(0, 40), xlab = "Proportion of Population Under 15")}
    
    if (data() == "2015") {
      hist( under15$Population.proportion.under.15.....2015,
            main = "Countries' Population Under 15: 2015", col = "plum2", 
            ylim = c(0, 40), xlab = "Proportion of Population Under 15")}
    
  })
  
  output$plotOver <- renderPlot({
    
    if (data() == "2013") {
      hist( over60$Population.proportion.over.60.....2013,
            main = "Countries' Population Over 60: 2013", col = "salmon1",
            ylim = c(0, 80), xlab = "Proportion of Population Over 60")}
    
    if (data() == "2014") {
      hist( over60$Population.proportion.over.60.....2014,
            main = "Countries' Population Over 60: 2014", col = "salmon1",
            ylim = c(0, 80), xlab = "Proportion of Population Over 60")}
    
    if (data() == "2015") {
      hist( over60$Population.proportion.over.60.....2015,
            main = "Countries Population Over 60: 2015", col = "salmon1",
            ylim = c(0, 80), xlab = "Proportion of Population Over 60")}
    
  })
  
  output$summaryTotal <- renderPrint( 
    
    if (data() == "2013") 
      summary(totalpop$Population..in.thousands..total.2013),
    
    if (data() == "2014") 
      summary(totalpop$Population..in.thousands..total.2014),
    
    if (data() == "2015") 
      summary(totalpop$Population..in.thousands..total.2015)
    
  )
  
  output$summaryUnder <- renderPrint(
    
    if (data() == "2013") 
      summary(under15$Population.proportion.under.15.....2013),
    
    if (data() == "2014") 
      summary(under15$Population.proportion.under.15.....2014),
    
    if (data() == "2015") 
      summary(under15$Population.proportion.under.15.....2015)
    
  )
  
  ouput$summaryOver <- renderPrint(
    
    if (data() == "2013") 
      summary(over60$Population.proportion.over.60.....2013),
    
    if (data() == "2014") 
      summary(over60$Population.proportion.over.60.....2014),
    
    if (data() == "2015") 
      summary(over60$Population.proportion.over.60.....2015)
    
  )
  
}

shinyApp( ui = ui, server = server )