# Load Packages --------------------------------------------------

# library(shiny)
# library(tidyverse)
# library(haven)
pacman::p_load(shiny, tidyverse, haven, ggplot2)

# Reading and Preparing Data -------------------------------------

world<- read_dta("world_internal.dta")
world<- filter(world, year>2005)
world1<- select(world, cname, iso3, year, 
                "Total Revenue"=rev,
                "Tax Revenue"=tax, 
                "Income Tax Revenue"= inc, 
                "Individual Income Tax Revenue"=indv,
                "Corporate Income Tax Revenue"=corp,
                "Payroll Tax Revenue"=pay, 
                "Property Tax Revenue"=propr,
                "Goods and Services Tax Revenue"=goods, 
                "General Goods and Services Tax Revenue"=genr,
                "Value Added Taxes Revenue"=vat, 
                "Excises Revenue"=excises,
                "Trade Tax Revenue"=trade, 
                "Grants"= grants, 
                "Social Contributions"= soc)
world1<- pivot_longer(world1,cols="Total Revenue":"Social Contributions",names_to="indicators", values_to="values")
world_data <- world1
rm(world1)
rm(world)

# Define UI for application that draws a plot ------------------------

ui <- fluidPage(
  
  titlePanel("World Revenue Longitudinal Data Visualization App"),
  
  #select countty
  selectInput(
    "select_cname",
    "Select Countries: ",
    choices = unique(world_data$cname),
    multiple = T),
  
  #select indicators
  selectInput(
    "select_indicators", 
    "Select Indicator:",
    choices = unique(world_data$indicators),
    multiple = F),
  
  #select year
  sliderInput(
    "select_year",
    "Select Year: ",
    min = min(world_data$year),
    max = max(world_data$year),
    value=c(2013,2016),
    sep=""
  ),
  
  
  
  #plot
  plotOutput("worldplot")
)


# Define server logic required to draw a plot -----------------------------


server <- function(input, output, session) {
  
  #filter country
  world1<-reactive({
    req(input$select_cname, input$select_indicators, input$select_year)
    foo <- subset(world_data, cname %in% input$select_cname & indicators %in% input$select_indicators & year >= input$select_year[1] & year <= input$select_year[2])
    return(foo)
  })
  
  
  #plot
  output$worldplot <- renderPlot({
    
    req(world1())
    
    
    ggplot(world1(), aes(x=year, y=values,color = cname))+geom_line()
  })
  
}

# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
