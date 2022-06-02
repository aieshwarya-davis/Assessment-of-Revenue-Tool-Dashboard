#Title: World Revenue Dashboard
#Author: Aieshwarya Davis
#Date: 06/02/2022


# Load Packages --------------------------------------------------


pacman::p_load(shiny, tidyverse, haven, ggplot2)

# Reading and Preparing Data -------------------------------------

world<- read_dta("world_internal.dta")
world1<- filter(world, year>2005)
world1<- select(world1, cname, iso3, year, rev, tax, inc, indv, corp, pay, propr, goods, genr, vat, excises, trade, grants, soc)
world1<- pivot_longer(world1,cols=rev:soc,names_to="indicators", values_to="values")


# Define UI for application that draws a plot ------------------------


ui <- fluidPage(
  SelectInput(
    "select_cname",
    "Select Countries: ",
    choices = world1$cname %>% unique()),


  
  uiOutput("world"),
  plotOutput("worldplot")
)


# Define server logic required to draw a plot -----------------------------


server <- function(input, output, session) {
  
  world1<-reactive({
    foo <- subset(world1, cname == input$select_cname)
    return(foo)
  })
  
  output$indicators<-renderUI({
    selectizeInput("select_indicators","Select Indicators:",
                   choices = unique(world1()$indicators))
  })
  
  world2<-reactive({
    foo<-subset(world1(),indicators == input$select_indicators)
    return(foo)
    
  })
  
  output$worldplot <- renderPlot({
    
    y <- world2$values()
    
    ggplot(world2, aes(x=years, y=world2$values))+geom_line()
  })
  
}

# Run the application -----------------------------------------------------


shinyApp(ui = ui, server = server)

