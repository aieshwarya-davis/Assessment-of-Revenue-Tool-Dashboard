# # Load Packages --------------------------------------------------
# 
# # library(shiny)
# # library(tidyverse)
# # library(haven)
pacman::p_load(shiny, bslib, tidyverse, haven, lubridate, ggplot2)

# # Reading and Preparing Data -------------------------------------
# 
world<- read_dta("world_internal.dta")
world<- filter(world, year>2000)


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

my_theme <- bs_theme(version=4, bootswatch = "cerulean", base_font = font_google("Work Sans"))


ui <- fluidPage(
  
  theme= my_theme,
  
  titlePanel(HTML("<h1><center><font size=6> World Revenue Longitudinal Data Visualization App </font></center></h1>") ),
 p("This app is designed to provide a cross-country comparison snapshot of 13 revenue indicators across 20 years for over 120 countries. These indicators are from four sources: the IMF’s Government Finance Statistics (GFS) and World Economic Outlook (WEO) and the OECD’s Revenue Statistics and Revenue Statistics."), 
 
  fluidRow( 
   
     column(3,
          
  #select countty
  selectInput(
    "select_cname",
    "Select Countries: ",
    choices = unique(world_data$cname),
    multiple = T)),
      
  
  column(4, 
  #select indicators
  selectInput(
    "select_indicators", 
    "Select Indicator:",
    choices = unique(world_data$indicators),
    multiple = F)),
  
  
  column(5,
  #select year
  sliderInput(
    "select_year",
    "Select Year: ",
    min = min(world_data$year),
    max = max(world_data$year),
    value=c(2013,2016),
    sep="" ) 
  ) 
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
    
    
    ggplot(world1(), aes(x=year, y=values,color = cname)) +
      geom_line(linetype="solid", size=1) + 
      geom_point(size=2)+ 
      theme_bw() + 
      scale_x_continuous(name= "Years", breaks = world_data$year) + 
      scale_y_continuous(name= "% of GDP", labels = percent_format(scale=1, accuracy=1)) + 
      labs(color = "Countries") +   
      theme( panel.border = element_rect(colour = "black", fill=NA),
               legend.background = element_blank(),
               legend.box.background = element_rect(colour = "black"),
               legend.title.align=0.5,
               legend.title = element_text(face = "bold"),
               axis.text.x = element_text(size=10),
               axis.text.y = element_text(size=10),
               axis.line = element_line(colour = 'black', size = 1.5))
     
  })
  
}

# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
