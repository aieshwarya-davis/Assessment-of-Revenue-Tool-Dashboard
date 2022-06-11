# # Load Packages --------------------------------------------------
# 
# # library(shiny)
# # library(tidyverse)
# # library(haven)
pacman::p_load(shiny, thematic, scales, bslib, tidyverse, haven, lubridate, ggplot2, plotly)

# # Reading and Preparing Data -------------------------------------
# # 
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

world_data2 <- subset(world_data, indicators!="Total Revenue" & indicators!="Tax Revenue" & indicators!="Payroll Tax Revenue" &
              indicators!="Individual Income Tax Revenue" & indicators!="Goods and Services Tax Revenue" 
              & indicators!= "Value Added Taxes Revenue" & indicators!="Grants")
rm(world1)
rm(world)

# Define UI for application that draws a plot ------------------------

my_theme <- bs_theme(version=4, bootswatch = "cerulean", base_font = font_google("Work Sans"))
thematic_shiny()

ui <- fluidPage(
  
  theme= my_theme,
  
  titlePanel(HTML("<h1><center><font size=6><strong> World Revenue Longitudinal Data Visualization App </font></center></h1></strong>") ),
 HTML("<p><font size=2>This app is designed to provide a cross-country and compositional comparison snapshot of 13 revenue indicators across 20 years for over 120 countries. These indicators are from four sources: the IMF’s Government Finance Statistics (GFS) and World Economic Outlook (WEO) and the OECD’s Revenue Statistics and Revenue Statistics.</font></p>"), 
 
##Revenue Trends Tab UI
    tabsetPanel(type="tabs", tabPanel("Revenue Trends",
  fluidRow( 
   
     column(3,
          
  #select countty
  selectInput(
    "select_cname",
    "Select Countries: ",
    choices = unique(world_data$cname),
    selected = sort(world_data$cname)[1],
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
  plotlyOutput("worldplot")
),

##Composition of Revenue tab UI
  tabPanel("Composition of Revenue",
       fluidRow( 
                                    
          column(3,
               #select countty
               selectInput(
                 "select_countries",
                 "Select Countries: ",
                 choices = unique(world_data2$cname),
                 selected = sort(world_data2$cname)[1],
                 multiple = F)),
        
        
        
        column(5,
               #select year
               sliderInput(
                 "select_yr",
                 "Select Year: ",
                 min = min(world_data2$year),
                 max = max(world_data2$year),
                 value=c(2013,2016),
                 sep="" ) 
        ) 
      ),
      
      #plot
      plotlyOutput("compositionplot")
)
)
)


# Define server logic required to draw a plot -----------------------------


server <- function(input, output, session) {
  
  #filter for revenue trends tab
  world1<-reactive({
    req(input$select_cname, input$select_indicators, input$select_year)
    foo <- subset(world_data, cname %in% input$select_cname & indicators %in% input$select_indicators & year >= input$select_year[1] & year <= input$select_year[2])
    return(foo)
  })
  
  
  #plot
  
   output$worldplot <- renderPlotly({
    
    req(world1())
 
      plot_ly(world1(), x= world1()$year, y= world1()$values, color= world1()$cname, 
           type= 'scatter', mode= 'lines+markers')%>%
            layout(xaxis = list(
              dtick = 1, 
              tick0 = 2000, 
              tickmode = "linear",
              title = "Years",
              showline= T, linewidth=2, linecolor='black'
              ),
              yaxis = list(
                title= "% of GDP", 
                ticksuffix = "%",
                zerolinecolor = 'black',
                showline= T, linewidth=2, linecolor='black'
              ),
              legend = list(title=list(text='Countries')
              )
              )
    
      
      
    # 
    # ggplot(world1(), aes(x=year, y=values,color = cname)) +
    #   geom_line(linetype="solid", size=1) + 
    #   geom_point(size=2)+ 
    #   theme_minimal() + 
    #   scale_x_continuous(name= "Years", breaks = world_data$year) + 
    #   scale_y_continuous(name= "% of GDP", labels = percent_format(scale=1, accuracy=1)) + 
    #   labs(color = "Countries") +   
    #   theme( panel.border = element_rect(colour = "black", fill=NA),
    #            legend.background = element_blank(),
    #            legend.box.background = element_rect(colour = "black"),
    #            legend.title.align=0.5,
    #            legend.title = element_text(face = "bold"),
    #            axis.text.x = element_text(size=10),
    #            axis.text.y = element_text(size=10),
    #            axis.line = element_line(colour = 'black', size = 1.5))
    #  
  })
   
   ##filter for composition of revenue tab
   world2<-reactive({
     req(input$select_countries, input$select_yr)
     foo <- subset(world_data2, cname %in% input$select_countries & year >= input$select_yr[1] & year <= input$select_yr[2])
     return(foo)
   })
   
   
   #plot
   
   output$compositionplot <- renderPlotly({
     
     req(world2())
     
     plot_ly(world2(), x= world2()$year, y= world2()$values, color= world2()$indicators, 
             type= 'bar')%>%
       layout(xaxis = list(
         dtick = 1, 
         tick0 = 2000, 
         tickmode = "linear",
         title = "Years",
         showline= T, linewidth=2, linecolor='black'
       ),
       yaxis = list(
         title= "% of GDP", 
         ticksuffix = "%",
         zerolinecolor = 'black',
         showline= T, linewidth=2, linecolor='black'
       ),
       legend = list(title=list(text='Indicators')
       ), 
       barmode = 'stack'
       
       )   
   
   })
  
    }

# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
