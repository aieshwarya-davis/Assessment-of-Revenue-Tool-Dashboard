# # Load Packages --------------------------------------------------
# 
# library(shiny)
# library(tidyverse)
# library(haven)
# library(thematic)
# library (scales)
# library(bslib)
# library(lubridate)
# library(plotly)
# library(shinythemes)
pacman::p_load(shiny, shinythemes, thematic, scales, bootstrap, bslib, tidyverse, tidyr, haven, lubridate, ggplot2, plotly)

# Reading and Preparing Data -------------------------------------


library(haven)
# load("~/art environment.RData")
# data cleaning for revenue trend chart
load("~/Downloads/art environment.RData")


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

##Data cleaning for revenue composition chart


world_data2<- select(world_data2, cname, iso3, year,
                     "Individual Income Tax Revenue"=indv,
                     "Corporate Income Tax Revenue"=corp,
                     "Payroll Tax Revenue"=pay,
                     "Property Tax Revenue"=propr,
                     "Goods and Services Tax Revenue"=goods,
                     "Trade Tax Revenue"=trade,
                     "Social Contributions"= soc,
                     "Other"=other)

world_data2<- pivot_longer(world_data2,cols="Individual Income Tax Revenue":"Other",names_to="indicators", values_to="values")


#Data Cleaning for tax and per capita scatter plot 


tax_percapita<- merge(world, weo, by=c("cname","year"))
tax_percapita<- filter(tax_percapita, year>2000)
tax_percapita <- select(tax_percapita, cname, iso3, year,
                        "Total Revenue"=rev,
                        "Tax Revenue"=tax,
                        ngdpdpc)

tax_percapita <- pivot_longer(tax_percapita, cols="Total Revenue":"Tax Revenue",names_to="indicators", values_to="values")
tax_percapita$ngdpdpc <- as.numeric(as.character(tax_percapita$ngdpdpc)) 

#Data cleaning for Rates data

rates<- filter(rates, year>2000)
rates<- select(rates, cname, iso3, year,
               "Personal Income Tax Rate"=pit_top_comb,
               "Corporate Income Tax Rate"=cit_top_comb,
               "Value Added Taxes Rate"=vat_comb,
               "Indirect Taxes Rate"=indirect_tax_comb)
rates_data<- pivot_longer(rates,cols="Personal Income Tax Rate":"Indirect Taxes Rate",names_to="indicators", values_to="values")
vatprod <- filter(vatprod, year>2000)

#Data cleaning for CIT Productivity

citprod<- pivot_longer(productivity, cols="1980":"2020", names_to="year", values_to="values")
citprod<- filter(citprod, indicators== "CIT Productivity")
citprod<- mutate(citprod, values= values*100)
rates_prod <- select(rates, cname, year, cit_top_comb= "Corporate Income Tax Rate")
citprod <- merge(citprod, rates_prod, by = c("cname", "year"))
citprod <- filter(citprod, year>2000)


#Data cleaning for VAT productivity

vatprod<- pivot_longer(productivity, cols="1980":"2020", names_to="year", values_to="values")
vatprod<- filter(vatprod, indicators== "VAT Productivity")
vatprod<- mutate(vatprod, values= values*100)
rates_prod <- select(rates, cname, year,vat_comb="Value Added Taxes Rate")
vatprod <- merge(vatprod, rates_prod, by = c("cname", "year"))
vatprod <- filter(vatprod, year>2000)


rm(rates_prod)


#rm(world1)
# rm(world)

# Define UI for application that draws a plot ------------------------

# library(thematic)
# library(bootstrap)
# library(bslib)
# library(plotly)
# library(shinythemes)

thematic_shiny()

ui <- fluidPage(
  
  theme= bs_theme(version=4, bootswatch = "cerulean", base_font = font_google("Work Sans")),
  
  titlePanel(HTML("<h1><center><font size=6><font color=#3F97D4><strong> Assessment of Revenue Tool </font></font></center></h1></strong>") ),
  HTML("<p><font size=3>This app is designed to provide a cross-country comparison snapshot of rates and revenue indicators across 20 years for over 120 countries. By allowing users to benchmark composition and efficiency of tax revenues to a group of country and country group comparators, ART provides information for tax policy assessment and helps identify potential areas for revenue mobilization and to increase revenue collection efficiency. The World Revenue Longitudinal database (WoRLD) used in this tool, contains revenue information curated from four sources: the IMF’s Government Finance Statistics (GFS) and World Economic Outlook (WEO) and the OECD’s Revenue Statistics and Revenue Statistics. The International Tax Rates Database (ITRD) on the other hand, is predominantly sourced from IBFD, EY, KPMG tax guides.<br>
       For more information regarding this tool, please contact the author: Aieshwarya Davis (aieshwaryadavis@gmail.com</font></p>"), 
  
  ##Revenue Trends Tab UI
  tabsetPanel(type="tabs", tabPanel("Revenue Trends",
                                    fluidRow( 
                                      
                                      column(3,
                                             
                                             #select countty
                                             selectInput(
                                               "select_cname",
                                               "Select Countries: ",
                                               choices = unique(world_data$cname),
                                               selected = c("Austria","Australia","India","United Kingdom"),
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
                                               value=c(2010,2018),
                                               sep="" ) 
                                      ) 
                                    ),
                                    
                                    #plot
                                    plotlyOutput("worldplot")
  ),
  
  #Rates Trends Tab UI
  
  tabPanel("Rates  Trends",
           fluidRow( 
             
             column(3,
                    
                    #select countty
                    selectInput(
                      "select_cname1",
                      "Select Countries: ",
                      choices = unique(rates_data$cname),
                      selected = c("Austria","Australia","India","United Kingdom"),
                      multiple = T)),
             
             
             column(4, 
                    #select indicators
                    selectInput(
                      "select_indicators1", 
                      "Select Indicator:",
                      choices = unique(rates_data$indicators),
                      multiple = F)),
             
             
             column(5,
                    #select year
                    sliderInput(
                      "select_year1",
                      "Select Year: ",
                      min = min(rates_data$year),
                      max = max(rates_data$year),
                      value=c(2010,2018),
                      sep="" ) 
             ) 
           ),
           
           #plot
           plotlyOutput("ratesplot")
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
                      selected = "Australia",
                      multiple = F)),
             
             
             
             column(5,
                    #select year
                    sliderInput(
                      "select_yr",
                      "Select Year: ",
                      min = min(world_data2$year),
                      max = max(world_data2$year),
                      value=c(2010,2018),
                      sep="" ) 
             ) 
           ),
           
           #plot
           plotlyOutput("compositionplot")
  ),
  
  
  ##CIT Productivity Chart
  tabPanel("CIT productivity",
           fluidRow( 
             
             column(3,
                    #select countty
                    selectInput(
                      "select_countries3",
                      "Select Countries: ",
                      choices = unique(citprod$cname),
                      selected = c("Austria","Australia","India","United Kingdom"),
                      multiple = T)),
             
             
             
             column(5,
                    #select year
                    selectInput(
                      "select_yr3",
                      "Select Year: ",
                      choices = unique(citprod$year),
                      multiple = F ) 
             ) 
           ),
           
           #plot
           plotlyOutput("citprodplot")
  ),
  
  ##VAT Productivity Chart
  tabPanel("VAT productivity",
           fluidRow( 
             
             column(3,
                    #select countty
                    selectInput(
                      "select_countries4",
                      "Select Countries: ",
                      choices = unique(vatprod$cname),
                      selected = c("Austria","Australia","India","United Kingdom"),
                      multiple = T)),
             
             
             
             column(5,
                    #select year
                    selectInput(
                      "select_yr4",
                      "Select Year: ",
                      choices = unique(vatprod$year),
                      multiple = F ) 
             ) 
           ),
           
           #plot
           plotlyOutput("vatprodplot")
  ),
  
  tabPanel("Revenue and per capita GDP",
           fluidRow(
             
             column(3,
                    
                    #select countty
                    selectInput(
                      "select_cnames",
                      "Select Countries: ",
                      choices = unique(tax_percapita$cname),
                      selected = c("Austria","Australia","India","United Kingdom","Germany","France","Iceland","China","Japan","Italy","Indonesia"),
                      multiple = T)),
             
             
             column(4,
                    #select indicators
                    selectInput(
                      "select_indics",
                      "Select Indicator:",
                      choices = unique(tax_percapita$indicators),
                      multiple = F)),
             
             
             column(5,
                    #select year
                    selectInput(
                      "select_yrs",
                      "Select Year: ",
                      choices = unique(tax_percapita$year),
                      multiple = F))
           ),
           
           #plot
           plotlyOutput("percapitaplot")
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
            type= 'scatter', mode= 'lines+markers') %>%
      layout(hovermode= 'closest', xaxis = list(
        dtick = 1, 
        tick0 = 2000, 
        tickmode = "linear",
        # title = "Years",
        showline= T, linewidth=2, linecolor='black'
      ),
      yaxis = list(
        title= "% of GDP", 
        ticksuffix = "%",
        zerolinecolor = 'black',
        hoverformat= '.2f', 
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
  
  #filter for rates trends tab 
  
  rates1<-reactive({
    req(input$select_cname1, input$select_indicators1, input$select_year1)
    foo <- subset(rates_data, cname %in% input$select_cname1 & indicators %in% input$select_indicators1 & year >= input$select_year1[1] & year <= input$select_year1[2])
    return(foo)
  })
  
  
  #plot
  
  output$ratesplot <- renderPlotly({
    
    req(rates1())
    
    plot_ly(rates1(), x= rates1()$year, y= rates1()$values, color= rates1()$cname, 
            type= 'scatter', mode= 'lines+markers') %>%
      layout(hovermode= 'closest', xaxis = list(
        dtick = 1, 
        tick0 = 2000, 
        tickmode = "linear",
        # title = "Years",
        showline= T, linewidth=2, linecolor='black'
      ),
      yaxis = list(
        title= "Rates %", 
        ticksuffix = "%",
        zerolinecolor = 'black',
        hoverformat= '.2f', 
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
        # title = "Years",
        showline= T, linewidth=2, linecolor='black'
      ),
      yaxis = list(
        title= "% of Total Revenue", 
        ticksuffix = "%",
        zerolinecolor = 'black',
        hoverformat= '.2f', 
        showline= T, linewidth=2, linecolor='black'
      ),
      legend = list(title=list(text='Indicators')
      ), 
      barmode = 'stack'
      
      )   
    
  })
  
  ##filter for CIT productivity chart
  citprod2<-reactive({
    req(input$select_countries3, input$select_yr3)
    foo <- subset(citprod, cname %in% input$select_countries3 & year %in% input$select_yr3)
    return(foo)
  })
  
  
  #plot
  
  output$citprodplot <- renderPlotly({
    
    req(citprod2())
    
    plot_ly(citprod2(), x= citprod2()$cname, y= citprod2()$values, color= citprod2()$cname, 
            type= 'bar')%>%
      add_trace(citprod2() , x = citprod2()$cname, y = citprod2()$cit_top_comb, type = 'scatter',  mode = 'markers', color= citprod2()$year , size = 2) %>%
      layout(xaxis = list(
        title = "Countries",
        showline= T, linewidth=2, linecolor='black'
      ),
      yaxis = list(
        title= "Productivity", 
        ticksuffix = "%",
        zerolinecolor = 'black',
        hoverformat= '.2f', 
        showline= T, linewidth=2, linecolor='black'
      ), showlegend = FALSE
      )
    
    
    
  })
  
  
  ##filter for VAT productivity chart
  vatprod2<-reactive({
    req(input$select_countries4, input$select_yr4)
    foo <- subset(vatprod, cname %in% input$select_countries4 & year %in% input$select_yr4)
    return(foo)
  })
  
  
  #plot
  
  output$vatprodplot <- renderPlotly({
    
    req(vatprod2())
    
    plot_ly(vatprod2(), x= vatprod2()$cname, y= vatprod2()$values, color= vatprod2()$cname, 
            type= 'bar')%>%
      add_trace(vatprod2() , x = vatprod2()$cname, y = vatprod2()$vat_comb, type = 'scatter',  mode = 'markers', size = 2, color= vatprod2()$year) %>%
      layout(xaxis = list(
        title = "Countries",
        showline= T, linewidth=2, linecolor='black'
      ),
      yaxis = list(
        title= "Productivity", 
        ticksuffix = "%",
        zerolinecolor = 'black',
        hoverformat= '.2f', 
        showline= T, linewidth=2, linecolor='black'
      ), showlegend = FALSE
      )
    
    
  })
  
  # #filter for percapita tab
  tax_percapita_chart<-reactive({
    req(input$select_cnames, input$select_indics, input$select_yrs)
    foo <- subset(tax_percapita, cname %in% input$select_cnames & indicators %in% input$select_indics & year %in% input$select_yrs)
    return(foo)
  })
  
  
  
  #plot
  
  output$percapitaplot <- renderPlotly({
    
    req(tax_percapita_chart())
    
    
    plot_ly(tax_percapita_chart(), x= tax_percapita_chart()$ngdpdpc, y= tax_percapita_chart()$values, color= tax_percapita_chart()$cname,
            type= 'scatter', mode= 'markers', size = 2)%>%
      layout(hovermode= 'closest', xaxis = list(
        title = "Per Capita GDP (Current US$)",
        dtick = 10000,
        hoverformat= '.2f',
        showline= T, linewidth=2, linecolor='black'
      ),
      yaxis = list(
        title= "% of GDP",
        ticksuffix = "%",
        zerolinecolor = 'black',
        hoverformat= '.2f',
        showline= F, linewidth=2, linecolor='black'
      ),
      
      legend = list(title=list(text='Countries')
      )
      
      )
    
    # ggplot(tax_percapita_chart(), aes(x=ngdpdpc, y=values,color = cname)) +
    #   geom_point(size=2)+ geom_smooth(method=lm)+
    #   theme_minimal() +
    #   scale_x_continuous(name= "Per Capita GDP", breaks = world_data$year) +
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
    
  })
  
}

# Run the application -----------------------------------------------------

shinyApp(ui = ui, server = server)
