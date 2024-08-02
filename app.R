# Load necessary libraries
library(shiny)
library(ggplot2)
library(maps)
library(dplyr)
library(WDI)
library(viridis)
library(countrycode)

# Function to load and prepare World Bank data
load_wb_data <- function(indicator, year, log_transform = FALSE) {
  data <- WDI(indicator = indicator, start = year, end = year) %>%
    mutate(country = countrycode(iso2c, origin = 'iso2c', destination = 'country.name')) %>%
    filter(!(iso2c %in% c("1A", "1W", "4E", "7E", "8S", "B8", "EU", "F1", "JG", "OE", "S1", "S2", "S3", "S4", "T2", "T3", "T4", "T5", "T6", "T7", "V1", "V2", "V3", "V4", "XC", "XD", "XE", "XF", "XG", "XH", "XI", "XJ", "XK", "XL", "XM", "XN", "XO", "XP", "XQ", "XT", "XU", "XY", "Z4", "Z7", "ZF", "ZG", "ZH", "ZI", "ZJ", "ZQ", "ZT"))) %>%
    filter(!(is.na(country))) %>% 
    filter(!(is.na(.data[[indicator]]))) %>%
    mutate(region = country)
  
  if (log_transform) {
    data <- data %>%
      mutate(across(all_of(indicator), log))
  }
  
  world_map <- map_data("world")
  world_map <- world_map %>%
    mutate(region = countrycode(region, origin = 'country.name', destination = 'country.name')) %>%
    filter(!region %in% c("Ascension Island", "Azores", "Barbuda", "Bonaire", "Canary Islands", "Chagos Archipelago", "Grenadines", "Heard Island", "Kosovo", "Madeira Islands", "Micronesia", "Saba", "Saint Martin", "Siachen Glacier", "Sint Eustatius", "Virgin Islands")) %>%
    filter(!is.na(region))
  
  merged_data <- world_map %>%
    left_join(data, by = "region")
  
  return(merged_data)
}

# Function to load data for multiple years and filter for the top 10 countries in the latest year
load_top10_data <- function(indicator, log_transform = FALSE) {
  data <- WDI(indicator = indicator, start = 1960, end = 2023) %>%
    mutate(country = countrycode(iso2c, origin = 'iso2c', destination = 'country.name')) %>%
    filter(!(iso2c %in% c("1A", "1W", "4E", "7E", "8S", "B8", "EU", "F1", "JG", "OE", "S1", "S2", "S3", "S4", "T2", "T3", "T4", "T5", "T6", "T7", "V1", "V2", "V3", "V4", "XC", "XD", "XE", "XF", "XG", "XH", "XI", "XJ", "XK", "XL", "XM", "XN", "XO", "XP", "XQ", "XT", "XU", "XY", "Z4", "Z7", "ZF", "ZG", "ZH", "ZI", "ZJ", "ZQ", "ZT"))) %>%
    filter(!(is.na(country))) %>% 
    filter(!(is.na(.data[[indicator]])))
  
  latest_year <- max(data$year, na.rm = TRUE)
  top10_countries <- data %>%
    filter(year == latest_year) %>%
    arrange(desc(.data[[indicator]])) %>%
    slice_head(n = 10) %>%
    pull(country)
  
  top10_data <- data %>%
    filter(country %in% top10_countries)
  
  if (log_transform) {
    top10_data <- top10_data %>%
      mutate(across(all_of(indicator), log))
  }
  
  return(top10_data)
}

# Define the UI
ui <- fluidPage(
  titlePanel("Economic Indicators Visualization with World Bank Data"),
  sidebarLayout(
    sidebarPanel(
      h4("Directions:"),
      p("1. Select an indicator from the dropdown menu."),
      p("2. Select a year from the dropdown menu."),
      p("3. Choose whether to apply a log transformation."),
      p("Please be patient while the data is being retrieved. If you see a world map without colors, it indicates that data is not available for the selected year."),
      selectInput("indicator", "Select Indicator:", 
                  choices = c(
                    "Population" = "SP.POP.TOTL", 
                    "GDP" = "NY.GDP.MKTP.CD", 
                    "Life Expectancy at Birth" = "SP.DYN.LE00.IN", 
                    "Military Expenditure" = "MS.MIL.XPND.CD", 
                    "Exports of Goods and Services" = "NE.EXP.GNFS.CD",
                    "Inflation, Consumer Prices" = "FP.CPI.TOTL",
                    "Unemployment Rate" = "SL.UEM.TOTL.ZS",
                    "CO2 Emissions" = "EN.ATM.CO2E.KT",
                    "Electricity Consumption" = "EG.USE.ELEC.KH.PC",
                    "Internet Users" = "IT.NET.USER.ZS",
                    "Urban Population" = "SP.URB.TOTL",
                    "Foreign Direct Investment" = "BX.KLT.DINV.WD.GD.ZS",
                    "Gross Savings" = "NY.GNS.ICTR.ZS",
                    "Poverty Headcount Ratio" = "SI.POV.DDAY",
                    "Education Expenditure" = "SE.XPD.TOTL.GD.ZS",
                    "Renewable Energy Consumption" = "EG.FEC.RNEW.ZS",
                    "Access to Electricity" = "EG.ELC.ACCS.ZS",
                    "Mobile Cellular Subscriptions" = "IT.CEL.SETS",
                    "Prevalence of Undernourishment" = "SN.ITK.DEFC.ZS",
                    "Forest Area" = "AG.LND.FRST.ZST",
                    "Employment in Agriculture" = "SL.AGR.EMPL.ZS",
                    "External Debt Stocks" = "DT.DOD.DECT.CD",
                    "Total Reserves" = "FI.RES.TOTL.CD",
                    "Gross Fixed Capital Formation" = "NE.GDI.FTOT.CD"
                  )),
      selectInput("year", "Select Year:", choices = as.character(1960:2023), selected = "2022"),
      checkboxInput("log_transform", "Log Transform", value = FALSE),
      uiOutput("indicator_description"),  # Moved to the sidebar
      width = 3
    ),
    mainPanel(
      plotOutput("map", height = "500px", width = "800px"),
      plotOutput("line_chart", height = "500px", width = "800px"),
      p("This visualization uses the WDI (World Development Indicators) package from the World Bank to provide a comprehensive view of various economic, social, and environmental metrics across different countries and regions. The data is sourced directly from the World Bank's extensive database."),
      p("Shiny app by Uras Demir")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  output$map <- renderPlot({
    indicator <- input$indicator
    year <- input$year
    log_transform <- input$log_transform
    data <- load_wb_data(indicator, year, log_transform)
    
    metric_label <- switch(indicator, 
                           "SP.POP.TOTL" = "Population", 
                           "NY.GDP.MKTP.CD" = "GDP", 
                           "SP.DYN.LE00.IN" = "Life Expectancy at Birth", 
                           "MS.MIL.XPND.CD" = "Military Expenditure", 
                           "NE.EXP.GNFS.CD" = "Exports of Goods and Services",
                           "FP.CPI.TOTL" = "Inflation, Consumer Prices",
                           "SL.UEM.TOTL.ZS" = "Unemployment Rate",
                           "EN.ATM.CO2E.KT" = "CO2 Emissions",
                           "EG.USE.ELEC.KH.PC" = "Electricity Consumption",
                           "IT.NET.USER.ZS" = "Internet Users",
                           "SP.URB.TOTL" = "Urban Population",
                           "BX.KLT.DINV.WD.GD.ZS" = "Foreign Direct Investment",
                           "NY.GNS.ICTR.ZS" = "Gross Savings",
                           "SI.POV.DDAY" = "Poverty Headcount Ratio",
                           "SE.XPD.TOTL.GD.ZS" = "Education Expenditure",
                           "EG.FEC.RNEW.ZS" = "Renewable Energy Consumption",
                           "EG.ELC.ACCS.ZS" = "Access to Electricity",
                           "IT.CEL.SETS" = "Mobile Cellular Subscriptions",
                           "SN.ITK.DEFC.ZS" = "Prevalence of Undernourishment",
                           "AG.LND.FRST.ZST" = "Forest Area",
                           "SL.AGR.EMPL.ZS" = "Employment in Agriculture",
                           "DT.DOD.DECT.CD" = "External Debt Stocks",
                           "FI.RES.TOTL.CD" = "Total Reserves",
                           "NE.GDI.FTOT.CD" = "Gross Fixed Capital Formation")
    
    title_suffix <- ifelse(log_transform, "(Log Transformed)", "")
    
    ggplot(data, aes(long, lat, group = group, fill = !!sym(indicator))) +
      geom_polygon(color = "white") +
      scale_fill_viridis_c(option = "viridis", na.value = "grey50") +
      labs(title = paste("World Map of", metric_label, "in", year, title_suffix),
           fill = metric_label,
           caption = "Shiny app by Uras Demir") +
      theme_minimal() +
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
  })
  
  output$line_chart <- renderPlot({
    indicator <- input$indicator
    log_transform <- input$log_transform
    data <- load_top10_data(indicator, log_transform)
    
    metric_label <- switch(indicator, 
                           "SP.POP.TOTL" = "Population", 
                           "NY.GDP.MKTP.CD" = "GDP", 
                           "SP.DYN.LE00.IN" = "Life Expectancy at Birth", 
                           "MS.MIL.XPND.CD" = "Military Expenditure", 
                           "NE.EXP.GNFS.CD" = "Exports of Goods and Services",
                           "FP.CPI.TOTL" = "Inflation, Consumer Prices",
                           "SL.UEM.TOTL.ZS" = "Unemployment Rate",
                           "EN.ATM.CO2E.KT" = "CO2 Emissions",
                           "EG.USE.ELEC.KH.PC" = "Electricity Consumption",
                           "IT.NET.USER.ZS" = "Internet Users",
                           "SP.URB.TOTL" = "Urban Population",
                           "BX.KLT.DINV.WD.GD.ZS" = "Foreign Direct Investment",
                           "NY.GNS.ICTR.ZS" = "Gross Savings",
                           "SI.POV.DDAY" = "Poverty Headcount Ratio",
                           "SE.XPD.TOTL.GD.ZS" = "Education Expenditure",
                           "EG.FEC.RNEW.ZS" = "Renewable Energy Consumption",
                           "EG.ELC.ACCS.ZS" = "Access to Electricity",
                           "IT.CEL.SETS" = "Mobile Cellular Subscriptions",
                           "SN.ITK.DEFC.ZS" = "Prevalence of Undernourishment",
                           "AG.LND.FRST.ZST" = "Forest Area",
                           "SL.AGR.EMPL.ZS" = "Employment in Agriculture",
                           "DT.DOD.DECT.CD" = "External Debt Stocks",
                           "FI.RES.TOTL.CD" = "Total Reserves",
                           "NE.GDI.FTOT.CD" = "Gross Fixed Capital Formation")
    
    latest_year <- max(data$year, na.rm = TRUE)
    
    ggplot(data, aes(x = year, y = !!sym(indicator), color = country, shape = country, group = country)) +
      geom_line() +
      geom_point(size = 3) +
      scale_shape_manual(values = 1:10) +  # Manually specify shapes for 10 countries
      labs(title = paste("Top 10 Countries by", metric_label, "from 1960 to 2023"),
           x = "Year", 
           y = metric_label,
           color = "Country",
           shape = "Country",
           caption = "Shiny app by Uras Demir") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(1960, 2023, by = 5)) +
      theme(plot.title = element_text(size = 20, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.title = element_text(size = 12),
            legend.text = element_text(size = 10),
            legend.position = "right")
  })
  
  output$indicator_description <- renderUI({
    indicator <- input$indicator
    description <- switch(indicator,
                          "SP.POP.TOTL" = "Total population counts all residents regardless of legal status or citizenship.",
                          "NY.GDP.MKTP.CD" = "GDP at purchaser's prices is the sum of gross value added by all resident producers in the economy.",
                          "SP.DYN.LE00.IN" = "Life expectancy at birth indicates the number of years a newborn infant would live if prevailing patterns of mortality at the time of its birth were to stay the same throughout its life.",
                          "MS.MIL.XPND.CD" = "Military expenditure is the current and capital expenditure on the armed forces.",
                          "NE.EXP.GNFS.CD" = "Exports of goods and services represent the value of all goods and other market services provided to the rest of the world.",
                          "FP.CPI.TOTL" = "Inflation as measured by the consumer price index reflects the annual percentage change in the cost to the average consumer of acquiring a basket of goods and services.",
                          "SL.UEM.TOTL.ZS" = "Unemployment refers to the share of the labor force that is without work but available for and seeking employment.",
                          "EN.ATM.CO2E.KT" = "CO2 emissions are those stemming from the burning of fossil fuels and the manufacture of cement.",
                          "EG.USE.ELEC.KH.PC" = "Electricity consumption measures the electric energy use per capita.",
                          "IT.NET.USER.ZS" = "Internet users are people with access to the worldwide network.",
                          "SP.URB.TOTL" = "Urban population refers to people living in urban areas as defined by national statistical offices.",
                          "BX.KLT.DINV.WD.GD.ZS" = "Foreign direct investment refers to the net inflows of investment to acquire a lasting management interest in an enterprise operating in an economy other than that of the investor.",
                          "NY.GNS.ICTR.ZS" = "Gross savings are calculated as gross national income less total consumption, plus net transfers.",
                          "SI.POV.DDAY" = "Poverty headcount ratio at $1.90 a day is the percentage of the population living on less than $1.90 a day at 2011 international prices.",
                          "SE.XPD.TOTL.GD.ZS" = "Education expenditure is the current operating expenditures in education, including wages and salaries and excluding capital investments in buildings and equipment.",
                          "EG.FEC.RNEW.ZS" = "Renewable energy consumption is the share of renewable energy in total final energy consumption.",
                          "EG.ELC.ACCS.ZS" = "Access to electricity is the percentage of the population with access to electricity.",
                          "IT.CEL.SETS" = "Mobile cellular subscriptions refer to the number of subscriptions to a public mobile telephone service using cellular technology.",
                          "SN.ITK.DEFC.ZS" = "Prevalence of undernourishment refers to the percentage of the population whose food intake is insufficient to meet dietary energy requirements.",
                          "AG.LND.FRST.ZST" = "Forest area is the land area that is covered by forest.",
                          "SL.AGR.EMPL.ZS" = "Employment in agriculture includes people working in farming, fishing, and forestry.",
                          "DT.DOD.DECT.CD" = "External debt stocks refer to the total debt of a country that is owed to foreign creditors.",
                          "FI.RES.TOTL.CD" = "Total reserves comprise holdings of monetary gold, special drawing rights, reserves of IMF members held by the IMF, and holdings of foreign exchange under the control of monetary authorities.",
                          "NE.GDI.FTOT.CD" = "Gross fixed capital formation is the net increase in physical assets (investment minus disposals) within the economy.")
    
    tagList(
      h4("Indicator Description"),
      p(description)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
