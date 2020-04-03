# install.packages(c("rsconnect", "shiny", "ggplot2", "scales", "shinythemes", "tidyverse", 
#                    "mapcan", "ggsci", "ggrepel", "ggiraph", "plotly"))

library(rsconnect)
library(shiny)
library(ggplot2)
library(scales)
library(shinythemes)
library(mapcan)
library(ggsci)
library(ggrepel)
library(ggiraph)
library(maps)
library(plotly)

data <- read.csv("data/finalData.csv", head=T, sep=",")
data$Age.group <- factor(data$Age.group, levels=c("All age groups", "Under 18 years", "15 to 24 years", "18 to 24 years",
                                                  "25 to 54 years", "55 to 64 years", "18 to 64 years",
                                                  "55 years and over", "65 years and over"))
data$Sex <- factor(data$Sex, levels = c("Both sexes", "Males", "Females"))


ui <- fluidPage(theme=shinytheme("paper"), 
                tags$style(HTML(".navbar-default .navbar-brand {color: #212121;}
                                 .navbar-default .navbar-brand:hover {color: #212121;}")),
                navbarPage("Interactive Poverty Dashboard",
                           tabPanel("Official poverty rate",
                                    sidebarLayout(
                                      sidebarPanel(
                                        width = 2,
                                        selectInput("year", 
                                                    label = "Year of interest:",
                                                    choices = c("2018", "2017", "2016", "2015", "2014", "2013", "2012", "2011", "2010", "2009", "2008", "2007", "2006"),
                                                    selected = "2018"),
                                        selectInput("geo",
                                                    label = "Geography:",
                                                    choices = c("Canada" = "Canada", "Newfoundland and Labrador" = "NL", "Prince Edward Island" = "PE",
                                                                "Nova Scotia" = "NS", "New Brunswick" = "NB", "Quebec" ="QC",
                                                                "Ontario" = "ON", "Manitoba" ="MB", "Saskatchewan" = "SK", "Alberta" = "AB",
                                                                "British Columbia" = "BC", "Yukon" = "YT", "Northwest Territories" = "NT",
                                                                "Nunavut" = "NU"),
                                                    selected = "Canada"),
                                        selectInput("age",
                                                    label = "Age group:",
                                                    choices = c("All age groups", "Under 18 years", "18 to 64 years", "65 years and over"),
                                                    selected = "All age groups"),
                                        selectInput("sex",
                                                    label = "Sex:",
                                                    choices = c("Both sexes", "Males", "Females"),
                                                    selected = "Both sexes")
                                      ),
                                      mainPanel(
                                        width = 10,
                                        fluidRow(style="display: flex; align-items: center;",
                                                 column(5, girafeOutput("officialMap")),
                                                 column(7, selectInput("officialByVar", 
                                                                       label = "By variable(s):",
                                                                       choices = c("None" = "Statistics",
                                                                                   "Age group" = "Age.group",
                                                                                   "Sex"),
                                                                       selected = "Statistics"),
                                                            girafeOutput("officialYears"))
                                        ),
                                        br(),
                                        fluidRow(style="display: flex; align-items: center;",
                                          column(4, girafeOutput("officialAge")),
                                          column(4, girafeOutput("officialSex")),
                                          column(4, wellPanel(p(em("Canada's Poverty Reduction Strategy"), 
                                                                " introduces the Official Poverty Line for Canada along with the Dashboard of 
                                                           12 indicators to track progress on poverty reduction for Canadians and their households."),
                                                              p("The ", em("Poverty Reduction Strategy")," sets an official measure of poverty: the Market Basket Measure as 
                                                           Canada's Official Poverty Line, based on the cost of a basket of goods and services that individuals 
                                                           and families require to meet their basic needs and achieve a modest standard of living in communities 
                                                           across the country (ESDC, 2018).")))
                                        )
                                      )
                                    )),
                           navbarMenu("Dignity",
                                      tabPanel("Overview",
                                               p("This isn't ready yet. Check back later!"),
                                               p("There will be info on deep income, unmet health needs ...")),
                                      tabPanel("Deep Income",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Unmet housing needs",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Unmet health needs",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Food insecurity",
                                               p("This isn't ready yet. Check back later!"))
                           ),
                           navbarMenu("Opportunity and inclusion",
                                      tabPanel("Overview",
                                               p("This isn't ready yet. Check back later!"),
                                               p("There will be info on deep income, unmet health needs ...")),
                                      tabPanel("Relative low income",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Bottom 40 percent income share",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Youth engagement",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Literacy and numeracy",
                                               p("This isn't ready yet. Check back later!"))
                           ),
                           navbarMenu("Resilience and security",
                                      tabPanel("Overview",
                                               p("This isn't ready yet. Check back later!"),
                                               p("There will be info on deep income, unmet health needs ...")),
                                      tabPanel("Median hourly wage",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Average poverty gap",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Asset resilience",
                                               p("This isn't ready yet. Check back later!")),
                                      tabPanel("Poverty entry and exit rates",
                                               p("This isn't ready yet. Check back later!"))
                           )
                )
)

officialByVarVec <- c("None" = "Statistics",
                      "Age group" = "Age.group",
                      "Sex" = "Sex")

server <- function(input, output) {
  
  # official poverty rate plots
  output$officialMap <- renderGirafe({
    officialMapData <- subset(data, Statistics == "Official poverty rate" &
                                    Year == input$year &
                                    Sex == input$sex &
                                    Age.group == input$age &
                                    GEO != "Canada")

    officialMapData$GEO <- as.character(officialMapData$GEO)

    officialMapData <- dplyr::left_join(mapcan(boundaries = province, type = standard),
                                        officialMapData,
                                        by = c("pr_alpha" = "GEO"))

    officialMapPlot <- ggplot() +
                    geom_polygon_interactive(data=officialMapData, 
                                             aes(x = long, y = lat, group = group, 
                                                 fill=VALUE, tooltip=paste(pr_alpha, VALUE))) +
                    coord_fixed() +
                    theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), legend.position = "bottom", axis.title = element_blank(),
                          axis.text = element_blank(), axis.ticks = element_blank(), panel.spacing = element_blank(),
                          legend.box.spacing = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
                          text=element_text(family="Roboto")) +
                    scale_fill_material("blue", name="Official poverty rate")
    
    girafe(ggobj = officialMapPlot)

  })
  
  output$officialYears <- renderGirafe({
    officialYearsData <- if (input$officialByVar == "Statistics") subset(data, Statistics == "Official poverty rate" &
                                                                              Sex == input$sex &
                                                                              Age.group == input$age &
                                                                              GEO == input$geo)
                        else if (input$officialByVar == "Age.group") subset(data, Statistics == "Official poverty rate" &
                                                                            Sex == input$sex &
                                                                            Age.group != "All age groups" &
                                                                            GEO == input$geo)
                        else if (input$officialByVar =="Sex") subset(data, Statistics == "Official poverty rate" &
                                                                     Age.group == input$age &
                                                                     Sex != "Both sexes" &
                                                                     GEO == input$geo)
    
    officialYearsPlot <- ggplot(officialYearsData) + 
                          geom_line(aes(x=Year, y=VALUE, colour=get(input$officialByVar), group=get(input$officialByVar)), size=1) +
                          geom_point_interactive(aes(x=Year, y=VALUE, tooltip=paste0(Year, ": ", VALUE, "%"), 
                                                     colour=get(input$officialByVar), group=get(input$officialByVar)), size = 2) +
                          scale_colour_jco() +
                          theme_classic() + theme(text=element_text(family="Roboto"), plot.margin=grid::unit(c(0,0,0,0), "mm"),legend.position = "bottom") +
                          {if (input$officialByVar != "Statistics") guides(colour = "legend") else guides(colour=F)} +
                          scale_y_continuous(limits = c(0,NA)) + 
                          labs(y="Official poverty rate (%)", 
                               x="Year", 
                               colour = names(officialByVarVec)[officialByVarVec == input$officialByVar],
                               title=paste("Official poverty rate by year")) 
      
      girafe(ggobj = officialYearsPlot)
    
  })
  
  officialSexData <- reactive ({data$highlight <- ifelse((data$Sex == input$sex), 1, ifelse((input$sex == "Both sexes"),1,0))
  return(subset(data,
                GEO == input$geo &
                  Sex != "Both sexes" &
                  Age.group == input$age &
                  Year == input$year &
                  Statistics == "Official poverty rate"))})
  
  output$officialSex <- renderGirafe({
    
    officialSexPlot <- ggplot(officialSexData()) + 
                        geom_col_interactive(aes(x=Sex, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE,"%"))) + 
                        theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto")) +
                        scale_alpha(range = c(max(0.45, min(officialSexData()$highlight)),1)) +
                        guides(alpha = FALSE, fill=FALSE) +
                        labs(y="Official poverty rate (%)", 
                             x="Sex", 
                             title=paste("Official poverty rate (%) by sex,",input$year))
    
    girafe(ggobj = officialSexPlot)
  })
  
  officialAgeData <- reactive ({data$highlight <- ifelse((data$Age.group == input$age), 1, ifelse((input$age == "All age groups"),1,0))
  return(subset(data,
                GEO == input$geo &
                  Sex == input$sex &
                  Age.group != "All age groups" &
                  Year == input$year &
                  Statistics == "Official poverty rate"))})
  
  output$officialAge <- renderGirafe({
    
    officialAgePlot <- ggplot(officialAgeData()) + 
                        geom_col_interactive(aes(x=Age.group, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE,"%"))) + 
                        theme_classic() + scale_fill_jco() + theme(text=element_text(family="Roboto")) +
                        scale_alpha(range = c(max(0.45, min(officialAgeData()$highlight)),1)) +
                        guides(alpha = FALSE, fill = FALSE) +
                        labs(y="Official poverty rate (%)", 
                             x="Age group", 
                             title=paste("Official poverty rate (%) by age group,",input$year))
    
    girafe(ggobj = officialAgePlot)

  })
  
}

shinyApp(ui, server)




