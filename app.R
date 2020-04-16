# install.packages(c("rsconnect", "shiny", "ggplot2", "scales", "shinythemes", "tidyverse", 
#                    "mapcan", "ggsci", "ggrepel", "ggiraph", "plotly", "shinydashbaord", "shinyWidgets", "ggthemes"))

require(rsconnect)
require(shiny)
require(ggplot2)
require(scales)
require(shinythemes)
require(mapcan)
require(ggsci)
require(ggrepel)
require(ggiraph)
require(maps)
require(plotly)
require(shinydashboard)
require(shinyWidgets)
require(ggthemes)

# read in data for each page
officialRate <- read.csv("data/officialRateData.csv", head=T, sep=",")
officialRate$Age.group <- factor(officialRate$Age.group, levels=c("All age groups", "Under 18 years", "18 to 64 years", "65 years and over"))
officialRate$Sex <- factor(officialRate$Sex, levels = c("Both sexes", "Males", "Females"))

resilience <- read.csv("data/resilienceData.csv", head=T, sep=",")
#resilience$Age.group <- factor(resilience$Age.group, levels=c("All age groups", "Under 18 years", "18 to 64 years", "65 years and over"))
resilience$Sex <- factor(resilience$Sex, levels = c("Both sexes", "Males", "Females"))
resilience$Year <- as.character(resilience$Year)





ui <- fluidPage(
  theme = shinytheme("paper"),
  useShinydashboard(),
  tags$style(
    HTML(
      ".navbar-default .navbar-brand {color: #212121;}
      .navbar-default .navbar-brand:hover {color: #212121;}
      .small-box {min-height: 35px;}
      .small-box .icon-large {font-size: 30px;}
      .small-box .bg-light-blue {background-color: #EFC00099;}
      .small-box>.inner {padding: 2px;}"
    )
    ),
  navbarPage(
    "Interactive Poverty Dashboard",
    tabPanel(
      "Official poverty rate",
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(
            "year",
            label = "Year of interest:",
            choices = seq(max(officialRate$Year), min(officialRate$Year),-1),
            selected = max(officialRate$Year)
          ),
          selectInput(
            "geo",
            label = "Geography:",
            choices = c(
              "Canada" = "Canada",
              "Newfoundland and Labrador" = "NL",
              "Prince Edward Island" = "PE",
              "Nova Scotia" = "NS",
              "New Brunswick" = "NB",
              "Quebec" = "QC",
              "Ontario" = "ON",
              "Manitoba" = "MB",
              "Saskatchewan" = "SK",
              "Alberta" = "AB",
              "British Columbia" = "BC",
              "Yukon" = "YT",
              "Northwest Territories" = "NT",
              "Nunavut" = "NU"
            ),
            selected = "Canada"
          ),
          selectInput(
            "age",
            label = "Age group:",
            choices = c(
              "All age groups",
              "Under 18 years",
              "18 to 64 years",
              "65 years and over"
            ),
            selected = "All age groups"
          ),
          selectInput(
            "sex",
            label = "Sex:",
            choices = c("Both sexes", "Males", "Females"),
            selected = "Both sexes"
          )
        ),
        mainPanel(
          width = 10,
          fluidRow(
            style = "display: flex; align-items: center;",
            column(5, girafeOutput("officialMap")),
            column(
              7,
              selectInput(
                "officialByVar",
                label = "By variable(s):",
                choices = c("None" = "Statistics",
                            "Age group" = "Age.group",
                            "Sex"),
                selected = "Statistics"
              ),
              girafeOutput("officialYears")
            )
          ),
          br(),
          fluidRow(
            style = "display: flex; align-items: center;",
            column(4, girafeOutput("officialAge")),
            column(4, girafeOutput("officialSex")),
            column(4, wellPanel(
              p(
                em("Canada's Poverty Reduction Strategy"),
                " introduces the Official Poverty Line for Canada along with the Dashboard of
                12 indicators to track progress on poverty reduction for Canadians and their households.
                This dashboard uses Statistics Canada data to visualize these 12 indicators."
              ),
              p(
                "The ",
                em("Poverty Reduction Strategy"),
                " sets an official measure of poverty: the Market Basket Measure as
                Canada's Official Poverty Line, based on the cost of a basket of goods and services that individuals
                and families require to meet their basic needs and achieve a modest standard of living in communities
                across the country (ESDC, 2018)."
              )
              ))
              )
              )
              )
          ),
    
    tabPanel("Dignity",
             p("this isn't ready yet! check back later...")),
    
    tabPanel(
      "Opportunity and inclusion",
      p("this isn't ready yet! check back later...")),
    
    tabPanel(
      "Resilience and security",
      fluidRow(wellPanel( "panel for main values"
        # flowLayout(
        #   valueBoxOutput("averageGapBox", width = NULL),
        #   valueBoxOutput("hourlyWageBox", width = NULL),
        #   valueBoxOutput("assetResilienceBox", width = NULL),
        #   valueBoxOutput("povertyEntranceBox", width = NULL),
        #   valueBoxOutput("povertyExitBox", width = NULL)
        # )
      )),
      fluidRow(sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(
            "resStat",
            label = "Select an indicator:",
            choices = c(
              "Average poverty gap",
              "Median hourly wage",
              "Low income entry rate",
              "Low income exit rate"
            ),
            selected = "Average poverty gap"
          ),
          selectInput(
            "resYear",
            label = "Year of interest:",
            choices = seq(as.numeric(max(resilience$Year[resilience$Statistics == "Average poverty gap"])), 2006,-1),
            selected = max(resilience$Year)
          ),
          selectInput(
            "resGeo",
            label = "Geography:",
            choices = c(
              "Canada" = "Canada",
              "Newfoundland and Labrador" = "NL",
              "Prince Edward Island" = "PE",
              "Nova Scotia" = "NS",
              "New Brunswick" = "NB",
              "Quebec" = "QC",
              "Ontario" = "ON",
              "Manitoba" = "MB",
              "Saskatchewan" = "SK",
              "Alberta" = "AB",
              "British Columbia" = "BC",
              "Yukon" = "YT",
              "Northwest Territories" = "NT",
              "Nunavut" = "NU"
            ),
            selected = "Canada"
          ),
          selectInput(
            "resAge",
            label = "Age group:",
            choices = c(
              "All age groups",
              "Under 18 years",
              "18 to 64 years",
              "65 years and over"
            ),
            selected = "All age groups"
          ),
          selectInput(
            "resSex",
            label = "Sex:",
            choices = c("Both sexes", "Males", "Females"),
            selected = "Both sexes"
          )
        ),
        mainPanel(
          width = 10,
          fluidRow(
            style = "display: flex; align-items: center;",
            column(5, girafeOutput("resMapOut")),
            column(
              7,
              selectInput(
                "resByVar",
                label = "By variable(s):",
                choices = c("None" = "Statistics",
                            "Age group" = "Age.group",
                            "Sex"),
                selected = "Statistics"
              ),
              girafeOutput("resYearsOut")
            )
          ),
          br(),
          fluidRow(
            style = "display: flex; align-items: top;",
            column(4, girafeOutput("resAgeOut")),
            column(4, girafeOutput("resSexOut")),
            column(4, wellPanel(
              p(
                em("Canada's Poverty Reduction Strategy"),
                " introduces the Official Poverty Line for Canada along with the Dashboard of
                12 indicators to track progress on poverty reduction for Canadians and their households.
                This dashboard uses Statistics Canada data to visualize these 12 indicators."
              )
              ))
              )
              )
              ))
          )
      )
      )

officialByVarVec <- c("None" = "Statistics",
                      "Age group" = "Age.group",
                      "Sex" = "Sex")

server <- function(input, output) {
  
  # official poverty rate plots
  output$officialMap <- renderGirafe({
    officialMapData <- subset(officialRate, Statistics == "Official poverty rate" &
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
                    scale_fill_gradient(low="#F5F5F5", high="#0073C2FF", name="Official poverty rate")
    
    girafe(ggobj = officialMapPlot, width_svg = 5)

  })
  
  output$officialYears <- renderGirafe({
    officialYearsData <- if (input$officialByVar == "Statistics") subset(officialRate, Statistics == "Official poverty rate" &
                                                                              Sex == input$sex &
                                                                              Age.group == input$age &
                                                                              GEO == input$geo)
                        else if (input$officialByVar == "Age.group") subset(officialRate, Statistics == "Official poverty rate" &
                                                                            Sex == input$sex &
                                                                            Age.group != "All age groups" &
                                                                            GEO == input$geo)
                        else if (input$officialByVar =="Sex") subset(officialRate, Statistics == "Official poverty rate" &
                                                                     Age.group == input$age &
                                                                     Sex != "Both sexes" &
                                                                     GEO == input$geo)
    
    officialYearsPlot <- ggplot(officialYearsData) + 
                          geom_line(aes(x=Year, y=VALUE, colour=get(input$officialByVar), group=get(input$officialByVar)), size=1.5) +
                          geom_point_interactive(aes(x=Year, y=VALUE, tooltip=paste0(Year, ": ", VALUE, "%"), 
                                                     colour=get(input$officialByVar), group=get(input$officialByVar)), size = 2.5) +
                          scale_colour_jco() +
                          theme_classic() + theme(text=element_text(family="Roboto"), plot.margin=grid::unit(c(0,0,0,0), "mm"),legend.position = "bottom") +
                          {if (input$officialByVar != "Statistics") guides(colour = "legend") else guides(colour=F)} +
                          scale_y_continuous(limits = c(0,NA)) + 
                          labs(y="Official poverty rate (%)", 
                               x="Year", 
                               colour = names(officialByVarVec)[officialByVarVec == input$officialByVar],
                               title=paste("Official poverty rate by year")) 
      
      girafe(ggobj = officialYearsPlot, width_svg = 8, height_svg = 4)
    
  })
  
  officialSexData <- reactive ({officialRate$highlight <- ifelse((officialRate$Sex == input$sex), 1, ifelse((input$sex == "Both sexes"),1,0))
  return(subset(officialRate,
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
    
    girafe(ggobj = officialSexPlot, height_svg = 5, width_svg = 4)
  })
  
  officialAgeData <- reactive ({officialRate$highlight <- ifelse((officialRate$Age.group == input$age), 1, ifelse((input$age == "All age groups"),1,0))
  return(subset(officialRate,
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
    
    girafe(ggobj = officialAgePlot, height_svg = 5, width_svg = 4)

  })
  
  # resilience and security page
  
  # value boxes
  output$averageGapBox <- renderValueBox({
    
    gapBoxData <- subset(resilience, 
                         GEO == "Canada" &
                           Sex == "Both sexes" &
                           Age.group == "All age groups" &
                           Statistics == "Average poverty gap")
    
    thisPeriod <- gapBoxData$VALUE[gapBoxData$Year == max(gapBoxData$Year)]
    lastPeriod <- gapBoxData$VALUE[gapBoxData$Year == max(gapBoxData$Year[gapBoxData$Year != max(gapBoxData$Year)])]
    
    valueBox(
      value = h4(paste0(thisPeriod,"%")),
      subtitle = paste0("Average poverty gap, ",max(gapBoxData$Year)),
      icon = icon("balance-scale"),
      color =  "light-blue")
  })
  
  output$hourlyWageBox <- renderValueBox({
    
    wageBoxData <- subset(resilience, 
                         GEO == "Canada" &
                           Sex == "Both sexes" &
                           Age.group == "15 years and over" &
                           Statistics == "Median hourly wage")
    
    thisPeriod <- wageBoxData$VALUE[wageBoxData$Year == max(wageBoxData$Year)]
    lastPeriod <- wageBoxData$VALUE[wageBoxData$Year == max(wageBoxData$Year[wageBoxData$Year != max(wageBoxData$Year)])]
    
    valueBox(
      value = h4(paste0(thisPeriod,"$")),
      subtitle = paste0("Median hourly wage, ", max(wageBoxData$Year)),
      icon = icon("coins"),
      color = "light-blue")
  })
  
  output$assetResilienceBox <- renderValueBox({
    
    valueBox(
      value = h4("NA"),
      subtitle = "Asset resilience, NA",
      icon = icon("life-ring"),
      color = "light-blue")
  })
  
  output$povertyEntranceBox <- renderValueBox({
    
    entryBoxData <- subset(resilience, 
                          GEO == "Canada" &
                            Sex == "Both sexes" &
                            Age.group == "18 years and over" &
                            Statistics == "Low income entry rate")
    
    thisPeriod <- entryBoxData$VALUE[entryBoxData$Year == max(entryBoxData$Year)]
    lastPeriod <- entryBoxData$VALUE[entryBoxData$Year == max(entryBoxData$Year[entryBoxData$Year != max(entryBoxData$Year)])]
    
    valueBox(
      value = h4(paste0(thisPeriod,"%")),
      subtitle = paste0("Low income entry rate, ", max(entryBoxData$Year)),
      icon = icon("sign-in-alt"),
      color = "light-blue")
  })
  
  output$povertyExitBox <- renderValueBox({
    
    exitBoxData <- subset(resilience, 
                           GEO == "Canada" &
                             Sex == "Both sexes" &
                             Age.group == "18 years and over" &
                             Statistics == "Low income exit rate")
    
    thisPeriod <- exitBoxData$VALUE[exitBoxData$Year == max(exitBoxData$Year)]
    lastPeriod <- exitBoxData$VALUE[exitBoxData$Year == max(exitBoxData$Year[exitBoxData$Year != max(exitBoxData$Year)])]
    
    valueBox(
      value = h4(paste0(thisPeriod,"%")),
      subtitle = paste0("Low income exit rate, ",max(exitBoxData$Year)),
      icon = icon("sign-out-alt"),
      color = "light-blue")
  })
  
  # resilience and security plots
  output$resMapOut <- renderGirafe({
    resMapData <- subset(resilience, Statistics == input$resStat &
                                Year == input$resYear &
                                Sex == input$resSex &
                                Age.group == input$resAge &
                                GEO != "Canada")
    
    resMapData$GEO <- as.character(resMapData$GEO)
    
    resMapData <- dplyr::left_join(mapcan(boundaries = province, type = standard),
                                   resMapData,
                                        by = c("pr_alpha" = "GEO"))
    
    resMapPlot <- ggplot() +
      geom_polygon_interactive(data=resMapData, 
                               aes(x = long, y = lat, group = group, 
                                   fill=VALUE, tooltip=paste(pr_alpha, VALUE))) +
      coord_fixed() +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), legend.position = "bottom", axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(), panel.spacing = element_blank(),
            legend.box.spacing = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
            text=element_text(family="Roboto")) +
            scale_fill_gradient(low="#F5F5F5", high="#BC3C29FF", name=input$resStat)
    
    girafe(ggobj = resMapPlot, width_svg = 5)
    
  })
  
  output$resYearsOut <- renderGirafe({
    resYearsData <- if (input$resByVar == "Statistics") subset(resilience, Statistics == input$resStat &
                                                                 Sex == input$resSex &
                                                                 Age.group == input$resAge &
                                                                 GEO == input$resGeo)
    else if (input$resByVar == "Age.group") subset(resilience, Statistics == input$resStat &
                                                     Sex == input$resSex &
                                                     Age.group != "All age groups" &
                                                     GEO == input$resGeo)
    else if (input$resByVar =="Sex") subset(resilience, Statistics == input$resStat &
                                              Age.group == input$resAge &
                                              Sex != "Both sexes" &
                                              GEO == input$resGeo)

    resYearsPlot <- ggplot(resYearsData) +
      geom_line(aes(x=Year, y=VALUE, colour=get(input$resByVar), group=get(input$resByVar)), size=1.5) +
      geom_point_interactive(aes(x=Year, y=VALUE, tooltip=paste0(Year, ": ", VALUE),
                                 colour=get(input$resByVar), group=get(input$resByVar)), size = 2.5) +
      scale_colour_nejm() +
      theme_classic() + theme(text=element_text(family="Roboto"), plot.margin=grid::unit(c(0,0,0,0), "mm"),legend.position = "bottom") +
      {if (input$resByVar != "Statistics") guides(colour = "legend") else guides(colour=F)} +
      labs(y=input$resStat,
           x="Year",
           colour = names(officialByVarVec)[officialByVarVec == input$resByVar],
           title=paste0(input$resStat, " by year"))

    girafe(ggobj = resYearsPlot, width_svg = 8, height_svg = 4) 

  })

  resSexData <- reactive ({resilience$highlight <- ifelse((resilience$Sex == input$resSex), 1, ifelse((input$resSex == "Both sexes"),1,0))
                            return(subset(resilience,
                                          GEO == input$resGeo &
                                            Sex != "Both sexes" &
                                            Age.group == input$resAge &
                                            Year == input$resYear &
                                            Statistics == input$resStat))})

  output$resSexOut <- renderGirafe({

    resSexPlot <- ggplot(resSexData()) +
      geom_col_interactive(aes(x=Sex, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE))) +
      theme_classic() + scale_fill_nejm() + theme(text=element_text(family="Roboto")) +
      scale_alpha(range = c(max(0.45, min(resSexData()$highlight)),1)) +
      guides(alpha = FALSE, fill=FALSE) +
      labs(y=input$resStat,
           x="Sex",
           title=paste0(input$resStat," by sex, ",input$year))

    girafe(ggobj = resSexPlot, height_svg = 5, width_svg = 4)
  })

  resAgeData <- reactive ({resilience$highlight <- ifelse((resilience$Age.group == input$resAge), 1, ifelse((input$resAge == "All age groups"),1,0))
                          return(subset(resilience,
                                        GEO == input$resGeo &
                                          Sex == input$resSex &
                                          Age.group != "All age groups" &
                                          Year == input$resYear &
                                          Statistics == input$resStat))})

  output$resAgeOut <- renderGirafe({

    resAgePlot <- ggplot(resAgeData()) +
      geom_col_interactive(aes(x=Age.group, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE))) +
      theme_classic() + scale_fill_nejm() + theme(text=element_text(family="Roboto")) +
      scale_alpha(range = c(max(0.45, min(resAgeData()$highlight)),1)) +
      guides(alpha = FALSE, fill = FALSE) +
      labs(y=input$resStat,
           x="Age group",
           title=paste0(input$resStat," by age group, ",input$year))

    girafe(ggobj = resAgePlot, height_svg = 5, width_svg = 4)

  })
  
  
}

shinyApp(ui, server)



