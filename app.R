# install.packages(c("rsconnect", "shiny", "ggplot2", "scales", "shinythemes", "tidyverse", 
#                    "mapcan", "ggsci", "ggrepel", "ggiraph", "plotly", "shinydashbaordPlus", "shinyWidgets", "ggthemes"))

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
require(shinydashboardPlus)
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

opportunity <- read.csv("data/opportunityData.csv", head=T, sep=",")
opportunity$Sex <- factor(opportunity$Sex, levels = c("Both sexes", "Males", "Females"))
opportunity$Age.group <- factor(opportunity$Age.group, levels=c("All age groups", "16 years and over", "Under 18 years", "18 to 64 years", "65 years and over"))




ui <- fluidPage(
  theme = shinytheme("paper"),
  useShinydashboardPlus(),
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
            choices = unique(officialRate$GEO),
            selected = "Canada"
          ),
          selectInput(
            "age",
            label = "Age group:",
            choices = unique(officialRate$Age.group)
          ),
          selectInput(
            "sex",
            label = "Sex:",
            choices = unique(officialRate$Sex)
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
                label = NULL,
                choices = c("No by variable" = "Statistics",
                            "By age group" = "Age.group",
                            "By sex" = "Sex"),
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
      fluidRow(sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(
            "oppStat",
            label = "Select an indicator:",
            choices = unique(opportunity$Statistics),
            selected = "Relative low income"
          ),
          selectInput(
            "oppYear",
            label = "Year of interest:",
            choices = seq(as.numeric(max(opportunity$Year[opportunity$Statistics == "Relative low income"])), 2006,-1),
            selected = max(opportunity$Year[opportunity$Statistics == "Relative low income"])
          ),
          selectInput(
            "oppGeo",
            label = "Geography:",
            choices = unique(opportunity$GEO[opportunity$Statistics == "Relative low income"]),
            selected = "Canada"
          ),
          selectInput(
            "oppAge",
            label = "Age group:",
            choices = unique(opportunity$Age.group[opportunity$Statistics == "Relative low income"]),
            selected = "All age groups"
          ),
          selectInput(
            "oppSex",
            label = "Sex:",
            choices = unique(opportunity$Sex[opportunity$Statistics == "Relative low income"]),
            selected = "Both sexes"
          ),
          actionButton("updateOpp",
                       label = "Apply changes",
                       width = "100%",
                       class = "btn btn-primary btn-custom")
        ),
        mainPanel(
          width = 10,
          fluidRow(
            style = "display: flex; align-items: center;",
            column(5, girafeOutput("oppMapOut")),
            column(
              7,
              selectInput(
                "oppByVar",
                label = NULL,
                choices = c("No by variable" = "Statistics",
                            "By age group" = "Age.group",
                            "By sex" = "Sex"),
                selected = "Statistics"
              ),
              
              girafeOutput("oppYearsOut")
            )
          ),
          br(),
          fluidRow(
            style = "display: flex; align-items: top;",
            column(4, girafeOutput("oppAgeOut")),
            column(4, girafeOutput("oppSexOut")),
            column(4, wellPanel(
              p(
                em("Canada's Poverty Reduction Strategy"),
                " introduces the Official Poverty Line for Canada along with the Dashboard of
                12 indicators to track progress on poverty reduction for Canadians and their households.
                This dashboard uses Statistics Canada data to visualize these 12 indicators."
              ),
              p("Note that tabulations by sex or age group are not available for the bottom 40% share indicator.")
              ))
            )
            )
            ))
      ),
    
    tabPanel(
      "Resilience and security",
      # fluidRow(box(title=NULL,
      #               width = 12,
      #   
      #     column(valueBoxOutput("averageGapBox", width = NULL),width=2),
      #     column(valueBoxOutput("hourlyWageBox", width = NULL),width=2),
      #     column(valueBoxOutput("assetResilienceBox", width = NULL),width=2),
      #     column(valueBoxOutput("povertyEntranceBox", width = NULL),width=2),
      #     column(valueBoxOutput("povertyExitBox", width = NULL),width=2)
      #   
      # )),
      fluidRow(sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput(
            "resStat",
            label = "Select an indicator:",
            choices = unique(resilience$Statistics),
            selected = "Average poverty gap"
          ),
          selectInput(
            "resYear",
            label = "Year of interest:",
            choices = seq(as.numeric(max(resilience$Year[resilience$Statistics == "Average poverty gap"])), 2006,-1),
            selected = max(resilience$Year[resilience$Statistics == "Average poverty gap"])
          ),
          selectInput(
            "resGeo",
            label = "Geography:",
            choices = unique(resilience$GEO[resilience$Statistics == "Average poverty gap"]),
            selected = "Canada"
          ),
          selectInput(
            "resAge",
            label = "Age group:",
            choices = unique(resilience$Age.group[resilience$Statistics == "Average poverty gap"]),
            selected = "All age groups"
          ),
          selectInput(
            "resSex",
            label = "Sex:",
            choices = unique(resilience$Sex[resilience$Statistics == "Average poverty gap"]),
            selected = "Both sexes"
          ),
          actionButton("updateRes",
                       label = "Apply changes",
                       width = "100%",
                       class = "btn btn-primary btn-custom")
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
                label = NULL,
                choices = c("No by variable" = "Statistics",
                            "By age group" = "Age.group",
                            "By sex" = "Sex"),
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
              ),
              p("Note that cross-tabulations of sex and age group are not available for low income entry and exit rates.")
              ))
              )
              )
              ))
          )
      )
      )

byVarVec <- c("None" = "Statistics",
                      "Age group" = "Age.group",
                      "Sex" = "Sex")

server <- function(input, output, session) {
  
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
                               colour = names(byVarVec)[byVarVec == input$officialByVar],
                               title=paste("Official poverty rate by year")) 
      
      girafe(ggobj = officialYearsPlot, width_svg = 9, height_svg = 4)
    
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
    
    girafe(ggobj = officialSexPlot, height_svg = 5, width_svg = 5)
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
    
    girafe(ggobj = officialAgePlot, height_svg = 5, width_svg = 5)

  })
  
  
  # change drop down menus based on the statistic chosen
  observe({
    # resilience and security page
    statRes <- input$resStat
    
    updateSelectInput(session, "resAge",
                      label = "Age group:",
                      choices = unique(resilience$Age.group[resilience$Statistics == statRes]))
    
    # update years for average poverty gap and hourly wage
    if (statRes %in% c("Average poverty gap", "Median hourly wage")) {
      updateSelectInput(session, "resYear",
                        label = "Year of interest:",
                        choices = seq(as.numeric(max(resilience$Year[resilience$Statistics == statRes])), 2006,-1),
                        selected = max(resilience$Year[resilience$Statistics == statRes]))
    }
    
    # updates for entry and exit rates
    if (statRes %in% c("Low income entry rate", "Low income exit rate")) {
      updateSelectInput(session, "resYear",
                        label = "Year of interest:",
                        choices = rev(unique(resilience$Year[resilience$Statistics == statRes])),
                        selected = rev(unique(resilience$Year[resilience$Statistics == statRes]))[1])
    }
    
    # opportunity and inclusion page
    statOpp <- input$oppStat
    
    updateSelectInput(session, "oppAge",
                      label = "Age group:",
                      choices = unique(opportunity$Age.group[opportunity$Statistics == statOpp]))
    
    updateSelectInput(session, "oppSex",
                      label = "Sex:",
                      choices = unique(opportunity$Sex[opportunity$Statistics == statOpp]))
    
    updateSelectInput(session, "oppYear",
                      label = "Year of interest:",
                      choices = seq(max(opportunity$Year[opportunity$Statistics == statOpp]), 2006,-1),
                      selected = max(opportunity$Year[opportunity$Statistics == statOpp]))
    
    if (statOpp == "Bottom 40% income share") {
      updateSelectInput(session, "oppByVar",
                        label = NULL,
                        choices = c("No by variable" = "Statistics"))
    }
    else {
      updateSelectInput(session, "oppByVar",
                        label = NULL,
                        choices = c("No by variable" = "Statistics",
                                    "By age group" = "Age.group",
                                    "By sex" = "Sex"),
                        selected = "Statistics")
    }
    
   
    
  })

  ######### opportunity and inclusion page
  output$oppMapOut <- renderGirafe({
    input$updateOpp
    isolate({oppMapData <- subset(opportunity, Statistics == input$oppStat &
                                    Year == input$oppYear &
                                    Sex == input$oppSex &
                                    Age.group == input$oppAge &
                                    GEO != "Canada")
    
    oppMapData$GEO <- as.character(oppMapData$GEO)
    
    oppMapData <- dplyr::left_join(mapcan(boundaries = province, type = standard),
                                   oppMapData,
                                   by = c("pr_alpha" = "GEO"))
    
    oppMapPlot <- ggplot() +
      geom_polygon_interactive(data=oppMapData, 
                               aes(x = long, y = lat, group = group, 
                                   fill=VALUE, tooltip=paste(pr_alpha, VALUE))) +
      coord_fixed() +
      theme(plot.margin=grid::unit(c(0,0,0,0), "mm"), legend.position = "bottom", axis.title = element_blank(),
            axis.text = element_blank(), axis.ticks = element_blank(), panel.spacing = element_blank(),
            legend.box.spacing = element_blank(), panel.border = element_blank(), panel.background = element_blank(),
            text=element_text(family="Roboto")) +
      scale_fill_gradient(low="#F5F5F5", high="#E24E42", name=input$oppStat, labels=label_number(accuracy=0.1), n.breaks=4)
    
    girafe(ggobj = oppMapPlot, width_svg = 5)})
    
  })
  
  output$oppYearsOut <- renderGirafe({
    (input$updateOpp | as.integer(as.factor(input$oppByVar)))
    isolate({oppYearsData <- if (input$oppByVar == "Statistics") subset(opportunity, Statistics == input$oppStat &
                                                                          Sex == input$oppSex &
                                                                          Age.group == input$oppAge &
                                                                          GEO == input$oppGeo)
    else if (input$oppByVar == "Age.group") subset(opportunity, Statistics == input$oppStat &
                                                     Sex == input$oppSex &
                                                     !(Age.group %in% c("All age groups", "16 years and over")) &
                                                     GEO == input$oppGeo)
    else if (input$oppByVar =="Sex") subset(opportunity, Statistics == input$oppStat &
                                              Age.group == input$oppAge &
                                              Sex != "Both sexes" &
                                              GEO == input$oppGeo)
    
    oppYearsPlot <- ggplot(oppYearsData) +
      geom_line(aes(x=Year, y=VALUE, colour=get(input$oppByVar), group=get(input$oppByVar)), size=1.5) +
      geom_point_interactive(aes(x=Year, y=VALUE, tooltip=paste0(Year, ": ", VALUE),
                                 colour=get(input$oppByVar), group=get(input$oppByVar)), size = 2.5) +
      scale_colour_canva(palette = "Bold and punchy") +
      theme_classic() + theme(text=element_text(family="Roboto"), plot.margin=grid::unit(c(0,0,0,0), "mm"),legend.position = "bottom") +
      {if (input$oppByVar != "Statistics") guides(colour = "legend") else guides(colour=F)} +
      labs(y=input$oppStat,
           x="Year",
           colour = names(byVarVec)[byVarVec == input$oppByVar],
           title=paste0(input$oppStat, " by year"))
    
    girafe(ggobj = oppYearsPlot, width_svg = 9, height_svg = 4)})
    
  })
  
  oppSexData <- reactive ({opportunity$highlight <- ifelse((opportunity$Sex == input$oppSex), 1, ifelse((input$oppSex == "Both sexes"),1,0))
  return(subset(opportunity,
                GEO == input$oppGeo &
                  Sex != "Both sexes" &
                  Age.group == input$oppAge &
                  Year == input$oppYear &
                  Statistics == input$oppStat))})
  
  output$oppSexOut <- renderGirafe({
    input$updateOpp
    isolate({oppSexPlot <- ggplot(oppSexData()) +
      geom_col_interactive(aes(x=Sex, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE))) +
      theme_classic() + scale_fill_canva(palette = "Bold and punchy") + theme(text=element_text(family="Roboto")) +
      scale_alpha(range = c(max(0.45, min(oppSexData()$highlight)),1)) +
      guides(alpha = FALSE, fill=FALSE) +
      labs(y=input$oppStat,
           x="Sex",
           title=paste0(input$oppStat," by sex, ",input$oppYear))
    
    girafe(ggobj = oppSexPlot, height_svg = 5, width_svg = 5)})
  })
  
  oppAgeData <- reactive ({opportunity$highlight <- ifelse((opportunity$Age.group == input$oppAge), 1, 
                                                          ifelse((input$oppAge %in% c("All age groups", "16 years and over")),1,0))
  return(subset(opportunity,
                GEO == input$oppGeo &
                  Sex == input$oppSex &
                  !(Age.group %in% c("All age groups", "16 years and over")) &
                  Year == input$oppYear &
                  Statistics == input$oppStat))})
  
  output$oppAgeOut <- renderGirafe({
    input$updateOpp
    isolate({oppAgePlot <- ggplot(oppAgeData()) +
      geom_col_interactive(aes(x=Age.group, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE))) +
      theme_classic() + scale_fill_canva(palette = "Bold and punchy") + theme(text=element_text(family="Roboto")) +
      scale_alpha(range = c(max(0.45, min(oppAgeData()$highlight)),1)) +
      guides(alpha = FALSE, fill = FALSE) +
      labs(y=input$oppStat,
           x="Age group",
           title=paste0(input$oppStat," by age group, ",input$oppYear))
    
    girafe(ggobj = oppAgePlot, height_svg = 5, width_svg = 5)})
    
  })
  

  
  ######### resilience and security page
  
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
      value = h5(paste0(thisPeriod,"%")),
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
    input$updateRes
    isolate({resMapData <- subset(resilience, Statistics == input$resStat &
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
            scale_fill_gradient(low="#F5F5F5", high="#258039", name=input$resStat, labels=label_number(accuracy=0.1), n.breaks=4)
    
    girafe(ggobj = resMapPlot, width_svg = 5)})
    
  })
  
  output$resYearsOut <- renderGirafe({
    (input$updateRes | as.integer(as.factor(input$resByVar)))
    isolate({resYearsData <- if (input$resByVar == "Statistics") subset(resilience, Statistics == input$resStat &
                                                                 Sex == input$resSex &
                                                                 Age.group == input$resAge &
                                                                 GEO == input$resGeo)
    else if (input$resByVar == "Age.group") subset(resilience, Statistics == input$resStat &
                                                     Sex == input$resSex &
                                                     !(Age.group %in% c("All age groups", "15 years and over", "18 years and over")) &
                                                     GEO == input$resGeo)
    else if (input$resByVar =="Sex") subset(resilience, Statistics == input$resStat &
                                              Age.group == input$resAge &
                                              Sex != "Both sexes" &
                                              GEO == input$resGeo)

    resYearsPlot <- ggplot(resYearsData) +
      geom_line(aes(x=Year, y=VALUE, colour=get(input$resByVar), group=get(input$resByVar)), size=1.5) +
      geom_point_interactive(aes(x=Year, y=VALUE, tooltip=paste0(Year, ": ", VALUE),
                                 colour=get(input$resByVar), group=get(input$resByVar)), size = 2.5) +
      scale_colour_canva(palette = "Cheerful brights") +
      theme_classic() + theme(text=element_text(family="Roboto"), plot.margin=grid::unit(c(0,0,0,0), "mm"),legend.position = "bottom") +
      {if (input$resByVar != "Statistics") guides(colour = "legend") else guides(colour=F)} +
      {if (input$resStat %in% c("Low income entry rate", "Low income exit rate")) 
        scale_x_discrete(breaks = unique(resilience$Year[resilience$Statistics == input$resStat])[c(TRUE, FALSE)]) else scale_x_discrete()} +
      labs(y=input$resStat,
           x="Year",
           colour = names(byVarVec)[byVarVec == input$resByVar],
           title=paste0(input$resStat, " by year"))

    girafe(ggobj = resYearsPlot, width_svg = 9, height_svg = 4)})

  })

  resSexData <- reactive ({resilience$highlight <- ifelse((resilience$Sex == input$resSex), 1, ifelse((input$resSex == "Both sexes"),1,0))
                            return(subset(resilience,
                                          GEO == input$resGeo &
                                            Sex != "Both sexes" &
                                            Age.group == input$resAge &
                                            Year == input$resYear &
                                            Statistics == input$resStat))})

  output$resSexOut <- renderGirafe({
    input$updateRes
    isolate({resSexPlot <- ggplot(resSexData()) +
      geom_col_interactive(aes(x=Sex, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE))) +
      theme_classic() + scale_fill_canva(palette = "Cheerful brights") + theme(text=element_text(family="Roboto")) +
      scale_alpha(range = c(max(0.45, min(resSexData()$highlight)),1)) +
      guides(alpha = FALSE, fill=FALSE) +
      labs(y=input$resStat,
           x="Sex",
           title=paste0(input$resStat," by sex, ",input$resYear))

    girafe(ggobj = resSexPlot, height_svg = 5, width_svg = 5)})
  })

  resAgeData <- reactive ({resilience$highlight <- ifelse((resilience$Age.group == input$resAge), 1, 
                                                          ifelse((input$resAge %in% c("All age groups", "15 years and over", "18 years and over")),1,0))
                          return(subset(resilience,
                                        GEO == input$resGeo &
                                          Sex == input$resSex &
                                          !(Age.group %in% c("All age groups", "15 years and over", "18 years and over")) &
                                          Year == input$resYear &
                                          Statistics == input$resStat))})

  output$resAgeOut <- renderGirafe({
    input$updateRes
    isolate({resAgePlot <- ggplot(resAgeData()) +
      geom_col_interactive(aes(x=Age.group, y=VALUE, alpha = highlight, fill=Statistics, tooltip=paste0(VALUE))) +
      theme_classic() + scale_fill_canva(palette = "Cheerful brights") + theme(text=element_text(family="Roboto")) +
      scale_alpha(range = c(max(0.45, min(resAgeData()$highlight)),1)) +
      guides(alpha = FALSE, fill = FALSE) +
      labs(y=input$resStat,
           x="Age group",
           title=paste0(input$resStat," by age group, ",input$resYear))

    girafe(ggobj = resAgePlot, height_svg = 5, width_svg = 5)})

  })
  
  
}

shinyApp(ui, server)



