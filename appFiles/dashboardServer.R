results = reactiveValues(
  dataframeTotal = NULL,
  dfDaily = NULL,
  newCases = NULL,
  dataframeTotalOldCases = NULL,
  newCasesDeath = NULL,
  dataframeFinal = NULL,
  newCasesRecovered = NULL,
  dataframeOldCases = NULL,
  modelFit = NULL,
  resultTable = NULL,
  df_covid_ward =NULL,
  totalCovid =NULL,
  regularWardCases =NULL,
  semiIcuCases =NULL,
  icuCases = NULL,
  dfWardTotal =NULL
)


my_css <- "

#descriptionreg {
color: darkred; font-size: 18px; font-style: bold;}

#descriptionbox {
color: darkred; font-size: 18px; font-style: bold;}

#description {
color: darkred; font-size: 18px; font-style: bold;}

#descriptiontukey {
color: darkred; font-size: 18px; font-style: bold;}
"

output$dashboard = renderUI({
  argonTabSet(
    id = "analysisSettingsTabs",
    card_wrapper = T,
    horizontal = TRUE,
    circle = F,
    size = "sm",
    width = 12,
    iconList = list(
      icon("home"),
      icon("tachometer-alt"), 
      icon("laptop-code"), 
      icon("chart-line"),
      icon("twitter")
    ),
    argonTab(
      tabName = "Home",
      active = T,
      argonRow(
        argonColumn(
          width = 4,
          img(src = 'covid.jpg',width = "100%"),
          # h6("Source: Wikipedia",style = 'text-align:center;
          #    font-style: italic;font-weight: bold;
          #    ')
          ),
        argonColumn(
          width = 5,
          p("A new invisible enemy, only 30kb in size, has emerged and is on a killing spree around the world: 2019-nCoV, the Novel Coronavirus!",style = 'text-align:justify;'),
          p("In right we can see some precautionary measures to prevent spread of Coronavirus.",style = 'text-align:justify;'),
          tags$br(),
          p("This monitor was developed to make the data and key visualizations of COVID-19 trends available to everyone and also provide a visualization platform for clinical spectrum",style = 'text-align:justify;')
        ),
        argonColumn(
          width = 3,
          img(src = 'covidGif.gif',width = "100%",height = "80%"),
          # h6("Source: Giphy",style = 'text-align:center;font-style: italic;font-weight: bold;')
        )
        
          ),
      p("This monitor has 3 tabs: Dashboard, WorldMeter and Conclusion .Worldmeter allows user to view a complete picture of COVID-19 spread around the world. User can also click on any country in the map to view the numbers in that country. In Dashboard tab user can compare the distribution of COVID-19 in multiple wards of hospital. ",style = 'text-align:justify;'),
      tags$br(),
      h4("Important Note:",style = 'color:Red;font-size:15px;text-align:Left;'),
      p("1. The data used in this dashboard taken from Hospital Israelita Albert Einstein,at São Paulo, Brazil and WHO website.",style = 'color:Red;font-size:13px;text-align:Left;'),
      p(paste0("2. World Meter will be updated on daily basis at GMT 00:00. It could be a chance that daily numbers not match as per your local source but aggregate numbers will definitely match."),style = 'color:Red;font-size:13px;text-align:Left;'),
      p(paste0("3. Last update: ",lastUpdate),style = 'color:Red;font-size:13px;text-align:Left;')
      
      
    ),
    # analysis setting tab -----
    argonTab(
      tabName = "World_Meter",
      active = F,
      tags$head(tags$style(type = "text/css", "
             #loadmessage {
                           position: fixed;
                           top: 150px;
                           left: 50px;
                           width: 93%;
                           padding: 5px 0px 5px 0px;
                           text-align: center;
                           font-weight: bold;
                           font-size: 100%;
                           color: #000000;
                           background-color: #CCFF66;
                           z-index: 105;
}
  ")),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("cardUI") %>% withSpinner()
        )
      ),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          uiOutput("chartUI") %>% withSpinner()
        )
      ),
      conditionalPanel(condition = "$('html').hasClass('shiny-busy')",
                       tags$div("Loading Page!!! Please wait...",id = "loadmessage")),
      uiOutput("countryComparisionChartsUI") %>% withSpinner(),
      tags$hr(),
      argonRow(
        argonColumn(
          width = 12,
          dataTableOutput("dataTableCountryWise") %>% withSpinner()
          
        )
      )
    ),
  argonTab(
    tabName = "Dashboard",
    active = F,
    argonRow(
      argonColumn(
        width = 12,
        uiOutput("hospCardUI") %>% withSpinner()
      )
      
    ),
    
    tags$hr(),
    argonRow(
      argonColumn(
        width = 6,
        uiOutput("ageGroupCompChartsUI") %>% withSpinner()
      ),
      
      argonColumn(
        width = 6,
        uiOutput("ageQuantileCompChartsUI") %>% withSpinner()
      )
      
    ),
    # uiOutput("ageGroupCompChartsUI") %>% withSpinner(),
    # tags$hr(),
    # uiOutput("ageQuantileCompChartsUI") %>% withSpinner(),
    # tags$hr(),
    # uiOutput("countryComparisionChartsUI") %>% withSpinner(),
    tags$hr(),
    argonRow(
      # argonColumn(
      #   width = 12,
      #   dataTableOutput("dataTableCountryCompare") %>% withSpinner()
      #   
      # )
    )
  ),
  
  
  
  argonTab(
    tabName = "Analysis",
    active = F,
    tags$style("

#descriptionreg {
color: darkred; font-size: 18px; font-style: bold;}

#descriptionbox {
color: darkred; font-size: 18px; font-style: bold;}

#description {
color: darkred; font-size: 18px; font-style: bold;}

#descriptiontukey {
color: darkred; font-size: 18px; font-style: bold;}
"),
    # uiOutput("sentimentUI") %>% withSpinner()
    theme = shinytheme("sandstone"),
    
    # Add the CSS style to the Shiny app"
    
    titlePanel("Analysis of the Hospital Dataset"),
    
    argonRow(
        # Sidebar layout with a input and output definitions 
      argonColumn(
        width = 12,
        sidebarLayout(
          
          # Inputs
          sidebarPanel(
            
            h3("Plotting"),      # Third level header: Plotting
            
            # Select variable for y-axis
            selectInput(inputId = "y", 
                        label = "Y-axis:",
                        # choices = c("Reading" = "read", "Writing" ="write", "Math" = "math", "Science" = "science", "Social Studies" = "socst"), 
                        choices = c(choices_for_plot),
                        selected = "patient_age_quantile"),
            
            # Select variable for x-axis
            selectInput(inputId = "x", 
                        label = "Scatterplot X-axis:",
                        # choices = c("Reading" = "read", "Writing" ="write", "Math" = "math", "Science" = "science", "Social Studies" = "socst"), 
                        choices = c(choices_for_plot),
                        selected = "patient_age_quantile"),
            
            # Select variable for w-axis Boxplot
            selectInput(inputId = "w", 
                        label = "Boxplot X-axis:",
                        # choices = c("Gender" = "gender", "Race" = "race", "Socioeconomic Status" = "ses", "School Type" = "schtyp", "Program" = "prog"), 
                        choices = c("Admission_Ward" = "target", "Age_group" = "age_group", "patient_age_quantile" = "patient_age_quantile"),
                        selected = "target"),
            
            # Select variable for color
            selectInput(inputId = "z", 
                        label = "Color by:",
                        # choices = c("Gender" = "gender", "Race" = "race", "Socioeconomic Status" = "ses", "School Type" = "schtyp", "Program" = "prog"),
                        choices = c("Admission_Ward" = "target", "Age_group" = "age_group", "patient_age_quantile" = "patient_age_quantile"),
                        selected = "age_group"),
            
            # Add checkbox for best fit line
            checkboxInput("fit", "Add best fit line to Scatterplot", TRUE),
            
            # Set alpha level
            sliderInput(inputId = "alpha", 
                        label = "Alpha:", 
                        min = 0, max = 1, 
                        value = 1),
            
            # Set point size
            sliderInput(inputId = "size", 
                        label = "Size:", 
                        min = 0, max = 5, 
                        value = 1),
            
            # Bin width for Histogram
            sliderInput(inputId = "binsin",
                        label = "Number of bins for histogram:",
                        min = 1,
                        max = 50,
                        value = 20),
            
            
            
          ),
          
          # Outputs
          mainPanel(
            tabsetPanel(type="tabs",
                        
                        #Tab 1 Plots
                        tabPanel(title="Plots",
                                 h3("Boxplot"),    # Third level header: Boxplot
                                 plotOutput(outputId = "boxplot"),
                                 textOutput(outputId = "descriptionbox"),
                                 br(),                 # Single line break for a little bit of visual separation
                                 h3("Scatterplot"),    # Third level header: Scatterplot
                                 plotlyOutput(outputId = "scatterplot"),
                                 textOutput(outputId = "description"),
                                 br(),                 # Single line break for a little bit of visual separation
                                 br(),                 # Single line break for a little bit of visual separation
                                 br()),                 # Single line break for a little bit of visual separation),
                        
                        #Tab 2 Histograms
                        tabPanel(title="Histograms",
                                 h3("Histogram for selected X value"),    # Third level header: Densityplot
                                 plotlyOutput(outputId = "histogramplot"),
                                 br(),                 # Single line break for a little bit of visual separation
                                 h3("Histogram for selected Y value"),    # Third level header: Scatterplot
                                 plotlyOutput(outputId = "histogramplottwo")),
                        
                        # #Tab 3 Regression
                        # tabPanel(title="Regression",
                        #          h3("Summary Statistics and Simple Linear Regression"),    # Third level header: Regression
                        #          h4("Using Scatterplot Y and X axis."),    # Fourth level header: Regression
                        #          textOutput(outputId = "correlation"),
                        #          htmlOutput(outputId = "avgs"),
                        #          textOutput(outputId = "descriptionreg"),
                        #          verbatimTextOutput(outputId = "lmoutput"),
                        #          h3("Regression Plots"),    # Third level header: Regression Plots
                        #          plotlyOutput(outputId = "regline"),
                        #          plotOutput(outputId = "regplots")),
                        
                        #Tab 4 Tukey Test
                        tabPanel(title="Tukey",
                                 h3("Tukey's HSD Test"), # Third level header: Tukey
                                 verbatimTextOutput(outputId = "tukeyoutput"),
                                 textOutput(outputId = "descriptiontukey")),
                        
                        #Tab 5 Data
                        tabPanel(title="Data Table",
                                 h3("Data table"),     # Third level header: Data table
                                 h5("Click the button Show (on left) to see the data by your specified number of rows"),     # Fifth level header
                                 DT::dataTableOutput("datatable"))
                        
            )
          )
        )
      
    ))

  ),
  argonTab(
    tabName = "Forecasting",
    active = F,
    uiOutput("forecastUI") %>% withSpinner()
  )

  )
})
outputOptions(output, "dashboard", suspendWhenHidden = FALSE)

########################################################################

# Create boxplot object the plotOutput function is expecting
output$boxplot <- renderPlot({
  ggplot(data = covid, aes_string(x = input$w, y = input$y, color=input$z)) +
    geom_boxplot(fill="peachpuff")+theme_minimal()
})

# Create descriptive text
output$descriptionbox <- renderText({
  paste0("The plot above visualizes the relationship between ", 
         input$w, " and ", input$y, ", conditional on ", input$z, ".")
})

# Create scatterplot object the plotlyOutput function is expecting
output$scatterplot <- renderPlotly({
  ggplotly({
    p <- ggplot(data = covid, aes_string(x = input$x, y = input$y, color=input$z)) +
      geom_point(alpha = input$alpha, size = input$size)+theme_minimal()
    
    #if check box selected plot best fit line
    if (input$fit) {
      p <- p + geom_smooth(method = "lm")
    }
    p
    
  })
})

# Create descriptive text
output$description <- renderText({
  paste0("The plot above visualizes the relationship between ", 
         input$x, " and ", input$y, ", conditional on ", input$z, ".")
})

# Create histogram
output$histogramplot <- renderPlotly({
  ggplotly({
    ggplot(data = covid, aes_string(x = input$x)) +
      geom_histogram(bins=input$binsin, color="black", fill="seagreen1")+theme_minimal()
  })
})

# Create histogram2
output$histogramplottwo <- renderPlotly({
  ggplotly({
    ggplot(data = covid, aes_string(x = input$y)) +
      geom_histogram(bins=input$binsin, color="black", fill="plum1")+theme_minimal()
  }) 
})

# Create text output stating the correlation between the two ploted 
output$correlation <- renderText({
  r <- round(cor(covid[, input$x], hsb2[, input$y], use = "pairwise"), 3)
  paste0("Correlation between ",input$x, " and ", input$y, " = ", r, ".")
})

# Calculate averages
output$avgs <- renderUI({
  avg_x <- covid %>% pull(input$x) %>% mean() %>% round(2)
  avg_y <- covid %>% pull(input$y) %>% mean() %>% round(2)
  HTML(
    paste("Average", input$x, "=", avg_x),
    "<br/>",
    paste("Average", input$y, "=", avg_y)
  )
})

# Create descriptive text
output$descriptionreg <- renderText({
  paste0("The regression below is between the dependent variable ", 
         input$y, " and the independent variable ", input$x, "")
})  

# Create regression output
output$lmoutput <- renderPrint({
  x <- covid %>% pull(input$x)
  y <- covid %>% pull(input$y)
  print(summary(lm(y ~ x, data = covid)), digits = 3, signif.stars = FALSE)
})

# Create regression line
output$regline <- renderPlotly({
  ggplotly({
    ggplot(data = covid, aes_string(x = input$x, y = input$y)) +
      geom_point(alpha = input$alpha, size = input$size, color="red")+
      geom_smooth(method='lm')+theme_minimal()
  })
})

# Create regression plots
output$regplots <- renderPlot({
  x <- covid %>% pull(input$x)
  y <- covid %>% pull(input$y)
  par(mfrow=c(2,2))
  plot(lm(y ~ x, data = covid))
  
})

# Create Tukey output
output$tukeyoutput <- renderPrint({
  y <- covid %>% pull(input$y)
  w <- covid %>% pull(input$w)
  print(TukeyHSD(aov(y ~ w, data = covid)))
})

# Create descriptive text
output$descriptiontukey <- renderText({
  paste0("The Tukey test helps determine if there's a statistically significant
           difference in ", input$y, " scores among the category ", input$w, "")
})

# Output data table
output$datatable <- DT::renderDataTable({
  covid
  
})



##############################################################################################
output$confirmedCount <- renderCountup({
  results$dataframeFinal = coronavirus
  dataframeTotal <- coronavirus %>% 
    dplyr::group_by(countryName) %>%
    slice(n()) %>%
    ungroup() %>%
    dplyr::mutate(Unrecovered = Confirmed - ifelse(is.na(Recovered), 0, Recovered) - ifelse(is.na(Deaths), 0, Deaths)) %>%
    dplyr::arrange(-Confirmed) %>%
    dplyr::ungroup() %>%
    select(-c(date,region,lat,lon))
  # browser()
  results$dataframeTotal = dataframeTotal
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Confirmed ")
  countup(
    totalConfirmed,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$activeCount <- renderCountup({
  totalUnrecovered = sum(results$dataframeTotal$Unrecovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Active ",
               suffix = paste0(" (",activeCasesPer,"%)")
  )
  countup(
    totalUnrecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$recoveredCount <- renderCountup({
  totalRecovered = sum(results$dataframeTotal$Recovered,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Recovered ",
               suffix = paste0(" (",totalRecoveredPer,"%)")
               )
  countup(
    totalRecovered,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$deathCount <- renderCountup({
  totalDeath = sum(results$dataframeTotal$Deaths,na.rm = T)
  totalConfirmed = sum(results$dataframeTotal$Confirmed,na.rm = T)
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Deaths ",
               suffix = paste0(" (",totalDeathPer,"%)"))
  countup(
    totalDeath,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})
output$countryCount <- renderCountup({
  x = results$dataframeTotal %>%
      filter(Confirmed > 0) %>%
      select(countryName) %>%
      unique() %>%                                                                              nrow()
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Total countries affected: "
              )
  countup(
    x,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

####################################
output$totalCount <- renderCountup({
  x = 5644
    opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Total patients data available: "
  )
  countup(
    x,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$covidCount <- renderCountup({
  results$df_covid_ward = covid_ward
  dfWardTotal <- covid_ward %>%
    group_by(age_group) %>%
    summarise(semi_icu = sum(semi_icu),regular_ward=sum(regular_ward),icu=sum(icu),covid_positive = sum(covid_positive))%>%
    mutate(not_admitted = covid_positive-semi_icu-regular_ward-icu)%>%
    arrange(-covid_positive)%>%
    ungroup()
  # browser()
  results$dfWardTotal = dfWardTotal
  totalCovidConfirmed = sum(results$dfWardTotal$covid_positive,na.rm = T)
  
  activeCasesPer = round(((totalCovidConfirmed/5644)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Covid Confirmed Patients :  ",
               suffix = paste0(" (",activeCasesPer,"% of Total patient) "))
  
  countup(
    totalCovidConfirmed,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$regularCount <- renderCountup({
  totalRegular = sum(results$dfWardTotal$regular_ward,na.rm = T)
  totalCovid = sum(results$dfWardTotal$covid_positive,na.rm = T)
  activeCasesPer = round(((totalRegular/totalCovid)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Covid Patients in regular ward : ",
               suffix = paste0(" (",activeCasesPer,"%)")
  )
  countup(
    totalRegular,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$semiIcuCount <- renderCountup({
  totalSemiIcu= sum(results$dfWardTotal$semi_icu,na.rm = T)
  totalConfirmed = sum(results$dfWardTotal$covid_positive,na.rm = T)
  totalRecoveredPer = round(((totalSemiIcu/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Covid Patients in Semi ICU ward :  ",
               suffix = paste0(" (",totalRecoveredPer,"%)")
  )
  countup(
    totalSemiIcu,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

output$icuCount <- renderCountup({
  totalIcu = sum(results$dfWardTotal$icu,na.rm = T)
  totalConfirmed = sum(results$dfWardTotal$covid_positive,na.rm = T)
  totalDeathPer = round(((totalIcu/totalConfirmed)*100),1)
  opts <- list(useEasing = TRUE,
               useGrouping = TRUE,
               prefix = "Covid Patients in ICU ward :   ",
               suffix = paste0(" (",totalDeathPer,"%)"))
  countup(
    totalIcu,
    start_at = 0,
    options = opts,
    duration = 2,
    start = TRUE,
    width = "100%",
    height = NULL,
    elementId = NULL
  )
})

########################################

output$cardUI = renderUI({
  df_daily <- coronavirus %>% 
    dplyr::group_by(date) %>%
    dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                     totalRecovered = sum(Recovered,na.rm = TRUE),
                     totalDeaths = sum(Deaths,na.rm = T)
    ) %>%
    dplyr::arrange(date) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) 
  results$dfDaily = df_daily
  
  max_date <- as.Date(max(coronavirus$date)) 
  newCases = coronavirus %>% 
    dplyr::filter(date == max_date | date == max_date - 1) %>%
    dplyr::group_by(countryName) %>%
    mutate(ConfirmedNew = Confirmed - shift(Confirmed,1)) %>% 
    mutate(RecoveredNew = Recovered - shift(Recovered,1)) %>%
    mutate(DeathsNew = Deaths - shift(Deaths,1)) %>%
    slice(n()) %>%
    ungroup() %>%
    select(countryName,ConfirmedNew,RecoveredNew,DeathsNew)
  
  results$newCases = newCases
  dataframeTotalOldCases = coronavirus %>%
    dplyr::filter(date == max_date - 1) %>%
    dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths) %>%
    summarise(totalConfirmed = sum(Confirmed,na.rm = T),
              totalDeath = sum(Deaths,na.rm = T),
              totalRecovered = sum(Recovered,na.rm = T),
              totalUnrecovered = sum(Unrecovered,na.rm = T)
    )
  results$dataframeTotalOldCases = dataframeTotalOldCases
  dataframeOldCases = coronavirus %>%
    dplyr::filter(date == max_date - 1) %>%
    dplyr::mutate(Unrecovered = Confirmed - Recovered - Deaths)
  results$dataframeOldCases = dataframeOldCases
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        center = T,
        argonBadge(text = countupOutput("countryCount"), 
                   src = NULL, 
                   pill = T, 
                   status = "danger")
       )
      ),
      tags$br(),
    argonRow(
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("confirmedCount"),
          icon = icon("users"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "warning",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalConfirmed,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("activeCount"),
          icon = icon("hospital"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "info",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalUnrecovered,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("recoveredCount"),
          icon = icon("smile"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "success",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalRecovered,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("deathCount"),
          icon = icon("heartbeat"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "danger",
          gradient = T,
          width = 12
        ),
        h6(paste0("Yesterday: ",
                  prettyNum(results$dataframeTotalOldCases$totalDeath,big.mark = ",")
                  ), 
                  style = 'text-align:center;
                           font-size:15px;')
      )
    )
  )
})











#########################################################################

output$hospCardUI = renderUI({

  tagList(
    argonRow(
      argonColumn(
        width = 12,
        center = T,
        argonBadge(text = countupOutput("totalCount"), 
                   src = NULL, 
                   pill = T, 
                   status = "danger")
      )
    ),
    tags$br(),
    argonRow(
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("covidCount"),
          icon = icon("users"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "warning",
          gradient = T,
          width = 12
        )
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("regularCount"),
          icon = icon("hospital"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "info",
          gradient = T,
          width = 12
        )
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("semiIcuCount"),
          icon = icon("procedures"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "success",
          gradient = T,
          width = 12
        ),
        
      ),
      argonColumn(
        width = 3,
        argonInfoCard(
          value = countupOutput("icuCount"),
          icon = icon("heartbeat"),
          icon_background = "default",
          hover_lift = F,
          shadow = T,
          background_color = "danger",
          gradient = T,
          width = 12
        )
      )
    )
  )
})



###################################################################################################















output$chartUI = renderUI({
  tagList(
  argonRow(
    argonColumn(
      width = 6,
      argonRow(
        argonColumn(
          width = 3,
          tags$strong("Aggregate results")
        ),
        argonColumn(
          width = 1,
          dropdownButton(
            tagList(
              prettyRadioButtons(
                inputId = "aggregatePlotOptions",
                label = NULL,
                choices = setNames(c(1:5),c("Total Cases",
                                            "Cumulative Cases",
                                            paste0("Confirmed New Cases (",Sys.Date() - 1,")"),
                                            paste0("Deaths (",Sys.Date() - 1,")"),
                                            paste0("Recovered (",Sys.Date() - 1,")")
                                            )
                                   ),
                selected = "1",
                shape = c("round"),
                outline = T,
                fill = T,
                width = "100%"
              )
            ),
            status = "primary",
            size = "sm",
            circle = T,
            icon = icon("wrench"),
            right = T,
            margin = "10px",
            inputId = "aggregatePlotOptionsDropdown"
          ),
          bsPopover("aggregatePlotOptionsDropdown", title = NULL, content = "Click to specify the type of plot", placement = "left", trigger = "hover",
                    options = NULL)
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          div(
            id = "totalCasesPlotDiv",
            highchartOutput("totalCasesPlot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "cumulativePlotDiv",
            highchartOutput("cumulativePlot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "newCasesPlotDiv",
            highchartOutput("newCasesPlot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "newCasesDeathsPlotDiv",
            highchartOutput("newCasesDeathsPlot",width = "100%") %>% withSpinner()
          ),
          div(
            id = "newCasesRecoveredPlotDiv",
            highchartOutput("newCasesRecoveredPlot",width = "100%") %>% withSpinner()
          )
        )
      )
    ),
    argonColumn(
      width = 6,
      argonRow(
        argonColumn(
          width = 8,
          tags$strong("Click on a country to view country specific results")
        ),
        argonColumn(
          width = 1,
          dropdownButton(
            tagList(
              prettyRadioButtons(
                inputId = "highchartOption",
                label = NULL,
                choices = setNames(c(1:4),c("Total Cases","Recovered cases","Deaths","Active Cases")),
                selected = "1",
                shape = c("round"),
                outline = T,
                fill = T,
                width = "100%"
              )
            ),
            status = "primary",
            size = "sm",
            circle = T,
            icon = icon("wrench"),
            right = T,
            margin = "10px",
            inputId = "worldMapOption"
          ),
          bsPopover("worldMapOption", title = NULL, content = "Click to specify the outcome", placement = "left", trigger = "hover",
                    options = NULL)
        )
      ),
      highchartOutput("worldMap",width = "100%") %>% withSpinner()
    )
  ),
  tags$hr(),
  argonRow(

  )
  )

})

output$worldMap <- renderHighchart({
  req(!is.null(results$dataframeTotal))
  canvasClickFunction <- JS("function(event) {Shiny.setInputValue('canvasClicked', [event.point.name]);}")
  x = input$highchartOption %>% as.numeric()
  data = results$dataframeTotal 
  # %>% 
  #        filter(str_detect(tolower(countryName), pattern = paste(y,collapse = "|"))) 
  value = switch(x,"Confirmed","Recovered","Deaths","Unrecovered")
  colnames(data)[5] = "name"
  highchart(type = "map",width = "100%",height = "100%") %>%
    hc_add_series_map(map = worldgeojson, df = data, value = value, joinBy = "name") %>%
    hc_colorAxis(stops = color_stops(5)) %>%
    hc_tooltip(useHTML = TRUE,
               headerFormat = '',
               pointFormat = paste0('{point.name}: {point.',value,'} ')) %>%
    hc_exporting(enabled = TRUE,filename = value) %>% 
    hc_add_theme(hc_theme_ffx()) %>%
    hc_chart(zoomType = "xy") %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_plotOptions(series = list( 
      events = list(click = canvasClickFunction),allowPointSelect = T))
})

output$cumulativePlot = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily = results$dfDaily
  x = max(df_daily$totalConfirmed,df_daily$totalUnrecovered,df_daily$totalDeaths,df_daily$totalRecovered)
  y = nchar(x) - 1
  yLimit = x %>% round(-y)
  hc <- highchart() %>% 
    hc_subtitle(text = "Cumulative Cases",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cumulative Number of Cases")) %>%
    hc_add_series(name = "Confirmed",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "Active",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "Recovered", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "Death", data = df_daily$totalDeaths)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$totalCasesPlot = renderHighchart({
  x = results$dataframeTotal %>%
        select(-countryCode) %>%
        arrange(desc(Confirmed)) %>%
        .[1:10,]
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Total  Cases (Top 10 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName) %>%
    hc_yAxis(title = list(text = "Total Cases (Log scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Confirmed",data = x$Confirmed) %>% 
    hc_add_series(name = "Active",data = x$Unrecovered) %>% 
    hc_add_series(name = "Recovered", data = x$Recovered) %>% 
    hc_add_series(name = "Death", data = x$Deaths)
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesPlot = renderHighchart({
  x = results$newCases %>%
        select(countryName,ConfirmedNew) %>%
        arrange(-ConfirmedNew) %>%
        top_n(n = 25,wt = ConfirmedNew)
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "New  Cases (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName,title = list(text = "Countries")) %>%
    hc_yAxis(title = list(text = "New Cases")) %>%
    hc_add_series(name = "Cases:",
                  data = x$ConfirmedNew,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(shared = TRUE, borderWidth = 5,table = F)
})
output$newCasesDeathsPlot = renderHighchart({
  x = results$newCases %>%
        select(countryName,DeathsNew) %>%
        arrange(-DeathsNew) %>%
        top_n(n = 25,wt = DeathsNew)
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "New Deaths (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName) %>%
    hc_yAxis(title = list(text = "New Deaths")) %>%
    hc_add_series(name = "Cases:",
                  data = x$DeathsNew,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})
output$newCasesRecoveredPlot = renderHighchart({
  x = results$newCases %>%
        select(countryName,RecoveredNew) %>%
        arrange(-RecoveredNew) %>%
        top_n(n = 25,wt = RecoveredNew)
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Newly Recovered (Top 25 countries)",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$countryName) %>%
    hc_yAxis(title = list(text = "New Recovered")) %>%
    hc_add_series(name = "Cases:",
                  data = x$RecoveredNew,
                  showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$dataTableCountryWise = renderDataTable({
  req(!is.null(results$dataframeTotal))
  x = results$dataframeTotal %>%
        select(-countryCode) %>%
        arrange(desc(Confirmed)) %>%
        mutate(totalActivePer = Unrecovered/Confirmed) %>%
        mutate(totalRecoveredPer = Recovered/Confirmed) %>%
        mutate(totalDeathPer = Deaths/Confirmed) %>%
    select(Country = countryName, Confirmed = Confirmed, Active = Unrecovered,Recovered = Recovered,Deaths = Deaths,"Active (%)" = totalActivePer,"Recovered (%)" = totalRecoveredPer,"Deaths (%)" = totalDeathPer)
  results$resultTable = x
  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'Download'
                  )
                )
              
            )
            
  ) %>%
  formatPercentage('Active (%)',2) %>%
  formatPercentage('Recovered (%)',2) %>%
  formatPercentage('Deaths (%)',2) %>%
  formatStyle(
      'Active (%)',
      background = styleColorBar(x$'Active (%)', '#31bed4'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Recovered (%)',
      background = styleColorBar(x$'Recovered (%)', '#8bd431'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Deaths (%)',
      background = styleColorBar(x$'Deaths (%)', '#ff5757'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})

#### Country Specific tab

countrySpecificModal <- function(){
  countryName = input$canvasClicked
  x = results$dataframeTotal %>% 
          filter(countryName == input$canvasClicked)
  totalConfirmed = x$Confirmed
  totalConfirmedYesterday = results$dataframeOldCases %>%
                                filter(countryName == input$canvasClicked) %>%
                                .["Confirmed"]
  totalUnrecovered = x$Unrecovered
  activeCasesPer = round(((totalUnrecovered/totalConfirmed)*100),1)
  yesterdayActiveCountry = results$dataframeOldCases %>%
                                filter(countryName == input$canvasClicked) %>%
                                .["Unrecovered"]
  totalRecovered = x$Recovered
  totalRecoveredPer = round(((totalRecovered/totalConfirmed)*100),1)
  yesterdayRecoveredCountry = results$dataframeOldCases %>%
                                  filter(countryName == input$canvasClicked) %>%
                                  .["Recovered"]
  totalDeath = x$Deaths
  totalDeathPer = round(((totalDeath/totalConfirmed)*100),1)
  yesterdayDeathsCountry = results$dataframeOldCases %>%
                              filter(countryName == input$canvasClicked) %>%
                              .["Deaths"]
  modalDialog(
  tagList(
    argonRow(
      argonColumn(
        center = T,
        width = 12,
        h1(countryName)
      )
    ),
    argonRow(
      argonColumn(
        width = 3,
        argonBadge(text = paste0("Confirmed: ",prettyNum(totalConfirmed,big.mark = ",")), 
                   src = NULL, 
                   pill = T, 
                   status = "warning"),
          h6(paste0("Yesterday: ",
                    prettyNum(totalConfirmedYesterday,
                              big.mark = ",")
          ),
          style = 'text-align:center;
          font-size:15px;')
      ),
      argonColumn(
        width = 3,
        argonBadge(text = paste0("Active: ",prettyNum(totalUnrecovered,big.mark = ",")
                                 # " (",activeCasesPer,"%)"
                                 ), 
                   src = NULL, 
                   pill = T, 
                   status = "info"),
        h6(paste0("Yesterday: ",
                  prettyNum(yesterdayActiveCountry,
                            big.mark = ",")
        ),
        style = 'text-align:center;
        font-size:15px;')
        ),
      argonColumn(
        width = 3,
        argonBadge(text = paste0("Recovered: ",prettyNum(totalRecovered,big.mark = ",")
                                 # " (",totalRecoveredPer,"%)"
                                 ), 
                   src = NULL, 
                   pill = T, 
                   status = "success"),
        h6(paste0("Yesterday: ",
                  prettyNum(yesterdayRecoveredCountry,
                            big.mark = ",")
        ),
        style = 'text-align:center;
        font-size:15px;')
        ),
      argonColumn(
        width = 3,
        argonBadge(text = paste0("Deaths: ",prettyNum(totalDeath,big.mark = ",")
                                 # " (",totalRecoveredPer,"%)"
        ), 
        src = NULL, 
        pill = T, 
        status = "danger"),
        h6(paste0("Yesterday: ",
                  prettyNum(yesterdayDeathsCountry,
                            big.mark = ",")
        ),
        style = 'text-align:center;
        font-size:15px;')
        )
    ),
    tags$hr(),
    argonRow(
    argonColumn(
      width = 12,
      argonRow(
        argonColumn(
          width = 5,
          # tags$strong("Country specific plots"),
          dateRangeInput( inputId = "dateRange", 
                         label = NULL,
                         start = min(coronavirus$date),
                         end   = Sys.Date() - 1,
                         min = min(coronavirus$date),
                         max   = Sys.Date() - 1,
                         format = "dd-mm-yyyy",
                         width = "100%"
                         )
        ),
        argonColumn(
          width = 4,
          prettySwitch(inputId = "scaleCountry",
                       label = "Logarithmic Scale",
                       value = F,
                       status = "primary",
                       fill = T,
                       inline = T,
                       width = "100%"
                       )
        ),
        argonColumn(
          width = 1,
          offset = 1,
          dropdownButton(
            tagList(
              prettyRadioButtons(
                inputId = "countryPlotOptions",
                label = NULL,
                choices = setNames(c(1:4),c("Cumulative Cases",
                                            paste0("Confirmed New Cases (",Sys.Date() - 1,")"),
                                            paste0("Deaths (",Sys.Date() - 1,")"),
                                            paste0("Recovered (",Sys.Date() - 1,")")
                )
                ),
                selected = "1",
                shape = c("round"),
                outline = T,
                fill = T,
                width = "100%"
              )
            ),
            status = "primary",
            size = "sm",
            circle = T,
            icon = icon("wrench"),
            right = T,
            margin = "10px",
            inputId = "countryPlotOptionsDropdown"
          ),
          bsPopover("countryPlotOptionsDropdown", title = NULL, content = "Click to specify the type of plot", placement = "left", trigger = "hover",
                    options = NULL)
        )
      ),
      argonRow(
        argonColumn(
          width = 12,
          div(
            id = "cumulativeCountryPlotDiv",
            highchartOutput("cumulativeCountryPlot",width = "100%") %>% withSpinner()
          ),
          hidden(
            div(
            id = "newCasesCountryPlotDiv",
            highchartOutput("newCasesCountryPlot",width = "100%") %>% withSpinner()
           )
          ),
          hidden(
            div(
            id = "newCasesDeathsCountryPlotDiv",
            highchartOutput("newCasesDeathCountryPlot",width = "100%") %>% withSpinner()
          )
          ),
          hidden(
            div(
            id = "newCasesRecoveredCountryPlotDiv",
            highchartOutput("newCasesRecoveredCountryPlot",width = "100%") %>% withSpinner()
          )
          )
        )
      )
     )
    )
  ),
  title = NULL,
  size = "l", 
  align = "center",
  easyClose = TRUE,
  fade = T, 
  footer = NULL
)
}


output$cumulativeCountryPlot = renderHighchart({
  confirmed_color = "#172b4d"
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  df_daily <- coronavirus %>% 
                filter(countryName == input$canvasClicked) %>%
                dplyr::group_by(date) %>%
                dplyr::summarise(totalConfirmed = sum(Confirmed, na.rm = TRUE),
                                 totalRecovered = sum(Recovered,na.rm = TRUE),
                                 totalDeaths = sum(Deaths,na.rm = T)
                ) %>%
                dplyr::arrange(date) %>%
                dplyr::ungroup() %>%
                dplyr::mutate(totalUnrecovered = totalConfirmed - totalRecovered - totalDeaths) %>%
                filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Cumulative Cases in ",input$countrySelect),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = df_daily$date) %>%
    hc_yAxis(title = list(text = "Cumulative Number of Cases"),type = scale) %>%
    hc_add_series(name = "Confirmed",data = df_daily$totalConfirmed) %>% 
    hc_add_series(name = "Active",data = df_daily$totalUnrecovered) %>% 
    hc_add_series(name = "Recovered", data = df_daily$totalRecovered) %>% 
    hc_add_series(name = "Death", data = df_daily$totalDeaths)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
              select(date,countryName,Confirmed) %>%
              dplyr::filter(countryName == input$canvasClicked) %>%
              mutate(confirmedDaily = if_else(is.na(Confirmed - shift(Confirmed,1)),
                                      Confirmed,
                                      Confirmed - shift(Confirmed,1)
                                      )
                     
                     ) %>% 
              filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear") 
  x = newCases
  death_color <- "orange"
  hc <- highchart() %>% 
    hc_subtitle(text = "Confirmed cases",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "New Cases"),type = scale) %>%
    hc_add_series(name = "Countries",data = x$confirmedDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesDeathCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
                select(date,countryName,Deaths) %>%
                dplyr::filter(countryName == input$canvasClicked) %>%
                mutate(deathDaily = if_else(is.na(Deaths - shift(Deaths,1)),
                                            Deaths,
                                            Deaths - shift(Deaths,1)
                )
                
                ) %>% 
                filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  x = newCases
  death_color <- "red"
  hc <- highchart() %>% 
    hc_subtitle(text = "Deaths",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Deaths"), type = scale) %>%
    hc_add_series(name = "Countries",data = x$deathDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

output$newCasesRecoveredCountryPlot = renderHighchart({
  newCases = results$dataframeFinal %>% 
                select(date,countryName,Recovered) %>%
                dplyr::filter(countryName == input$canvasClicked) %>%
                mutate(recoveredDaily = if_else(is.na(Recovered - shift(Recovered,1)),
                                            Recovered,
                                            Recovered - shift(Recovered,1)
                )
                
                ) %>% 
                filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  scale = ifelse(input$scaleCountry,"logarithmic","linear")
  x = newCases
  death_color <- "green"
  hc <- highchart() %>% 
    hc_subtitle(text = "Recovered",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = x$date) %>%
    hc_yAxis(title = list(text = "Recovered"),type = scale) %>%
    hc_add_series(name = "Countries",data = x$recoveredDaily) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_colors(c(death_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

#### Country Comparision tab ----

output$countryComparisionChartsUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        selectInput(
          inputId = "countryCompareList",
          label = strong("Select all the countries you want to compare in graphs"),
          choices = results$dataframeTotal$countryName,
          selected = results$dataframeTotal$countryName[c(3)],
          selectize = T,
          multiple = T,
          width = "100%"
        )
      ),
      argonColumn(
        width = 3,
        dropdownButton(
          prettyRadioButtons(
            inputId = "countryCompareTypeofPlot",
            label = NULL,
            choices = setNames(c(1:5),c("Cumulative",
                                        "Confirmed cases",
                                        "Active cases",
                                        "Recovered cases",
                                        "Deaths")),
            selected = "1",
            shape = c("round"),
            outline = T,
            fill = T,
            width = "100%"
          ),
          label = "Outcome to be compared:",
          status = "primary",
          circle = F,
          icon = icon("wrench"),
          right = T,
          margin = "10px",
          inputId = "countryCompareTypeofPlotDropdown"
        )
      ),
      argonColumn(
        width = 4,
        dateRangeInput( inputId = "dateRangeCompare", 
                        label = NULL,
                        start = min(coronavirus$date),
                        end   = Sys.Date() - 1,
                        min = min(coronavirus$date),
                        max   = Sys.Date() - 1,
                        format = "dd-mm-yyyy",
                        width = "100%"
        )
      ),
      argonColumn(
        width = 2,
        offset = 3,
        prettySwitch(inputId = "scaleCountryCompare",
                     label = "Logarithmic Scale",
                     value = F,
                     status = "primary",
                     fill = T,
                     inline = T,
                     width = "100%"
        )
      )
    ),
    tags$hr(),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("countryCompareChart") %>% withSpinner()
      )
    )
  )
})

#######################################################################################################################################################

output$ageGroupCompChartsUI = renderUI({

    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("ageCompareChart") %>% withSpinner()
      )
    )
  
})

output$ageCompareChart = renderHighchart({
  d_covid_ward=covid_ward %>%
    group_by(age_group) %>%
    summarise(semi_icu = sum(semi_icu),regular_ward=sum(regular_ward),icu=sum(icu),covid_positive = sum(covid_positive))%>%
    mutate(not_admitted = covid_positive-semi_icu-regular_ward-icu)%>%
    arrange(-covid_positive)%>%
    ungroup()
  
  data = d_covid_ward 
  semi_icu_color = "#172b4d"
  regular_ward_color <- "#1f77b4"
  not_admitted_color <- "forestgreen"
  
  icu_color <- "red"
  
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Aggregate Result Based on Age Group"),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = data$age_group) %>%
    hc_yAxis(title = list(text = "Total Cases"),type = scale) %>%
    hc_add_series(name = "semi_icu",data = data$semi_icu) %>% 
    hc_add_series(name = "regular_ward",data = data$regular_ward) %>% 
    hc_add_series(name = "not_admitted", data = data$not_admitted) %>% 
    hc_add_series(name = "icu", data = data$icu)
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(semi_icu_color,regular_ward_color, not_admitted_color ,icu_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
})

#########################################################################################################################################################


####################################################################################################################


output$ageQuantileCompChartsUI = renderUI({
  
  argonRow(
    argonColumn(
      width = 12,
      highchartOutput("ageQuantileCompareChart") %>% withSpinner()
    )
  )
  
})

output$ageQuantileCompareChart = renderHighchart({
  quantile_covid_ward=covid_ward %>%
    group_by(patient_age_quantile) %>%
    summarise(semi_icu = sum(semi_icu),regular_ward=sum(regular_ward),icu=sum(icu),covid_positive = sum(covid_positive))%>%
    mutate(not_admitted = covid_positive-semi_icu-regular_ward-icu)%>%
    arrange(-patient_age_quantile)%>%
    ungroup()
  
  data = quantile_covid_ward 
  semi_icu_color = "#172b4d"
  regular_ward_color <- "#1f77b4"
  not_admitted_color <- "forestgreen"
  
  icu_color <- "red"
  
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Comparing Age Quantiles"),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = data$patient_age_quantile) %>%
    hc_yAxis(title = list(text = "Total Cases"),type = scale) %>%
    hc_add_series(name = "semi_icu",data = data$semi_icu) %>% 
    hc_add_series(name = "regular_ward",data = data$regular_ward) %>% 
    hc_add_series(name = "not_admitted", data = data$not_admitted) %>% 
    hc_add_series(name = "icu", data = data$icu)
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(semi_icu_color,regular_ward_color, not_admitted_color ,icu_color)) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
})



#####################################################################################################################################################################################


output$countryCompareChart = renderHighchart({
  req(!is.null(input$countryCompareList))
  countryList = input$countryCompareList
  scale = ifelse(input$scaleCountryCompare,"logarithmic","linear")
  dateRange = input$dateRangeCompare
  plotType = switch(as.numeric(input$countryCompareTypeofPlot),
                    "Cumulative",
                    "Confirmed",
                    "Active",
                    "Recovered",
                    "Deaths")
  if (plotType != "Cumulative") {
      data =  coronavirus %>%
                    filter(countryName %in% countryList) %>%
                    filter(date >= dateRange[1] & date <= dateRange[2]) %>%
                    mutate(Active = Confirmed - Recovered - Deaths) %>%
                    select(date,countryName,plotType)
      hc <- highchart() %>% 
              hc_subtitle(text = paste0("Comparing ",plotType," cases for ",length(countryList)," countries"),
                          align = "left",
                          style = list(color = "#2b908f", fontWeight = "bold")) %>%
              hc_xAxis(categories = data$date) %>%
              hc_yAxis(title = list(text = paste0(plotType," cases")),
                       type = scale)
      for (i in 1:length(countryList)) {
        hc = hc_add_series(hc,
                      name = countryList[i],
                      data = data %>%
                              filter(countryName == countryList[i]) %>%
                              .[,3]
                        )
            }
      hc %>% 
        hc_chart(borderColor = '#EBBA95',
                 borderRadius = 10,
                 borderWidth = 2
        ) %>%
        hc_exporting(
          enabled = TRUE
        ) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5,table = T)
  } else {
      data = results$dataframeTotal %>%
                  filter(countryName %in% countryList)
      confirmed_color = "#172b4d"
      active_color <- "#1f77b4"
      recovered_color <- "forestgreen"
      death_color <- "red"
      hc <- highchart() %>% 
        hc_subtitle(text = paste0("Comparing ",length(countryList)," countries"),
                    align = "left",
                    style = list(color = "#2b908f", fontWeight = "bold")) %>%
        hc_xAxis(categories = data$countryName) %>%
        hc_yAxis(title = list(text = "Total Cases"),type = scale) %>%
        hc_add_series(name = "Confirmed",data = data$Confirmed) %>% 
        hc_add_series(name = "Active",data = data$Unrecovered) %>% 
        hc_add_series(name = "Recovered", data = data$Recovered) %>% 
        hc_add_series(name = "Death", data = data$Deaths)
      hc %>% 
        hc_chart(type = "column") %>%
        hc_chart(borderColor = '#EBBA95',
                 borderRadius = 10,
                 borderWidth = 2
        ) %>%
        hc_exporting(
          enabled = TRUE
        ) %>%
        hc_colors(c(confirmed_color,active_color, recovered_color ,death_color)) %>%
        hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
                   shared = TRUE, borderWidth = 5,table = T)
  }
  
})

output$dataTableCountryCompare = renderDataTable({
  req(!is.null(input$countryCompareList))
  countryList = input$countryCompareList
  x = results$dataframeTotal %>%
        filter(countryName %in% countryList) %>%
        select(-countryCode) %>%
        arrange(desc(Confirmed)) %>%
        mutate(totalActivePer = Unrecovered/Confirmed) %>%
        mutate(totalRecoveredPer = Recovered/Confirmed) %>%
        mutate(totalDeathPer = Deaths/Confirmed) %>%
        select(Country = countryName, Confirmed = Confirmed, Active = Unrecovered,Recovered = Recovered,Deaths = Deaths,"Active (%)" = totalActivePer,"Recovered (%)" = totalRecoveredPer,"Deaths (%)" = totalDeathPer)
  datatable(x,
            extensions = 'Buttons',
            rownames = FALSE,
            filter = 'top',
            options = list(
              searchHighlight = TRUE,
              pageLength = 25,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons =
                list(
                  list(
                    extend = 'collection',
                    buttons = c('csv', 'pdf'),
                    text = 'Download'
                  )
                )
              
            )
            
  ) %>%
    formatPercentage('Active (%)',2) %>%
    formatPercentage('Recovered (%)',2) %>%
    formatPercentage('Deaths (%)',2) %>%
    formatStyle(
      'Active (%)',
      background = styleColorBar(x$'Active (%)', '#31bed4'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Recovered (%)',
      background = styleColorBar(x$'Recovered (%)', '#8bd431'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    ) %>%
    formatStyle(
      'Deaths (%)',
      background = styleColorBar(x$'Deaths (%)', '#ff5757'),
      backgroundSize = '100% 90%',
      backgroundRepeat = 'no-repeat',
      backgroundPosition = 'center'
    )
})

#### Forecast UI ----

output$forecastUI = renderUI({
  tagList(
    h3("Prediction of nCov-19 cases using Infectious Disease Modelling"),
    tags$br(),
    argonRow(
      argonColumn(
        width = 3,
        pickerInput(
          inputId = "countryForecastInput",
          label = strong("Select country:"),
          choices = results$dataframeTotal$countryName,
          selected = "India",
          width = "100%",
          options = list(`live-search` = TRUE),
          inline = F
        )
      ),
      argonColumn(
        width = 2,
        numericInput(
          inputId = "populationInput",
          label = strong("Population (N)"),
          value = 1380004385,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 2,
        numericInput(
          inputId = "fatalityInput",
          label = strong("Fatality rate (in %)"),
          value = 0.7,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 3,
        numericInput(
          inputId = "severeInput",
          label = strong("Severe cases (in %): Assumption"),
          value = 5,
          min = 0,
          width = "100%"
        )
      ),
      argonColumn(
        width = 2,
        dateInput( inputId = "dateForecast", 
                        label = strong("Forecast till which date:"),
                        value = Sys.Date() + 120,
                        min = Sys.Date(),
                        format = "dd-mm-yyyy",
                        width = "100%"
        )
      )
    ),
    tags$hr(),
    uiOutput("forecastBadge") %>% withSpinner(),
    tags$br(),
    argonRow(
      argonColumn(
        width = 5,
        highchartOutput("currentScenario") %>% withSpinner()
      ),
      argonColumn(
        width = 7,
        highchartOutput("forecastedScenario") %>% withSpinner()
      )
      
    )
  )
})

observeEvent(input$countryForecastInput,{
  value = population %>%
            filter(Country == input$countryForecastInput) %>%
            .[,2] %>%
            gsub(",", "",.) %>%
            as.numeric()
  valueDeath = results$resultTable %>%
                  filter(Country == input$countryForecastInput) %>%
                  .["Deaths (%)"] %>%
                  as.numeric() * 100 %>%
                  round(.,2)
  updateNumericInput(session,
                     inputId = "populationInput",
                     value = value)
  updateNumericInput(session,
                     inputId = "fatalityInput",
                     value = valueDeath)
})

output$currentScenario = renderHighchart({
  req(!is.null(coronavirus))
  data = coronavirus %>% 
            filter(countryName == input$countryForecastInput) %>%
            filter(Confirmed > 0) %>%
            select(date,Confirmed,Recovered)
  day = 1:(nrow(data))
  lmModel <- augment(lm(log10(data$Confirmed) ~ day, data = data))
  hc <- highchart() %>% 
           hc_subtitle(text = paste0("Cumulative Infected Cases in ",input$countryForecastInput),
                       align = "left",
                       style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = data$date) %>%
    hc_yAxis(title = list(text = "Infected Cases (Log-scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Actual",data = data$Confirmed,type = "scatter") %>% 
    hc_add_series(name = "Fitted",data = 10^lmModel$.fitted,type = "line")
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
  
})

output$forecastedScenario = renderHighchart({
  req(!is.null(coronavirus))
  req(!is.na(input$populationInput))
  active_color <- "#1f77b4"
  recovered_color <- "forestgreen"
  death_color <- "red"
  orange_color <- "#172b4d"
  data = coronavirus %>% 
          filter(countryName == input$countryForecastInput) %>%
          filter(Confirmed > 0) %>%
          select(date,Confirmed,Recovered)
  I = data$Confirmed[1]
  R = data$Recovered[1]
  N = input$populationInput
  Day = 1:length(data$Confirmed)
  SIR <- function(time, state, parameters) {
    par <- as.list(c(state, parameters))
    with(par, {
      dS <- -beta/N * I * S
      dI <- beta/N * I * S - gamma * I
      dR <- gamma * I
      list(c(dS, dI, dR))
    })
  }
  init <- c(S = N - I, I = I, R = R)
  RSS <- function(parameters) {
    names(parameters) <- c("beta", "gamma")
    out <- ode(y = init, times = Day, func = SIR, parms = parameters)
    fit <- out[ , 3]
    sum((data$Confirmed - fit)^2)
  }
  model = optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1),hessian = F)
  modelPar <- setNames(model$par, c("beta", "gamma"))
  t = 1:(input$dateForecast - as.Date(min(data$date)))
  fitValue <- data.frame(ode(y = init, times = t, func = SIR, parms = modelPar))
  fitValue = fitValue %>%
                mutate(date = as.Date(min(data$date)) + time)
  results$modelFit = list(params = modelPar,fitValue = fitValue)
  hc <- highchart() %>% 
    hc_subtitle(text = paste0("Predicted Infected Cases in ",input$countryForecastInput," by ",input$dateForecast),
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = fitValue$date) %>%
    hc_yAxis(title = list(text = "Predicted Cases (Log-scale)"),type = "logarithmic") %>%
    hc_add_series(name = "Susceptible",data = round(fitValue$S,0)) %>% 
    hc_add_series(name = "Infected",data = round(fitValue$I,0)) %>%
    hc_add_series(name = "Recovered",data = round(fitValue$R,0)) %>%
    hc_add_series(name = "Actual infected",data = data$Confirmed)
  
  hc %>% 
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_exporting(
      enabled = TRUE
    ) %>%
    hc_colors(c(active_color,death_color,recovered_color,orange_color)) %>%
    hc_tooltip(crosshairs = T, backgroundColor = "#FCFFC5",
               shared = T, borderWidth = 5,table = T)
  
})

output$forecastBadge = renderUI({
  req(!is.null(results$modelFit))
  req(!is.null(input$fatalityInput))
  req(!is.null(input$severeInput))
  r0 = results$modelFit$params[1] / results$modelFit$params[2]
  pandemicHeight = max(results$modelFit$fitValue$I)
  deathForecast = pandemicHeight * (input$fatalityInput/100)
  severeForecast = pandemicHeight * (input$severeInput/100)
  argonRow(
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Reproductive rate = ",round(r0,2)),
        pill = T,
        status = ifelse(as.numeric(r0) < 1,"success",
                        ifelse(as.numeric(r0) < 1.5,"warning",
                               "danger")
                        ),
        src = "https://nivedi.res.in/Nadres_v2/ro_material.php"
      )
    ),
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Pandemic height = ",prettyNum(round(pandemicHeight,0),big.mark = ",")),
        pill = T,
        status = "primary"
      )
    ),
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Severe cases = ",prettyNum(round(severeForecast,0),big.mark = ",")),
        pill = T,
        status = "warning"
      )
    ),
    argonColumn(
      width = 3,
      argonBadge(
        text = paste0("Deaths till height= ",prettyNum(round(deathForecast,0),big.mark = ",")),
        pill = T,
        status = "danger"
      )
    )
  )
})
#### Sentiment analysis ----

sentimentAnalResult = reactiveValues(
  run = 1,
  tweetText = NULL,
  tweetScores = NULL
)

output$sentimentUI = renderUI({
  tagList(
    argonRow(
      argonColumn(
        width = 12,
        center = T,
        h2("Sentiment analysis using social media data")
      )
    ),
    argonRow(
      argonColumn(
        width = 4,
        argonRow(
          argonColumn(
            width = 12,
            argonRow(
              center = T,
              actionBttn(
                inputId = "runSentiment",
                label = "Click to Run Sentiment Analysis",
                color = "warning",
                block = T
              )
            ),
            pickerInput(
              "twitterHashtag",
              label = strong("Please select the hashtags you want to include in sentiment analysis: "),
              choices = c("#coronavirus","#covid19","#Covid_19","#CoronaCrisis"),
              options = list(`actions-box` = TRUE,
                             `live-search` = TRUE
              ),
              multiple = T,
              selected = c("#coronavirus","#covid19","#Covid_19","#CoronaCrisis"),
              width = "100%",
              inline = F
            ),
            radioGroupButtons(inputId = "tweetsOption", 
                              label = strong("Specify the number of latest posts use for analysis (time take to run the analysis):"), 
                              choices = setNames(c(1:5),c("500 (<1 Minutes)","1000 (1 Minutes)","2000 (2 Minutes)","5000 (3 Minutes)","10000 (5 Minutes)")),
                              selected = "1",
                              justified = T,
                              width = "100%",
                              status = "primary",
                              direction = "vertical"
            )
          )
        )
      ),
      argonColumn(
        width = 8,
        plotOutput("workcloud",width = "100%",height = "500px") %>% withSpinner()
      )
    ),
    tags$hr(),
    argonRow(
      argonColumn(
        width = 12,
        highchartOutput("sentimentPlot") %>% withSpinner()
      )
    )
    
    
  )
})

observeEvent(input$runSentiment,{
  req(c(!is.null(input$tweetsOption),!is.null(input$twitterHashtag)))
  time = switch(input$tweetsOption,
                "1" = "<1 minute",
                "2" = "1 minute",
                "3" = "2 minutes",
                "4" = "3 minutes",
                "5" = "5 minutes"
  )
  tweetsN = switch(input$tweetsOption,
                "1" = 500,
                "2" = 1000,
                "3" = 2000,
                "4" = 5000,
                "5" = 10000
  )
    progressSweetAlert(session, id = "twitterProgress", 
                       value = 10, 
                       total = NULL,
                       display_pct = T, 
                       size = NULL, 
                       status = "success", 
                       striped = T,
                       title = paste0("Fetching data...","(Take approx ",time," to run)"))
    consumer_key <- 'lJPpmqW41pv6K6miqXmcXYemV'
    consumer_secret <- '01kHQaZqGhmboh7bAUBJIy1rqdqb7QLjan0RQgyNIyWwLOm2u1'
    access_token <- '1966387032-BDCHsUK08TbtmxOcFuFnKii1Jt94l72tn1Oz5lZ'
    access_secret <- 'Ry8n90UsUerTmcUnoNxBfVSnwR6fCmV4aC7j02JGQnPPq'
    setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
    tweets <- searchTwitter(paste0(input$twitterHashtag, collapse = " OR "),
                            n = tweetsN,
                            lang = "en")
    updateProgressBar(
      session = session,
      id = "twitterProgress",
      value = 50,
      title = "Processing data..."
    )
    tweetsDF <- twListToDF(tweets)
    tweetsText <- tweetsDF$text
    tweetsText <- tolower(tweetsText)
    tweetsText <- gsub("rt", "", tweetsText)
    tweetsText <- gsub("@\\w+", "", tweetsText)
    tweetsText <- gsub("[[:punct:]]", "", tweetsText)
    tweetsText <- gsub("http\\w+", "", tweetsText)
    tweetsText <- gsub("[ |\t]{2,}", "", tweetsText)
    tweetsText <- gsub("^ ", "", tweetsText)
    tweetsText <- gsub(" $", "", tweetsText)
    sentimentAnalResult$tweetText = tweetsText
    sample <- sample(tweetsText, (length(tweetsText)))
    corpus <- Corpus(VectorSource(list(sample)))
    corpus <- tm_map(corpus, removePunctuation) %>% suppressWarnings()
    corpus <- tm_map(corpus, content_transformer(tolower)) %>% suppressWarnings()
    corpus <- tm_map(corpus, removeNumbers) %>% suppressWarnings()
    corpus <- tm_map(corpus, stripWhitespace) %>% suppressWarnings()
    corpus <- tm_map(corpus, removeWords, stopwords('english')) %>% suppressWarnings()
    
    corpus <- tm_map(corpus, stemDocument)##obtain word stems
    sentimentAnalResult$corpus = corpus
    
    updateProgressBar(
      session = session,
      id = "twitterProgress",
      value = 90,
      title = "Generating scores.."
    )
    tweetsScores <- get_nrc_sentiment((sentimentAnalResult$tweetText))
    tweetsScores <- data.frame(colSums(tweetsScores[,]))
    names(tweetsScores) <- "Score"
    tweetsScores <- cbind("sentiment" = rownames(tweetsScores),tweetsScores)
    rownames(tweetsScores) <- NULL
    sentimentAnalResult$tweetScores = tweetsScores
    closeSweetAlert(session = session)
    sendSweetAlert(
      session = session,
      title = " Analysis completed !!!",
      type = "success"
    )
  
})

output$workcloud = renderPlot({
  req(!is.null(sentimentAnalResult$corpus))
  wordcloud(sentimentAnalResult$corpus,
            min.freq = 10,
            scale = c(5,0.5),
            colors = brewer.pal(8, "Dark2"),
            random.color = TRUE)
})

output$sentimentPlot = renderHighchart({
  req(!is.null(sentimentAnalResult$tweetScores))
  tweetsScores = sentimentAnalResult$tweetScores
  proper = function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))
  hc <- highchart() %>% 
    hc_subtitle(text = "Sentiments of people behind the posts on pandemic CORONAVIRUS",
                align = "left",
                style = list(color = "#2b908f", fontWeight = "bold")) %>%
    hc_xAxis(categories = proper(tweetsScores$sentiment),title = list(text = "Sentiments")) %>%
    hc_yAxis(title = list(text = "Score")) %>%
    hc_add_series(name = "Score",data = tweetsScores$Score,showInLegend = F) 
  
  hc %>% 
    hc_chart(type = "column") %>%
    hc_chart(borderColor = '#EBBA95',
             borderRadius = 10,
             borderWidth = 2
    ) %>%
    hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5",
               shared = TRUE, borderWidth = 5,table = T)
})

#### to check which countries are new
# 
# mapdata <- get_data_from_map(download_map_data("custom/world"))
# # # 
# results$dataframeTotal$countryName[!(results$dataframeTotal$countryName %in% mapdata$name)]
# 
# [1] "Cruise Ship"
# [2] "Holy See

observeEvent(input$aggregatePlotOptions,{
  x = as.numeric(input$aggregatePlotOptions)
  divlist = c("totalCasesPlotDiv","cumulativePlotDiv","newCasesPlotDiv","newCasesDeathsPlotDiv","newCasesRecoveredPlotDiv")
  hideAllBut(divlist,x)
})
observeEvent(input$countryPlotOptions,{
  x = as.numeric(input$countryPlotOptions)
  divlist = c("cumulativeCountryPlotDiv","newCasesCountryPlotDiv",
              "newCasesDeathsCountryPlotDiv","newCasesRecoveredCountryPlotDiv")
  hideAllBut(divlist,x)
})


observeEvent(input$canvasClicked,{
  showModal(countrySpecificModal())
})






##### debug server logic #####
output$runRCodeOutput = renderPrint({
  req(rcode())
  isolate({
    eval(parse(text = rcode()$text))
  })
})
rcode = reactiveVal()
observeEvent(input$runRCodeButton, {
  rcode(list("text" = input$runRCode, "type" = "runRCode", "rand" = runif(1)))
}, ignoreNULL = TRUE, ignoreInit = TRUE)