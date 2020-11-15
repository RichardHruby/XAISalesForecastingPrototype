rm(list = ls())
library(forecast)
library(tree.interpreter)
library(caret)
library(dygraphs)
library(shinycssloaders)
library(neighbr)
library(tidyverse)
library(DT)
library(formattable)
library(randomForest)
library(data.table)
library(ggplot2)
library(lime)
library(shinydashboard)
library(plotly())

# Read in train and test data

train_product = fread("train_data.csv")
test_product = fread("test_data.csv")


sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Bread Forecasting Dashboard",
    tabName = "dashboard",
    icon = icon("dashboard")
  ),
  menuItem(
    "Potential Extention",
    icon = icon("dashboard"),
    tabName = "Potential_Extention",
    badgeLabel = "beta",
    badgeColor = "green"
  )
))


body <- dashboardBody(tabItems(
  tabItem(
    tabName = "dashboard",
    h2("Welcome to your Personal Bread Sales Forecasting Dashboard"),
    fluidRow(
      column(
        width = 4,
        box(
          title = "Control Panel",
          width = NULL,
          solidHeader = TRUE,
          sliderInput(
            "BW",
            label = "Business weeks to predict",
            value = 1,
            min = 1,
            max = 4
          ),
          selectInput("forecast_choice", "Forecast:",
                      choices = list("AI", "Human")),
          checkboxInput("showgrid", label = "Show Grid", value = TRUE),
        ),
        conditionalPanel(condition = "input.forecast_choice == 'AI'",
                         valueBoxOutput(width = NULL, "improvementBox")),
        uiOutput(width = NULL, "MADbox")
        
      ),
      
      tabBox(
        side = "right",
        width = 8,
        selected = "Overview",
        tabPanel(
          "Overview",
          dygraphOutput("dygraph") %>%
            withSpinner(color = "#0dc5c1"),
          helpText("Click and drag to zoom in (double click to zoom back out).")
        ),
        tabPanel(
          "Predictions for the Next Business Week",
          plotlyOutput("plotlyBars") %>%
            withSpinner(color = "#0dc5c1")
        )
      )
    ),
    fluidRow(
      box(
        width = 9,
        title = "Example-Based Explanation",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        
        wellPanel(
          conditionalPanel(
            condition = "input.forecast_choice == 'AI'",
            uiOutput(width = NULL, "no_explanation2") %>%
              withSpinner(color = "#0dc5c1"),
          ),
          conditionalPanel(condition = "input.forecast_choice != 'AI'",
                           htmlOutput("no_explanation1")),
        ),
      ),
      box(
        title = "Make a Specific Prediction",
        status = "primary",
        width = 3,
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,
        dateInput(
          "date",
          "Date:",
          value = "2019-03-13",
          weekstart = 1,
          datesdisabled = c(seq(
            as.Date("2000-01-01"), by = "day", length.out = 7011
          ))
        ),
        strong("Week of Year (implied): "),
        textOutput("week_of_year_implied"),
        br(),
        strong("Day of Week (implied): "),
        textOutput("day_of_week_implied"),
        br(),
        sliderInput(
          "qlag7",
          label = "Sales one Week Ago",
          value = 25,
          min = 0,
          max = 45
        ),
        sliderInput(
          "qlag14",
          label = "Sales two Weeks Ago",
          value = 15,
          min = 0,
          max = 45
        ),
        sliderInput(
          "seasonal",
          label = "Seasonal Component",
          value = -4,
          min = -15.0,
          max = 15.0
        ),
        actionButton("go", "Go"),
        
      )
      
    )
  ),
  tabItem(
    tabName = "Potential_Extention",
    h2(
      "Extend the explanation interface further! Tailor it to your end user's needs."
    )
  )
))

ui <-
  dashboardPage(dashboardHeader(title = "Explanation Interface"),
                sidebar,
                body,
  )


server <- function(input, output) {
  predictedData <- reactive({
    predict_and_list_nbours(train_product, test_product, input$BW, 7, 77)
  })
  
  predictedDataSnaiv <- reactive({
    predict_snaive(train_product, test_product, input$BW)
  })
  
  predictedDataSinglePred <- eventReactive(input$go, {
    predict_and_list_nbours_single(
      train_product,
      test_product,
      input$date,
      input$qlag7,
      input$qlag14,
      input$seasonal,
      0,
      7,
      77
    )
    
  })
  
  reactive_date <- eventReactive(input$go, {
    input$date
  })
  
  output$plot <- renderPlot({
    hist(randomVals())
  })
  
  output$no_explanation1 <- renderText({
    "<b>No explanation Provided for your Human Forecast.</b>
        <br>Please switch to AI Forecast and select a date on the forecast horizon to be explained!"
  })
  
  output$no_explanation_past <- renderText({
    "<b>No explanation Provided for Past Dates.</b>
        <br>Please select a date on the forecast horizon to be explained!"
    
  })
  
  output$select_a_date <- renderText({
    paste(
      "<b>Please select a date on the forecast horizon to be explained, or make an individual prediction!</b>"
    )
    
  })
  
  output$week_of_year_implied <- renderText({
    paste("#", week(as.Date(input$date) - 2))
  })
  
  output$day_of_week_implied <- renderText({
    paste(strftime(input$date, format = "%A"))
  })
  
  output$improvementBox <- renderValueBox({
    valueBox(
      percent(1 - mean(predictedData()[[2]]) / mean(predictedDataSnaiv()[[2]])),
      "Performance Improvement over Human Forecast",
      icon = icon("stats", lib = "glyphicon"),
      color = "green"
    )
  })
  
  output$MADbox <- renderUI({
    condition1 = as.Date(input$dygraph_click$x) + 1 >= as.Date("2019-03-13")
    if (length(condition1) != 0) {
      if (condition1 == TRUE) {
        if (typeof(r$my_selector) == 'integer') {
          # The conditions for coloring the value box should not be hard coded. This below lines are just for demonstration.
          if (input$forecast_choice == "AI") {
            m = MAD(predictedDataSinglePred()[[1]],
                    strftime(as.Date(reactive_date()),
                             "%Y-%m-%d"),
                    2)[[1]]
            if (m > 3 && m < 6) {
              color = 'yellow'
              icon = ''
            } else if (m >= 6) {
              color = 'red'
              icon = 'thumbs-down'
            } else {
              color = 'green'
              icon = 'thumbs-up'
            }
            
            
            
            valueBox(
              paste(
                MAD(
                  predictedDataSinglePred()[[1]],
                  strftime(as.Date(reactive_date()),
                           "%Y-%m-%d"),
                  2
                ),
                "Units"
              ),
              paste(
                "Average Deviation from forecast on 2 most similar days to:",
                strftime(as.Date(reactive_date()),
                         "%b, %d, %Y")
              ),
              icon = icon(icon, lib = "glyphicon"),
              color = color,
              width = NULL
            )
            
            
          }
        } else {
          if (input$forecast_choice == "AI") {
            m = MAD(predictedData()[[1]],
                    strftime(as.Date(input$dygraph_click$x) + 1,
                             "%Y-%m-%d"),
                    2)[[1]]
            if (m > 3 && m < 6) {
              color = 'yellow'
              icon = ''
            } else if (m >= 6) {
              color = 'red'
              icon = 'thumbs-down'
            } else {
              color = 'green'
              icon = 'thumbs-up'
            }
            
            valueBox(
              paste(MAD(
                predictedData()[[1]],
                strftime(as.Date(input$dygraph_click$x) + 1,
                         "%Y-%m-%d"),
                2
              ), "Units"),
              paste(
                "Average Deviation from forecast on 2 most similar days to:",
                strftime(as.Date(input$dygraph_click$x) + 1,
                         "%b, %d, %Y")
              ),
              icon = icon(icon, lib = "glyphicon"),
              color = color,
              width = NULL
            )
          }
        }
      }
    }
    else if (typeof(r$my_selector) == 'integer') {
      m = MAD(predictedDataSinglePred()[[1]],
              strftime(as.Date(reactive_date()),
                       "%Y-%m-%d"),
              2)[[1]]
      if (m > 3 && m < 6) {
        color = 'yellow'
        icon = ''
      } else if (m >= 6) {
        color = 'red'
        icon = 'thumbs-down'
      } else {
        color = 'green'
        icon = 'thumbs-up'
      }
      valueBox(
        paste(MAD(
          predictedDataSinglePred()[[1]],
          strftime(as.Date(reactive_date()),
                   "%Y-%m-%d"),
          2
        ), "Units"),
        paste(
          "Average Deviation from forecast on 2 most similar days to:",
          strftime(as.Date(reactive_date()),
                   "%b, %d, %Y")
        ),
        icon = icon(icon, lib = "glyphicon"),
        color = "blue",
        width = NULL
      )
    }
  })
  
  output$plotlyBars <- renderPlotly({
    generatePlotly(predictedData()[[1]], input$BW)
  })
  
  output$dygraph <- renderDygraph({
    if (input$forecast_choice == "Human") {
      predicted = predictedDataSnaiv()[[1]]
      ts = predicted[, c("Date", "Q", "Forecast", "Upper", "Lower")]
      dygraph(ts, main = "Bread Sales and Human Forecast ",
              ylab = "Sales / Forecast") %>%
        dySeries("Q", label = "Sales", color = "black") %>%
        dySeries(c("Lower", "Forecast", "Upper"),
                 label = "Human Forecast",
                 color = "orange") %>%
        dyRangeSelector(dateWindow = c("2019-01-01", toString(as.Date(
          tail(ts$Date, 1), format = "%Y-%m-%d"
        )))) %>%
        dyOptions(drawGrid = input$showgrid)
      
    } else {
      predicted = predictedData()[[1]]
      ts = predicted[, c("Date", "Q", "Forecast", "Upper", "Lower")]
      dygraph(ts, main = "Bread Sales and AI Forecast ",
              ylab = "Sales / Forecast") %>%
        dySeries("Q", label = "Sales", color = "black") %>%
        dySeries(c("Lower", "Forecast", "Upper"),
                 label = "AI Forecast",
                 color = "blue") %>%
        dyRangeSelector(dateWindow = c("2019-01-01", toString(as.Date(
          tail(ts$Date, 1), format = "%Y-%m-%d"
        )))) %>%
        dyOptions(drawGrid = input$showgrid)
    }
  })
  
  output$clicked <- renderText({
    if (input$go) {
      paste(strftime(as.Date(reactive_date()), "%b, %d, %Y"),
            "via the Make an Individual Prediction panel.")
    } else {
      paste(strftime(as.Date(input$dygraph_click$x) + 1, "%b, %d, %Y"),
            "via the Overview chart")
    }
    
  })
  
  output$tbl_instance <-
    DT::renderDT({
      if (typeof(r$my_selector) == 'integer') {
        return(produce_instance_tbl(predictedDataSinglePred()[[1]], reactive_date()))
        
      } else {
        condition = as.Date(input$dygraph_click$x) + 1 >= as.Date("2019-03-13")
        if (length(condition) != 0) {
          if (condition == TRUE) {
            produce_instance_tbl(predictedData()[[1]],
                                 strftime(as.Date(input$dygraph_click$x) + 1,
                                          "%Y-%m-%d"))
          }
        }
        
      }
    })
  
  output$tbl_nbours <-
    DT::renderDT({
      if (typeof(r$my_selector) == 'integer') {
        produce_nbour_tbl(predictedDataSinglePred()[[1]], reactive_date())
      }
      
      else {
        condition = as.Date(input$dygraph_click$x) + 1 >= as.Date("2019-03-13")
        if (length(condition) != 0) {
          if (condition == TRUE) {
            produce_nbour_tbl(predictedData()[[1]],
                              strftime(as.Date(input$dygraph_click$x) + 1,
                                       "%Y-%m-%d"))
          }
        }
      }
      
    })
  
  output$no_explanation2 <- renderUI({
    condition = as.Date(input$dygraph_click$x) + 1 >= as.Date("2019-03-13")
    if (length(condition) != 0) {
      if (condition == TRUE) {
        div(
          strong("You have selected: "),
          textOutput("clicked", inline = TRUE),
          hr(),
          DTOutput('tbl_instance'),
          br(),
          DTOutput('tbl_nbours') %>%
            withSpinner(color = "#0dc5c1")
        )
      }
      else {
        htmlOutput("no_explanation_past")
        
      }
      
    } else if (typeof(r$my_selector) == 'integer') {
      div(
        strong("You have selected: "),
        textOutput("clicked", inline = TRUE),
        hr(),
        DTOutput('tbl_instance'),
        br(),
        DTOutput('tbl_nbours') %>%
          withSpinner(color = "#0dc5c1")
      )
      
      
    }
    
    
    
    
    else {
      htmlOutput("select_a_date")
    }
  })
  
  r <- reactiveValues(my_selector = NULL)
  
  observeEvent(input$dygraph_click$x, {
    r$my_selector <- input$dygraph_click$x
  })
  
  observeEvent(input$go, {
    r$my_selector <- input$go[1]
  })
  
}

RMSE = function(actual, predicted) {
  sqrt(mean((actual - predicted) ^ 2))
}

produce_instance_tbl = function(predicted_data, instance_date) {
  row = predicted_data[predicted_data$Date == instance_date,]
  
  temp = row[, c(
    "Date",
    "week_of_year",
    "Forecast",
    "quantity_lag7",
    "quantity_lag14",
    "Season",
    "is_closed"
  )]
  
  
  f = formattable(
    temp,
    list(
      Forecast = formatter(
        "span",
        style = x ~  formattable::style(color = "blue", font.weight = "bold")
      ),
      area(col = c(quantity_lag7, quantity_lag14)) ~ color_bar("lightgrey"),
      Season = formatter("span",
                         style = x ~  formattable::style(color = ifelse(
                           x == "Low", "red",
                           ifelse(
                             x == "High",
                             "lightgreen",
                             ifelse(x == "Ultra High", "#088000", "black")
                           )
                         ))),
      is_closed  = formatter(
        "span",
        style = x ~  formattable::style(color = ifelse(x, "red", "green")),
        x ~ icontext(ifelse(x, "remove", "ok"), ifelse(x, "Closed", "Open"))
      )
    )
  )
  
  
  return(
    as.datatable(
      f,
      caption = 'Table 1: Selected Prediction Instance',
      colnames = c(
        'Week of the Year' = "week_of_year",
        'Sales 1 Week Ago' = 'quantity_lag7',
        'Sales 2 Weeks Ago' = 'quantity_lag14',
        'Season' = 'Season',
        'Open / Closed' = 'is_closed'
      ),
      extensions = c('Buttons', 'ColReorder'),
      rownames = FALSE,
      options = list(
        colReorder = list(realtime = FALSE),
        dom = 'Bt',
        columnDefs = list(list(
          className = 'dt-right', targets = 1:6
        )),
        buttons = list(list(
          extend = 'colvis', columns = c(1, 2, 3, 4, 5, 6)
        )),
        pageLength = 4
      )
    ) %>%
      
      formatDate(1, "toDateString")
  )
}

produce_nbour_tbl = function(data, instance_date) {
  row = data[data$Date == instance_date,]
  
  Neighbour1 = data[row$Neighbour1]
  Neighbour2 = data[row$Neighbour2]
  Neighbour3 = data[row$Neighbour3]
  Neighbour4 = data[row$Neighbour4]
  Neighbour5 = data[row$Neighbour5]
  Neighbour6 = data[row$Neighbour6]
  Neighbour7 = data[row$Neighbour7]
  
  var = rbind(Neighbour1,
              Neighbour2,
              Neighbour3,
              Neighbour4,
              Neighbour5,
              Neighbour6,
              Neighbour7)[, c(
                "Date",
                "week_of_year",
                "Past_Forecast",
                "quantity",
                "AD",
                "quantity_lag7",
                "quantity_lag14",
                "Season",
                "is_closed"
              )]
  var$rank = c(1:7)
  
  f = formattable(
    var,
    list(
      quantity = formatter(
        "span",
        # style = x ~  formattable::style(
        #     "font-weight" = ifelse(abs(mean(x) - x) == min(abs(mean(
        #         x
        #     ) - x)), "bold", NA),
        #     "color" = ifelse(abs(mean(x) - x) == min(abs(mean(
        #         x
        #     ) - x)), "black", NA)
        # )
        style = ~  formattable::style(
          "font-weight" = ifelse(abs(quantity - Past_Forecast) == min(AD), "bold", NA),
          "color" = ifelse(abs(quantity - Past_Forecast) == min(AD), "black", NA)
        )
      ),
      Past_Forecast = formatter(
        "span",
        # style = ~  formattable::style(
        #     "font-weight" = ifelse(abs(quantity - Past_Forecast) == min(abs(
        #         quantity
        #         - Past_Forecast
        #     )), "bold", NA),
        #     "color" = ifelse(abs(quantity - Past_Forecast) == min(abs(
        #         quantity
        #         - Past_Forecast
        #     )), "blue", NA)
        # )
        style = ~  formattable::style(
          "font-weight" = ifelse(abs(quantity - Past_Forecast) == min(AD), "bold", NA),
          "color" = ifelse(abs(quantity - Past_Forecast) == min(AD), "blue", NA)
        )
      ),
      AD = color_text("lightgreen", "red"),
      area(col = c(quantity_lag7, quantity_lag14)) ~ color_bar("lightgrey"),
      Season = formatter("span",
                         style = x ~  formattable::style(color = ifelse(
                           x == "Low", "red",
                           ifelse(
                             x == "High",
                             "lightgreen",
                             ifelse(x == "Ultra High", "#088000", "black")
                           )
                         ))),
      rank = color_text("darkgreen", "gray"),
      is_closed  = formatter(
        "span",
        style = x ~  formattable::style(color = ifelse(x, "red", "green")),
        x ~ icontext(ifelse(x, "remove", "ok"), ifelse(x, "Closed", "Open"))
      )
    )
  )
  
  return(
    as.datatable(
      f,
      caption = 'Table 2: Most Similar Datapoints to Selected Instance',
      colnames = c(
        'Week of the Year' = "week_of_year",
        'Sales' = 'quantity',
        'Absolute Deviation' = 'AD',
        'Sales 1 Week Ago' = 'quantity_lag7',
        'Sales 2 Weeks Ago' = 'quantity_lag14',
        'Season' = 'Season',
        'Past Forecast' = 'Past_Forecast',
        'Open / Closed' = 'is_closed',
        'Similarity Ranking' = 'rank'
      ),
      extensions = c('Buttons', 'ColReorder'),
      rownames = FALSE,
      options = list(
        columnDefs = list(
          list(targets = list(4, 9), visible = FALSE),
          list(className = 'dt-right', targets = 1:9)
        ),
        colReorder = list(realtime = FALSE),
        dom = 'Btp',
        buttons = list(list(
          extend = 'colvis', columns = c(1, 2, 3, 4, 5, 6, 7, 8, 9)
        )),
        pageLength = 4
      )
    ) %>%
      formatDate(1, "toDateString")
  )
}

MAD = function(data, instance_date, top_k) {
  row = data[data$Date == instance_date,]
  
  Neighbour1 = data[row$Neighbour1]
  Neighbour2 = data[row$Neighbour2]
  Neighbour3 = data[row$Neighbour3]
  Neighbour4 = data[row$Neighbour4]
  Neighbour5 = data[row$Neighbour5]
  Neighbour6 = data[row$Neighbour6]
  Neighbour7 = data[row$Neighbour7]
  
  var = rbind(Neighbour1,
              Neighbour2,
              Neighbour3,
              Neighbour4,
              Neighbour5,
              Neighbour6,
              Neighbour7)[, c("AD")]
  
  MAD = sapply(var[1:top_k], mean, na.rm = TRUE)
  return(round(MAD, digits = 1))
  
}

predict_and_list_nbours = function(train_product,
                                   test_product,
                                   BW,
                                   k,
                                   min_window_length) {
  performance_collector = c()
  history = train_product
  history$ID = c(1:nrow(history))
  history$Date = NULL
  
  
  temporary = train_product
  temporary$ID = c(1:nrow(temporary))
  temporary$Date = NULL
  
  test = test_product
  test$Date = NULL
  
  Forecast = c(rep(NA, nrow(train_product)))
  Past_Forecast = c(rep(NA, min_window_length))
  
  Q = c(train_product$quantity, rep(NA, BW * 7))
  Neighbour1 = c(rep(NA, min_window_length))
  Neighbour2 = c(rep(NA, min_window_length))
  Neighbour3 = c(rep(NA, min_window_length))
  Neighbour4 = c(rep(NA, min_window_length))
  Neighbour5 = c(rep(NA, min_window_length))
  Neighbour6 = c(rep(NA, min_window_length))
  Neighbour7 = c(rep(NA, min_window_length))
  
  for (a in (1:((
    nrow(train_product) - min_window_length
  ) / 7))) {
    dat = head(temporary, min_window_length + 7)
    
    train = head(dat, min_window_length)
    
    to_predict = tail(dat, 7)
    to_predict$quantity = NULL
    to_predict$ID = NULL
    
    fit <- knn(
      train_set = train,
      test_set = to_predict,
      k = k,
      comparison_measure = "squared_euclidean",
      return_ranked_neighbors = k,
      continuous_target = "quantity",
      id = "ID"
    )
    
    prediction = round(fit$test_set_scores$continuous_target, digits = 0)
    actual = tail(dat, 7)$quantity
    
    performance = RMSE(actual, prediction)
    
    # Store predictions and neighbours
    Past_Forecast = c(Past_Forecast, prediction)
    Neighbour1 = c(Neighbour1, fit$test_set_scores$neighbor1)
    Neighbour2 = c(Neighbour2, fit$test_set_scores$neighbor2)
    Neighbour3 = c(Neighbour3, fit$test_set_scores$neighbor3)
    Neighbour4 = c(Neighbour4, fit$test_set_scores$neighbor4)
    Neighbour5 = c(Neighbour5, fit$test_set_scores$neighbor5)
    Neighbour6 = c(Neighbour6, fit$test_set_scores$neighbor6)
    Neighbour7 = c(Neighbour7, fit$test_set_scores$neighbor7)
    
    # Update history and collect performance
    temporary = tail(temporary, -7)
    performance_collector = c(performance_collector, performance)
    
  }
  
  for (w in head(unique(test$week_of_year), BW)) {
    dat = tail(history, 119)
    dat = rbind(dat, test[week_of_year == w, , ], fill = TRUE)
    dat$ID[120:126] = c((length(history$ID) + 1):(length(history$ID) +
                                                    7))
    
    #Feature Engineering in Train Data
    dat$quantity_lag7 = shift(dat$quantity,
                              n = 7,
                              fill = NA,
                              type = "lag")
    dat$quantity_lag14 = shift(dat$quantity,
                               n = 14,
                               fill = NA,
                               type = "lag")
    
    #Seasonal component
    dat$seasonal[120:126] = dat$seasonal[1:7]
    
    #Training window
    train = dat[42:119, ]
    
    to_predict = tail(dat, 7)
    to_predict$quantity = NULL
    to_predict$ID = NULL
    
    fit <- knn(
      train_set = train,
      test_set = to_predict,
      k = k,
      comparison_measure = "squared_euclidean",
      return_ranked_neighbors = k,
      continuous_target = "quantity",
      id = "ID"
    )
    
    prediction = round(fit$test_set_scores$continuous_target, digits = 0)
    actual = test[test$week_of_year == w, ]$quantity
    
    performance = RMSE(actual, prediction)
    
    # Store predictions and neighbours
    Forecast = c(Forecast, prediction)
    Past_Forecast = c(Past_Forecast, prediction)
    Neighbour1 = c(Neighbour1, fit$test_set_scores$neighbor1)
    Neighbour2 = c(Neighbour2, fit$test_set_scores$neighbor2)
    Neighbour3 = c(Neighbour3, fit$test_set_scores$neighbor3)
    Neighbour4 = c(Neighbour4, fit$test_set_scores$neighbor4)
    Neighbour5 = c(Neighbour5, fit$test_set_scores$neighbor5)
    Neighbour6 = c(Neighbour6, fit$test_set_scores$neighbor6)
    Neighbour7 = c(Neighbour7, fit$test_set_scores$neighbor7)
    
    # Update history and collect performance
    history = rbind(history, tail(dat, 7))
    performance_collector = c(performance_collector, performance)
    
  }
  
  return_df <- history %>%
    mutate(Date = head(c(train_product$Date, test_product$Date), nrow(history))) %>%
    mutate(Forecast = Forecast) %>%
    mutate(Neighbour1 = Neighbour1) %>%
    mutate(Neighbour2 = Neighbour2) %>%
    mutate(Neighbour3 = Neighbour3) %>%
    mutate(Neighbour4 = Neighbour4) %>%
    mutate(Neighbour5 = Neighbour5) %>%
    mutate(Neighbour6 = Neighbour6) %>%
    mutate(Neighbour7 = Neighbour7) %>%
    mutate(Q = Q)
  
  return_df = cbind(return_df, Past_Forecast)
  
  Lower = c()
  Upper = c()
  for (i in (1:nrow(return_df))) {
    row = return_df[i, ]
    if (!is.na(row$Neighbour1)) {
      q1 = return_df[return_df$ID == row$Neighbour1]$quantity
      q2 = return_df[return_df$ID == row$Neighbour2]$quantity
      q3 = return_df[return_df$ID == row$Neighbour3]$quantity
      q4 = return_df[return_df$ID == row$Neighbour4]$quantity
      q5 = return_df[return_df$ID == row$Neighbour5]$quantity
      q6 = return_df[return_df$ID == row$Neighbour6]$quantity
      q7 = return_df[return_df$ID == row$Neighbour7]$quantity
      
      vect = c(q1, q2, q3, q4, q5, q6, q7)
      sd = sd(vect)
      mean = mean(vect)
      
      Lower = c(Lower, round(mean - sd, 0))
      Upper = c(Upper, round(mean + sd, 0))
    } else{
      Lower = c(Lower, NA)
      Upper = c(Upper, NA)
    }
  }
  
  return_df <- return_df %>%
    mutate(Upper = Upper) %>%
    mutate(Lower = Lower) %>%
    mutate(Season = cut(
      return_df$seasonal,
      4,
      labels = list('Low', 'Normal', 'High', 'Ultra High')
    )) %>%
    mutate(AD = abs(return_df$Past_Forecast - return_df$quantity))
  
  return(list(return_df, performance_collector))
}



predict_snaive = function(train_product, test_product, BW) {
  history = train_product
  history = history[, -c("quantity_lag7", "quantity_lag14", "seasonal")]
  test = test_product
  performance_collector = c()
  
  Forecast = c(rep(NA, nrow(train_product)))
  Lower = c(rep(NA, nrow(train_product)))
  Upper = c(rep(NA, nrow(train_product)))
  Q = c(train_product$quantity, rep(NA, BW * 7))
  
  for (w in head(unique(test$week_of_year), BW)) {
    dat = tail(history, 119)
    
    dat_ts = ts(dat$quantity, frequency = 7)
    
    prediction = snaive(dat_ts, h = 7)$mean
    lwr = snaive(dat_ts, h = 7)$lower[, 1]
    upr = snaive(dat_ts, h = 7)$upper[, 1]
    actual = test[week_of_year == w, ]$quantity
    
    performance = RMSE(actual, prediction)
    
    # Store predictions
    Forecast = c(Forecast, prediction)
    Lower = c(Lower, lwr)
    Upper = c(Upper, upr)
    
    # Update history and collect performance
    history = rbind(history, test[week_of_year == w, ])
    performance_collector = c(performance_collector, performance)
  }
  return_df <- history %>%
    mutate(Date = head(c(train_product$Date, test_product$Date), nrow(history))) %>%
    mutate(Forecast = Forecast) %>%
    mutate(Lower = Lower) %>%
    mutate(Upper = Upper) %>%
    mutate(Q = Q)
  
  return(list(return_df, performance_collector))
}

predict_and_list_nbours_single = function(train_product,
                                          test_product,
                                          date,
                                          quantity_lag7,
                                          quantity_lag14,
                                          seasonal,
                                          is_closed = 0,
                                          k,
                                          min_window_length) {
  date = as.Date(date, format = "%Y-%m-%d")
  day_of_week = as.numeric(strftime(date, "%u"))
  week_of_year = week(date - 2)
  BW = week_of_year - 10
  
  performance_collector = c()
  history = train_product
  history$ID = c(1:nrow(history))
  history$Date = NULL
  
  
  temporary = train_product
  temporary$ID = c(1:nrow(temporary))
  temporary$Date = NULL
  
  test = test_product
  test$Date = NULL
  
  Forecast = c(rep(NA, nrow(train_product)))
  Past_Forecast = c(rep(NA, min_window_length))
  
  Q = c(train_product$quantity, rep(NA, BW * 7))
  Neighbour1 = c(rep(NA, min_window_length))
  Neighbour2 = c(rep(NA, min_window_length))
  Neighbour3 = c(rep(NA, min_window_length))
  Neighbour4 = c(rep(NA, min_window_length))
  Neighbour5 = c(rep(NA, min_window_length))
  Neighbour6 = c(rep(NA, min_window_length))
  Neighbour7 = c(rep(NA, min_window_length))
  
  
  
  for (a in (1:((
    nrow(train_product) - min_window_length
  ) / 7))) {
    dat = head(temporary, min_window_length + 7)
    
    train = head(dat, min_window_length)
    
    to_predict = tail(dat, 7)
    to_predict$quantity = NULL
    to_predict$ID = NULL
    
    fit <- knn(
      train_set = train,
      test_set = to_predict,
      k = k,
      comparison_measure = "squared_euclidean",
      return_ranked_neighbors = k,
      continuous_target = "quantity",
      id = "ID"
    )
    
    prediction = round(fit$test_set_scores$continuous_target, digits = 0)
    actual = tail(dat, 7)$quantity
    
    performance = RMSE(actual, prediction)
    
    # Store predictions and neighbours
    Past_Forecast = c(Past_Forecast, prediction)
    Neighbour1 = c(Neighbour1, fit$test_set_scores$neighbor1)
    Neighbour2 = c(Neighbour2, fit$test_set_scores$neighbor2)
    Neighbour3 = c(Neighbour3, fit$test_set_scores$neighbor3)
    Neighbour4 = c(Neighbour4, fit$test_set_scores$neighbor4)
    Neighbour5 = c(Neighbour5, fit$test_set_scores$neighbor5)
    Neighbour6 = c(Neighbour6, fit$test_set_scores$neighbor6)
    Neighbour7 = c(Neighbour7, fit$test_set_scores$neighbor7)
    
    # Update history and collect performance
    temporary = tail(temporary, -7)
    performance_collector = c(performance_collector, performance)
    
  }
  
  
  if (week_of_year <= 14) {
    for (w in Filter(function (x)
      x <= week_of_year, unique(test$week_of_year))) {
      dat = tail(history, 119)
      dat = rbind(dat, test[week_of_year == w, , ], fill = TRUE)
      dat$ID[120:126] = c((length(history$ID) + 1):(length(history$ID) +
                                                      7))
      
      # Feature Engineering in Train Data
      dat$quantity_lag7 = shift(dat$quantity,
                                n = 7,
                                fill = NA,
                                type = "lag")
      
      dat$quantity_lag14 = shift(dat$quantity,
                                 n = 14,
                                 fill = NA,
                                 type = "lag")
      
      # Seasonal Component
      dat$seasonal[120:126] = dat$seasonal[1:7]
      
      # Training window
      train = dat[42:119, ]
      
      to_predict = tail(dat, 7)
      to_predict$quantity = NULL
      to_predict$ID = NULL
      
      fit <- knn(
        train_set = train,
        test_set = to_predict,
        k = k,
        comparison_measure = "squared_euclidean",
        return_ranked_neighbors = k,
        continuous_target = "quantity",
        id = "ID"
      )
      
      prediction = round(fit$test_set_scores$continuous_target, digits = 0)
      actual = test[test$week_of_year == w, ]$quantity
      
      performance = RMSE(actual, prediction)
      
      # Store predictions and neighbours
      Forecast = c(Forecast, prediction)
      Past_Forecast = c(Past_Forecast, prediction)
      Neighbour1 = c(Neighbour1, fit$test_set_scores$neighbor1)
      Neighbour2 = c(Neighbour2, fit$test_set_scores$neighbor2)
      Neighbour3 = c(Neighbour3, fit$test_set_scores$neighbor3)
      Neighbour4 = c(Neighbour4, fit$test_set_scores$neighbor4)
      Neighbour5 = c(Neighbour5, fit$test_set_scores$neighbor5)
      Neighbour6 = c(Neighbour6, fit$test_set_scores$neighbor6)
      Neighbour7 = c(Neighbour7, fit$test_set_scores$neighbor7)
      
      # Update history and collect performance
      history = rbind(history, tail(dat, 7))
      performance_collector = c(performance_collector, performance)
      
    }
    
    # Get last week of processed history to make forecast
    train = dat[42:119, ]
    
    to_predict = data.frame(is_closed,
                            day_of_week,
                            week_of_year,
                            quantity_lag7,
                            quantity_lag14,
                            seasonal)
    
    fit <- knn(
      train_set = train,
      test_set = to_predict,
      k = k,
      comparison_measure = "squared_euclidean",
      return_ranked_neighbors = k,
      continuous_target = "quantity",
      id = "ID"
    )
    
    prediction = round(fit$test_set_scores$continuous_target, digits = 0)
    
    # Storing Neighbours
    Neighbour1 = fit$test_set_scores$neighbor1
    Neighbour2 = fit$test_set_scores$neighbor2
    Neighbour3 = fit$test_set_scores$neighbor3
    Neighbour4 = fit$test_set_scores$neighbor4
    Neighbour5 = fit$test_set_scores$neighbor5
    Neighbour6 = fit$test_set_scores$neighbor6
    Neighbour7 = fit$test_set_scores$neighbor7
    
  } else {
    for (w in unique(test$week_of_year)) {
      dat = tail(history, 119)
      dat = rbind(dat, test[week_of_year == w, , ], fill = TRUE)
      dat$ID[120:126] = c((length(history$ID) + 1):(length(history$ID) +
                                                      7))
      
      # Feature Engineering in Train Data
      dat$quantity_lag7 = shift(dat$quantity,
                                n = 7,
                                fill = NA,
                                type = "lag")
      
      dat$quantity_lag14 = shift(dat$quantity,
                                 n = 14,
                                 fill = NA,
                                 type = "lag")
      
      # Seasonal component
      dat$seasonal[120:126] = dat$seasonal[1:7]
      
      # Training window
      train = dat[42:119, ]
      
      to_predict = tail(dat, 7)
      to_predict$quantity = NULL
      to_predict$ID = NULL
      
      fit <- knn(
        train_set = train,
        test_set = to_predict,
        k = k,
        comparison_measure = "squared_euclidean",
        return_ranked_neighbors = k,
        continuous_target = "quantity",
        id = "ID"
      )
      
      prediction = round(fit$test_set_scores$continuous_target, digits = 0)
      actual = test[test$week_of_year == w, ]$quantity
      
      performance = RMSE(actual, prediction)
      
      # Store predictions and neighbours
      Forecast = c(Forecast, prediction)
      Past_Forecast = c(Past_Forecast, prediction)
      Neighbour1 = c(Neighbour1, fit$test_set_scores$neighbor1)
      Neighbour2 = c(Neighbour2, fit$test_set_scores$neighbor2)
      Neighbour3 = c(Neighbour3, fit$test_set_scores$neighbor3)
      Neighbour4 = c(Neighbour4, fit$test_set_scores$neighbor4)
      Neighbour5 = c(Neighbour5, fit$test_set_scores$neighbor5)
      Neighbour6 = c(Neighbour6, fit$test_set_scores$neighbor6)
      Neighbour7 = c(Neighbour7, fit$test_set_scores$neighbor7)
      
      # Update history and collect performance
      history = rbind(history, tail(dat, 7))
      performance_collector = c(performance_collector, performance)
      
    }
    
    train = dat[15:119, ]
    
    to_predict = data.frame(is_closed,
                            day_of_week,
                            week_of_year,
                            quantity_lag7,
                            quantity_lag14,
                            seasonal)
    
    fit <- knn(
      train_set = train,
      test_set = to_predict,
      k = k,
      comparison_measure = "squared_euclidean",
      return_ranked_neighbors = k,
      continuous_target = "quantity",
      id = "ID"
    )
    
    prediction = round(fit$test_set_scores$continuous_target, digits = 0)
    
    # Storing neighbours
    Neighbour1 = fit$test_set_scores$neighbor1
    Neighbour2 = fit$test_set_scores$neighbor2
    Neighbour3 = fit$test_set_scores$neighbor3
    Neighbour4 = fit$test_set_scores$neighbor4
    Neighbour5 = fit$test_set_scores$neighbor5
    Neighbour6 = fit$test_set_scores$neighbor6
    Neighbour7 = fit$test_set_scores$neighbor7
    
  }
  
  predicted_df <- to_predict %>%
    mutate(quantity = NA) %>%
    mutate(Date = date) %>%
    mutate(Past_Forecast = NA) %>%
    mutate(Forecast = prediction) %>%
    mutate(Neighbour1 = Neighbour1) %>%
    mutate(Neighbour2 = Neighbour2) %>%
    mutate(Neighbour3 = Neighbour3) %>%
    mutate(Neighbour4 = Neighbour4) %>%
    mutate(Neighbour5 = Neighbour5) %>%
    mutate(Neighbour6 = Neighbour6) %>%
    mutate(Neighbour7 = Neighbour7)
  
  history = head(history, -7)
  data_df <- history %>%
    mutate(Date = as.Date(head(
      c(train_product$Date, test_product$Date), nrow(history)
    ))) %>%
    mutate(ID = NULL) %>%
    mutate(Forecast = rep(NA, nrow(history))) %>%
    mutate(Neighbour1 = rep(NA, nrow(history))) %>%
    mutate(Neighbour2 = rep(NA, nrow(history))) %>%
    mutate(Neighbour3 = rep(NA, nrow(history))) %>%
    mutate(Neighbour4 = rep(NA, nrow(history))) %>%
    mutate(Neighbour5 = rep(NA, nrow(history))) %>%
    mutate(Neighbour6 = rep(NA, nrow(history))) %>%
    mutate(Neighbour7 = rep(NA, nrow(history)))
  
  return_df = cbind(data_df, Past_Forecast)
  
  return_df <- rbind(return_df, predicted_df)
  
  Lower = c()
  Upper = c()
  for (i in (1:nrow(return_df))) {
    row = return_df[i, ]
    if (!is.na(row$Neighbour1)) {
      q1 = return_df[return_df$ID == row$Neighbour1]$quantity
      q2 = return_df[return_df$ID == row$Neighbour2]$quantity
      q3 = return_df[return_df$ID == row$Neighbour3]$quantity
      q4 = return_df[return_df$ID == row$Neighbour4]$quantity
      q5 = return_df[return_df$ID == row$Neighbour5]$quantity
      q6 = return_df[return_df$ID == row$Neighbour6]$quantity
      q7 = return_df[return_df$ID == row$Neighbour7]$quantity
      
      vect = c(q1, q2, q3, q4, q5, q6, q7)
      sd = sd(vect)
      mean = mean(vect)
      
      Lower = c(Lower, round(mean - sd, 0))
      Upper = c(Upper, round(mean + sd, 0))
    } else{
      Lower = c(Lower, NA)
      Upper = c(Upper, NA)
    }
  }
  
  
  return_df <- return_df %>%
    mutate(Upper = Upper) %>%
    mutate(Lower = Lower) %>%
    mutate(Season = cut(
      return_df$seasonal,
      4,
      labels = list('Low',
                    'Normal',
                    'High',
                    'Ultra High')
    )) %>%
    mutate(AD = abs(return_df$Past_Forecast - return_df$quantity))
  
  
  
  return(list(return_df, 0))
}

generatePlotly = function(predicted_data, BW) {
  week = tail(predicted_data, BW * 7)
  data = week[, c("Date", "week_of_year", "Forecast", "Upper", "Lower")]
  business_week = unique(data$week_of_year)
  data = data %>%
    mutate(sd = (data$Upper - data$Lower) / 2)
  fig <-
    plot_ly(
      data = data,
      x = ~ Date,
      y = ~ Forecast,
      hovertemplate = paste('<b>%{y}</b>', '<br>%{x}'),
      name = 'Forecast',
      type = 'bar',
      error_y = ~ list(array = sd,
                       color = 'lightblue')
    ) %>%
    layout(
      title = paste(
        "<b>AI Forecasted Bread Sales for Business Week",
        tail(business_week, 1),
        "</b>\n"
      ),
      xaxis = list(
        range = c(
          as.numeric(max(data$Date) - 7) * 86400000 + 43200000 ,
          as.numeric(max(data$Date)) * 86400000 + 43200000
        ),
        rangeselector = list(buttons = list(
          list(
            count = 7,
            label = paste("BW", tail(business_week, 1)),
            step = "day",
            stepmode = "forward"
          ),
          list(step = "all")
        ))
      ),
      
      yaxis = list(title = "Sales Forecast")
    ) %>% config(displayModeBar = F)
  
  return(fig)
}

# Run the application
shinyApp(ui = ui, server = server)
