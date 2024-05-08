# this script was created by Michael LaScaleia for use in UConn EEB 2244 
# please ask for permission before using it for any other purpose
# but I also can't stop you and I made it publicly available so...

# this app is published at https://mlascaleia.shinyapps.io/MockulusV1/
# if that site goes down or stops working, you can rehost it wherever

# if you plan on editing how the plots are made, please see my note about
# minstep in the repo

# load packages
library(shiny)
library(tidyverse)
library(ggthemes)

# set useful values
e <- exp(1)


# create ui ####

ui <- fluidPage(tabsetPanel(
  
  # specify that there are tabs (3 of them)
  
  type = "tabs",
  
  # tab 1 ####
  tabPanel(
    
    # tab title
    "Part I",
    
    # title on page
    titlePanel("Part I: Single Population Dynamics"),
    
    # things students can change go in the sidebar
    sidebarLayout(
        sidebarPanel(
          
          # select which of the 3 growth types to use
          # terminology from 2244 class lecture
          selectInput(
            inputId = "growthType",
            label = "Growth type",
            choices = c("Exponential", "Geometric", "Logistic")
          ),
          
          # If they select exponential...
          conditionalPanel(
            condition = "input.growthType == 'Exponential'",
            
            # set how long they want it to run for
            numericInput(
              inputId = "t_exp",
              label = "Time steps (t)",
              min = 0,
              step = 1,
              value = 25
            ),
            
            # set r
            sliderInput(
              inputId = "r_exp",
              label = "Intrinsic growth rate (r)",
              step = .01,
              value = 0.05,
              min = -2,
              max = 2
            ),
            
            # set inital pop size
            numericInput(
              inputId = "n_0_exp",
              label = "Initial population size (n_0)",
              min = 0,
              step = 1,
              value = 10
            ),
            
            # change plot type
            checkboxInput(
              inputId = "rate",
              label = "dN/dt vs N",
              value = F
            ),
            
            # change scaling
            checkboxInput(
              inputId = "ln_exp",
              label = "ln(N)",
              value = F
            )
          ),
          
          # If they select geometric...
          conditionalPanel(
            condition = "input.growthType == 'Geometric'",
            
            # set amount of time to run for
            numericInput(
              inputId = "t_geo",
              label = "Time steps (t)",
              min = 0,
              step = 1,
              value = 25
            ),
            
            # set lambda (despite the fact it's called r)
            sliderInput(
              inputId = "r_geo",
              label = "Intrinsic growth rate (lambda)",
              step = .01,
              value = 1.05,
              min = 0,
              max = 3
            ),
            
            # set inital value
            numericInput(
              inputId = "n_0_geo",
              label = "Initial population size (n_0)",
              min = 0,
              step = 1,
              value = 10
            ),
            
            # change scaling?
            checkboxInput(
              inputId = "ln_geo",
              label = "ln(N)",
              value = F
            ),
          ),
          
          # If they select logistic...
          conditionalPanel(
            condition = "input.growthType == 'Logistic'",
            
            # set time steps
            numericInput(
              inputId = "t_log",
              label = "Time steps (t)",
              min = 0,
              step = 1,
              value = 25
            ),
            
            # set time lag
            numericInput(
              inputId = "tau_log",
              label = "Time lag (tau)",
              min = 0,
              step = .1,
              value = 0
            ),
            
            # set intrinsic growth rate
            sliderInput(
              inputId = "r_log",
              label = "Intrinsic growth rate (r)",
              step = .01,
              value = 0.25,
              min = -2,
              max = 5
            ),
            
            # set carrying capacity
            numericInput(
              inputId = "k_log",
              label = "Carrying capacity (k)",
              min = 0,
              step = 1,
              value = 100
            ),
            
            # select plot type
            checkboxInput(
              inputId = "rate_log",
              label = "dN/dt vs. N",
              value = F
            )
          )
        ),
        
        
      # now that values have been selected, what should be shown on the main panel?
      mainPanel(
        conditionalPanel(
          condition = "input.growthType == 'Exponential'",
          plotOutput("plot_exp")
        ),
        conditionalPanel(
          condition = "input.growthType == 'Geometric'",
          plotOutput("plot_geom")
        ),
        conditionalPanel(
          condition = "input.growthType == 'Logistic'",
          plotOutput("plot_log")
        )
      )
    )
  ),
  
  # panel 2 ####
  tabPanel(
    "Part II",
    headerPanel("Part II: Lotka-Volterra competition"),
    
    # create two columns, one for each column
    fluidRow(
      column(
        # "6" is the width - the whole screen is 12 wide I believe
        6,
        titlePanel("Species 1 (red)"),
        
        # these are the inputs for the red species (red trillium)
        sliderInput(
          inputId = "r1",
          label = "Intrinsic growth rate (r1)",
          step = .01,
          value = 0.05,
          min = 0,
          max = 2
        ),
        numericInput(
          inputId = "k1",
          label = "Carrying capacity (k1)",
          min = 0,
          step = 1,
          value = 100
        ),
        numericInput(
          inputId = "n1",
          label = "Initial population size (N1)",
          min = 0,
          step = 1,
          value = 10
        ),
        numericInput(
          inputId = "alpha",
          label = "Competition coefficient (alpha)",
          min = 0,
          step = .01,
          value = .5
        )
      ),
      
      # inputs for the blues species (garlic mustard)
      column(
        
        # 6 specifices it will take the other half of the screen
        6,
        titlePanel("Species 2 (blue)"),
        sliderInput(
          inputId = "r2",
          label = "Intrinsic growth rate (r2)",
          step = .01,
          value = 0.05,
          min = 0,
          max = 2
        ),
        numericInput(
          inputId = "k2",
          label = "Carrying capacity (k2)",
          min = 0,
          step = 1,
          value = 100
        ),
        numericInput(
          inputId = "n2",
          label = "Initial population size (N2)",
          min = 0,
          step = 1,
          value = 10
        ),
        numericInput(
          inputId = "beta",
          label = "Competition coefficient (beta)",
          min = 0,
          step = .01,
          value = .5
        )
      )
    ),
    
    # what will the main panel show, now that values have been selected?
    # okay the main panel actually has a few more things to select:
    mainPanel(
      titlePanel("Global settings"),
      numericInput(
        inputId = "t_II",
        label = "Time steps (t)",
        min = 0,
        step = 1,
        value = 4
      ),
      selectInput(
        inputId = "plotType_II",
        label = "Plot type",
        choices = c("Populations vs. time", "N1 vs. N2")
      )
    ),
    
    # okay NOW what will the main panel show?
    conditionalPanel(condition = "input.plotType_II == 'Populations vs. time'",
                     plotOutput("plot_PvT")),
    conditionalPanel(condition = "input.plotType_II == 'N1 vs. N2'",
                     plotOutput("plot_N1vN2"))
  ),
  # panel 3 ####
  tabPanel(
    "Part III",
    headerPanel("Part III: Predator-prey models"),
    
    # again choosing values for two populations (predator and prey)
    
    fluidRow(
      column(
        6,
        titlePanel("Prey (green)"),
        numericInput(
          inputId = "n_III",
          label = "Initial population size (N)",
          min = 0,
          step = 1,
          value = 10
        ),
        sliderInput(
          inputId = "r_III",
          label = "Intrinsic growth rate (r)",
          step = .01,
          value = 0.05,
          min = 0,
          max = 2
        ),
        checkboxInput(
          inputId = "useK",
          label = "prey carrying capacity?",
          value = F
        ),
        conditionalPanel(
          condition = "input.useK",
          numericInput(
            inputId = "k_III",
            label = "Carrying capacity (k)",
            min = 0,
            step = 1,
            value = 100
          ),
        )
      ),
      column(
        6,
        titlePanel("Predator (orange)"),
        numericInput(
          inputId = "p_III",
          label = "Initial population size (P)",
          min = 0,
          step = 1,
          value = 0
        ),
        sliderInput(
          inputId = "a_III",
          label = "Conversion efficiency (a)",
          min = 0,
          step = .01,
          value = 0.5,
          max = 2
        ),
        sliderInput(
          inputId = "c_III",
          label = "Capture rate (c)",
          min = 0,
          step = .01,
          value = 0.05,
          max = 1
        ),
        sliderInput(
          inputId = "m_III",
          label = "Mortality (m)",
          min = 0,
          step = .01,
          value = .1,
          max = 1
        )
      )
    ),
    
    # main panel is essentially the same as in tab 2
    mainPanel(
      titlePanel("Global settings"),
      numericInput(
        inputId = "t_III",
        label = "Time steps (t)",
        min = 0,
        step = 1,
        value = 2
      ),
      selectInput(
        inputId = "plotType_III",
        label = "Plot type",
        choices = c("Populations vs. time", "N vs. P")
      )
    ),
    conditionalPanel(condition = "input.plotType_III == 'Populations vs. time'",
                     plotOutput("plot_NPvT")),
    conditionalPanel(condition = "input.plotType_III == 'N vs. P'",
                     plotOutput("plot_NvP"))
  )
  # closing ####
))

# functions ####

# these are the functions that are used to make the plots
# this was a nightmare of repeating myself, and now with these functions
# it's only somewhat horrible

# part I plots

partI_plots <- function(timeSteps_input,
                        plotType,
                        n0 = NULL,
                        r = NULL,
                        lambda = NULL,
                        k = NULL,
                        tau = NULL,
                        
                        # SEE MY NOTE ABOUT MINSTEP
                        
                        minstep = .1){
  
  # initialize
  series <- seq(1, input$t_exp, by = minstep)
  vals <- vector()
  change <- vector()
  
  # I set the initial value to be 10 when we're talking about change
  # this is arbitrary
  if(plotType == "log") 
    vals[1] <- 10
  else 
    vals[1] <- n0
  
  # make values
  # okay honestly I'm coming back a full year after I wrote this to comment it
  # this section horrifies me as much as it horrifies you 
  # https://i.kym-cdn.com/photos/images/newsfeed/002/687/797/332.png
  
  # and to be honest I'm not sure I actually used this function? 
  # I just wrote it and abandoned it
  
  if(plotType == "exp"){
    for (i in 1:length(series)) {
      vals[i + 1] <- vals[i] + (r * vals[i] * minstep)
      change[i] <- r * vals[i]
      if (vals[i + 1] <= 0)
        vals[i + 1] <- 0
    }
  } else if (plotType == "geom"){
    vals <- input$n_0_geo * input$r_geo ^ series
  } else {
    for (i in 1:length(series)) {
      if (i < (i_back + .01))
        vals[i + 1] <-
          vals[i] + ((r * vals[i]) * (1 - (vals[1] / k)) * minstep)
      else if (i >= i_back)
        vals[i + 1] <-
          vals[i] + ((r * vals[i]) * (1 - (vals[i - i_back] / k)) * minstep)
      if (vals[i + 1] <= 0)
        vals[i + 1] <- 0
      change[i] <- vals[i + 1] - vals[i]
    }
  }
  
  vv <- vals[1:(length(vals) - 1)]
  
  # plot
  
  if (!input$rate) {
    g <- ggplot(mapping = aes(x = series, y = vv)) +
      geom_line() +
      theme_bw() +
      ylab("Population size (N)") +
      xlab("Time step (t)") +
      ylim(0, max(vv)) +
      theme(text = element_text(size = 24))
  } else if (r == 0) {
    g <- ggplot(mapping = aes(x = vv, y = change)) +
      geom_point() +
      theme_bw() +
      ylab("Change in population size (dN/dt)") +
      xlab("N") +
      theme(text = element_text(size = 24))
  } else {
    g <- ggplot(mapping = aes(x = vv, y = change)) +
      geom_line() +
      theme_bw() +
      ylab("Change in population size (dN/dt)") +
      xlab("N") +
      theme(text = element_text(size = 24))
  }
  if (input$ln_exp) {
    g <- g + scale_y_continuous(trans = "log")
  }
  g
  
  
}





# server ####
server <- function(input, output) {
  # exp growth ####
  
  output$plot_exp <- renderPlot({
    minstep <- .1
    series <- seq(1, input$t_exp, by = minstep)
    vals <- vector()
    change <- vector()
    vals[1] <- input$n_0_exp
    r <- input$r_exp
    
    # these for loops each create the next step in the line
    # based off the previous step in the line
    
    for (i in 1:length(series)) {
      
      # see how it's the growth rate * the pop size * whatever the step size is?
      # that's the fundemental idea behind each of these figures
      vals[i + 1] <- vals[i] + (r * vals[i] * minstep)
      change[i] <- r * vals[i]
      if (vals[i + 1] <= 0)
        vals[i + 1] <- 0
    }
    vv <- vals[1:(length(vals) - 1)]
    if (!input$rate) {
      g <- ggplot(mapping = aes(x = series, y = vv)) +
        geom_line() +
        theme_bw() +
        ylab("Population size (N)") +
        xlab("Time step (t)") +
        ylim(0, max(vv)) +
        theme(text = element_text(size = 24))
    } else if (r == 0) {
      g <- ggplot(mapping = aes(x = vv, y = change)) +
        geom_point() +
        theme_bw() +
        ylab("Change in population size (dN/dt)") +
        xlab("N") +
        theme(text = element_text(size = 24))
    } else {
      g <- ggplot(mapping = aes(x = vv, y = change)) +
        geom_line() +
        theme_bw() +
        ylab("Change in population size (dN/dt)") +
        xlab("N") +
        theme(text = element_text(size = 24))
    }
    if (input$ln_exp) {
      g <- g + scale_y_continuous(trans = "log")
    }
    g
  })
  
  # Discrete exponential growth ####
  
  output$plot_geom <- renderPlot({
    series <- seq(1, input$t_geo, by = 1)
    vals <- input$n_0_geo * input$r_geo ^ series
    g <- ggplot(mapping = aes(x = series, y = vals)) +
      geom_point() +
      theme_bw() +
      ylab("Population size") +
      xlab("Time step") +
      ylim(0, max(vals)) +
      theme(text = element_text(size = 24))
    
    if (input$ln_geo) {
      g <- g + scale_y_continuous(trans = "log")
    }
    g
  })
  
  # logistic growth ####
  
  output$plot_log <- renderPlot({
    minstep <- .1
    series <- seq(1, input$t_log, by = minstep)
    tau <- input$tau_log
    i_back <- tau / minstep
    vals <- vector()
    change <- vector()
    # vals[1] <- input$n_0_log
    vals[1] <- 10
    r <- input$r_log
    k <- input$k_log
    for (i in 1:length(series)) {
      if (i < (i_back + .01))
        vals[i + 1] <-
          vals[i] + ((r * vals[i]) * (1 - (vals[1] / k)) * minstep)
      else if (i >= i_back)
        vals[i + 1] <-
          vals[i] + ((r * vals[i]) * (1 - (vals[i - i_back] / k)) * minstep)
      if (vals[i + 1] <= 0)
        vals[i + 1] <- 0
      change[i] <- vals[i + 1] - vals[i]
    }
    vv <- vals[1:(length(vals) - 1)]
    if (!input$rate_log) {
      g <- ggplot(mapping = aes(x = series, y = vv)) +
        geom_line() +
        theme_bw() +
        ylab("Population size") +
        xlab("Time step") +
        geom_hline(yintercept = k,
                   color = "red",
                   linetype = "dotted") +
        theme(text = element_text(size = 24))
    } else {
      g <- ggplot(mapping = aes(x = vv, y = change)) +
        geom_line() +
        theme_bw() +
        ylab("Change in population size (dN/dt)") +
        xlab("N") +
        ylim(0, max(change)) +
        theme(text = element_text(size = 24))
    }
    
    g
    
  })
  # Pops v. time ####
  
  output$plot_PvT <- renderPlot({
    # global values
    series <- seq()
    minstep <- 1
    series <- seq(1, input$t_II, by = minstep)
    
    # n1 values
    vals1 <- vector()
    vals1[1] <- input$n1
    r1 <- input$r1
    k1 <- input$k1
    alpha <- input$alpha
    
    # n2 values
    vals2 <- vector()
    vals2[1] <- input$n2
    r2 <- input$r2
    k2 <- input$k2
    beta <- input$beta
    
    # loop
    for (i in 1:length(series)) {
      vals1[i + 1] <- vals1[i] + ((r1 * vals1[i]) *
                                    (1 - ((
                                      vals1[i] + (alpha * vals2[i])
                                    ) / k1)))
      vals2[i + 1] <- vals2[i] + ((r2 * vals2[i]) *
                                    (1 - ((
                                      vals2[i] + (beta * vals1[i])
                                    ) / k2)))
      if (vals1[i + 1] <= 0)
        vals1[i + 1] <- 0
      if (vals2[i + 1] <= 0)
        vals2[i + 1] <- 0
    }
    vv1 <- vals1[1:(length(vals1) - 1)]
    vv2 <- vals2[1:(length(vals2) - 1)]
    
    # plot
    
    ggplot(mapping = aes(x = series)) +
      geom_line(mapping = aes(y = vv1), color = "red") +
      geom_line(mapping = aes(y = vv2), color = "blue") +
      theme_bw() +
      ylab("Population size") +
      xlab("Time step") +
      ylim(0, max(c(vv1, vv2))) +
      theme(text = element_text(size = 24))
  })
  
  # N1 v. N2 ####
  
  output$plot_N1vN2 <- renderPlot({
    # global values
    series <- seq()
    minstep <- 1
    series <- seq(1, input$t_II, by = minstep)
    
    # n1 values
    vals1 <- vector()
    vals1[1] <- input$n1
    r1 <- input$r1
    k1 <- input$k1
    alpha <- input$alpha
    
    # n2 values
    vals2 <- vector()
    vals2[1] <- input$n2
    r2 <- input$r2
    k2 <- input$k2
    beta <- input$beta
    
    # loop
    for (i in 1:length(series)) {
      vals1[i + 1] <- vals1[i] + ((r1 * vals1[i]) *
                                    (1 - ((
                                      vals1[i] + (alpha * vals2[i])
                                    ) / k1)))
      vals2[i + 1] <- vals2[i] + ((r2 * vals2[i]) *
                                    (1 - ((
                                      vals2[i] + (beta * vals1[i])
                                    ) / k2)))
      if (vals1[i + 1] <= 0)
        vals1[i + 1] <- 0
      if (vals2[i + 1] <= 0)
        vals2[i + 1] <- 0
    }
    vv1 <- vals1[1:(length(vals1) - 1)]
    vv2 <- vals2[1:(length(vals2) - 1)]
    
    pp <- data.frame(step = series, n1 = vv1, n2 = vv2)
    
    startx <- c(0, 0)
    starty <- c(k1 / alpha, k2)
    endx <- c(k1, k2 / beta)
    endy <- c(0, 0)
    
    lindat <- data.frame(
      sx = startx,
      sy = starty,
      ex = endx,
      ey = endy
    )
    
    # plot
    ggplot(data = pp, aes(x = n1, y = n2)) +
      geom_point(data = pp[1, ],
                 size = 3,
                 color = "darkgreen") +
      geom_label(data = pp[1, ],
                 label = "start",
                 nudge_y = 1) +
      geom_point(
        data = pp[nrow(pp), ],
        size = 3,
        color = "darkgreen",
        shape = 1
      ) +
      geom_label(data = pp[nrow(pp), ],
                 label = "end",
                 nudge_y = 1) +
      geom_path(color = "darkgreen") +
      geom_segment(
        data = lindat[1, ],
        aes(
          x = sx,
          y = sy,
          xend = ex,
          yend = ey
        ),
        color = "red",
        linetype = "longdash"
      ) +
      geom_segment(
        data = lindat[2, ],
        aes(
          x = sx,
          y = sy,
          xend = ex,
          yend = ey
        ),
        color = "blue",
        linetype = "longdash"
      ) +
      theme_bw() +
      ylab("N2") +
      xlab("N1") +
      theme(text = element_text(size = 24))
  })
  
  
  # N & P v. time ####
  
  output$plot_NPvT <- renderPlot({
    # global values
    minstep <- .1
    series <- seq(1, input$t_III, by = minstep)
    
    # n values
    vals1 <- vector()
    vals1[1] <- input$n_III
    r <- input$r_III
    
    if (input$useK) {
      k <- input$k_III
    } else {
      k <- 2 ^ 31
    }
    
    # p values
    vals2 <- vector()
    vals2[1] <- input$p_III
    a <- input$a_III
    c <- input$c_III
    m <- input$m_III
    
    # loop
    for (i in 1:length(series)) {
      vals1[i + 1] <- vals1[i] + ((r * vals1[i] * (1 - vals1[i] / k)) -
                                    (c * vals1[i] * vals2[i])) * minstep
      vals2[i + 1] <- vals2[i] + ((a * c * vals1[i] * vals2[i]) -
                                    (m * vals2[i])) * minstep
      if (vals1[i + 1] <= 0)
        vals1[i + 1] <- 0
      if (vals2[i + 1] <= 0)
        vals2[i + 1] <- 0
    }
    vv1 <- vals1[1:(length(vals1) - 1)]
    vv2 <- vals2[1:(length(vals2) - 1)]
    
    # plot
    
    ggplot(mapping = aes(x = series)) +
      geom_line(mapping = aes(y = vv1), color = "green") +
      geom_line(mapping = aes(y = vv2), color = "orange") +
      theme_bw() +
      ylab("Population size") +
      xlab("Time step") +
      theme(text = element_text(size = 24))
  })
  
  
  # N v. P ####
  
  output$plot_NvP <- renderPlot({
    # global values
    minstep <- .1
    series <- seq(1, input$t_III, by = minstep)
    
    # n values
    vals1 <- vector()
    vals1[1] <- input$n_III
    r <- input$r_III
    
    if (input$useK) {
      k <- input$k_III
    } else {
      k <- 2 ^ 31
    }
    
    # p values
    vals2 <- vector()
    vals2[1] <- input$p_III
    a <- input$a_III
    c <- input$c_III
    m <- input$m_III
    
    # loop
    for (i in 1:length(series)) {
      vals1[i + 1] <- vals1[i] + ((r * vals1[i] * (1 - vals1[i] / k)) -
                                    (c * vals1[i] * vals2[i])) * minstep
      vals2[i + 1] <- vals2[i] + ((a * c * vals1[i] * vals2[i]) -
                                    (m * vals2[i])) * minstep
      if (vals1[i + 1] <= 0)
        vals1[i + 1] <- 0
      if (vals2[i + 1] <= 0)
        vals2[i + 1] <- 0
    }
    vv1 <- vals1[1:(length(vals1) - 1)]
    vv2 <- vals2[1:(length(vals2) - 1)]
    
    pp <- data.frame(step = series, n = vv1, p = vv2)
    
    
    # plot
    ggplot(data = pp, aes(x = n, y = p)) +
      geom_point(data = pp[1, ],
                 size = 3,
                 color = "darkgreen") +
      geom_label(data = pp[1, ],
                 label = "start",
                 nudge_y = 1) +
      geom_point(
        data = pp[nrow(pp), ],
        size = 3,
        color = "darkgreen",
        shape = 1
      ) +
      geom_label(data = pp[nrow(pp), ],
                 label = "end",
                 nudge_y = 1) +
      geom_path(color = "darkgreen") +
      geom_segment(
        x = 0,
        y = r / c,
        xend = k,
        yend = 0,
        color = "green",
        linetype = "longdash"
      ) +
      geom_vline(
        xintercept = m / (a * c),
        color = "orange",
        linetype = "longdash"
      ) +
      theme_bw() +
      ylab("Predators (P)") +
      xlab("Prey (N)") +
      xlim(0, max(vv1) + 1) +
      ylim(0, max(vv2) + 1) +
      theme(text = element_text(size = 24))
  })
  
}

# run ####
shinyApp(ui = ui, server = server)

