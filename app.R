# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(DT)
library(ggplot2)
library(tidyr)
library(plotly)
library(MASS)
library(graphics)
library(viridis)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Bivariate Continuous", # You may use a shortened form of the title here
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "App_Template")
      ),
      tags$li(
        class = "dropdown",
        tags$a(
          id = "home",
          href = 'https://shinyapps.science.psu.edu/',
          icon("house")
        )
      )
    ),
    ### Create the sidebar/left navigation menu ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("gauge-high")),
        menuItem("Prerequisites", tabName = "prerequisites", icon = icon("book")),
        menuItem("Joint vs Marginal", tabName = "explore1", icon = icon("wpexplorer")),
        menuItem("Conditioning", tabName = "explore2", icon = icon("wpexplorer")),
        menuItem("Challenge", tabName = "challenge", icon = icon("gears")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    ### Create the content ----
    dashboardBody(
      tabItems(
        #### Set up the Overview Page ----
        tabItem(
          tabName = "overview",
          withMathJax(),
          h1("Bivariate Continuous Distributions"), # This should be the full name.
          p("This app will explore bivariate continuous probability distributions in a real-world sense
            by examining joint vs marginal Probability Density Functions (PDFs) along with the conditional PDF.
            It will also explore ideas of independence and parameter adjustments to see how the PDFs respond."),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Navigate to the Joint vs Marginal tab to work with interactive 3D
                    graphs of the joint and marginal PDFs with the button below."),
            tags$li("Continue on to the Conditioning tab to explore representations
                    of conditional and joint PDFs to see how they compare."),
            tags$li("Test your knowledge on the Challenge tab by answering questions
                    about material involving joint, marginal, and conditional distributions.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "exploreButton",
              label = "Explore",
              size = "large",
              icon = icon("bolt"),
              style = "default"
            )
          ),
          ##### Create two lines of space
          br(),
          br(),
          h2("Acknowledgements"),
          p(
            "This version of the app was developed and coded by Nathan Pechulis. 
            Special thanks to Neil J. Hatfield and Dennis Pearl for their help
            with programming and content.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/22/2024 by NP.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Here, you can review some topics involved in bivariate continuous distributions,
            if necessary. Simply click on the plus sign on each tab to expand it."),
          br(),
          box(
            title = strong("Distribution Information and Notation"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            p(tags$strong("Distributions:"), 'A probability distribution is a function that 
              maps the probability for all potential events associated with a random variable 
              (function that assigns a number to each outcome in any event). Distributions can 
              be either discrete, where the data takes on only specific values (like shoe size 
              that comes in half or full size increments), or continuous, where the data can take 
              on any value within a certain interval (like volume or mass). But, in this app, we 
              will be exclusively examining continuous distributions. ',
              br(),
              br(),
              tags$strong("PDF:"), 'The probability mass function, or PDF, of a continuous distribution
              describes how proabability is distributed over the values of a random variable.',
              br(),
              br(),
              tags$strong("Expected Value:"), "The expected value for a random variable measures its central
              tendency and is a synonym for the mean. It is denoted by 'E(X)' and it can be calculated
              by integrating over the possible range of the random variable.",
              br(),
              br(),
              tags$strong("Notation:"), "Distribution notation typically looks like this:",
              tags$em('X'), "~ Distribution(p)",
              br(),
              'where', tags$em('X'), "is the random variable, ~ means 'distributed as', 'Distribution'
              represents the type of named distribution of the variable, and p represents any parameters
              necessary for the distribution."
            )
          ),
          box(
            title = strong("Marginal Distributions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "fill in later"
          ),
          box(
            title = strong("Joint Distributions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "fill in later"
          ),
          box(
            title = strong("Conditional Distributions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "fill in later"
          ),
          box(
            title = strong("Independence"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "fill in later"
          ),
          box(
            title = strong("Interpreting Plots"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "fill in later"
          )
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up joint vs marginals Page ----
        tabItem(
          tabName = "explore1",
          withMathJax(),
          h2("Joint vs Marginal PDFs"),
          p('This explore page features the 3D joint PDF of two independent standard normal
            random variables with some correlation value', tags$em('p'), 'that can be
            adjusted using the slider. The joint PDF graph also features the normalized
            marginal PDFs of each random variable. The graph on the bottom is a contour plot that shows
            an aerial view of the spread of the joint distribution as the correlation slider
            is adjusted. Use the slider to adjust the correlation value and see how both plots respond,
            then use the buttons to change between the marginal perspectives or reset the view.'),
          p(tags$strong('Guiding Question: How do the marginal PDFs change as the correlation value is adjusted
                        and what does that tell you about the Bivariate Normal distribution?')),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = 'correlationSlider',
                  label = p('Correlation value', tags$em('p')),
                  min = -0.9,
                  max = 0.9,
                  value = 0,
                  step = 0.01),
                bsButton(
                  inputId = "marg_y", 
                  label = "Marginal of Y", 
                  size = "large"
                ),
                bsButton(
                  inputId = "marg_x", 
                  label = "Marginal of X", 
                  size = "large"
                ),
                bsButton(
                  inputId = "defaultView", 
                  label = "Reset View", 
                  size = "large"
                )
              )
            ),
            column(
              width = 8,
              uiOutput("normPlot"),
              align = 'center',
              plotOutput("contourMap", width = "60%")
            )
          )
        ),
        #### Set up conditional Explore Page ----
        tabItem(
          tabName = "explore2",
          withMathJax(),
          h2("Conditioning"),
          p("This explore page features a 3D graph of the joint PDF with a conditional slice cutting through it and a 2D graph 
          of the conditional PDF below it. The conditional value", tags$em('p') ,"and the positioning of the conditioning plane 
          can be adjusted using the sliders on the left."),
          p(tags$strong('Guiding Question: How does the conditional PDF respond when the conditoning
                        plane is shifted with and without a correlation value?')),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = 'corrVal',
                  label = p('Correlation value', tags$em('p')),
                  min = -0.9,
                  max = 0.9,
                  value = 0,
                  step = 0.01),
                br(),
                sliderInput(
                  inputId = 'condSliderPos',
                  label = p('Conditional plane (X =', tags$em('x)')),
                  min = -2.5,
                  max = 2.5,
                  value = 0,
                  step = 0.1)
              )
            ),
            column(
              width = 8,
              uiOutput("condCorr"),
              align = 'center',
              plotOutput('condCorrPlane', width = '60%')
            )
          )
        ),
        #### Set up a Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("This page will have single questions and the user can move onto
            the next level when they get it right (instead of having one large quiz).
            I think the challenge will consist of maybe five questions from a question
            bank of maybe ten or so general questions.")
        ),
        #### Set up the References Page ----
        tabItem(
          tabName = "references",
          withMathJax(),
          h2("References"),
          p(
            class = "hangingindent",
            "Bailey, E. (2015). shinyBS: Twitter bootstrap components for shiny.
            (v0.61). [R package]. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  ## Set up Info and Explore buttons ----
  observeEvent(
    eventExpr = input$info,
    handlerExpr = {
      sendSweetAlert(
        session = session,
        type = "info",
        title = "Information",
        text = "This App Template will help you get started building your own app"
      )
    }
  )
  
  observeEvent(
    eventExpr = input$exploreButton,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "explore1"
      )
    })
  
  
  ## Create Explore Page Graphs ----
  
  # create norm functions
  joint_normal <- function(x,y) {
    (1 / (2 * pi)) * exp(-0.5 * (x^2 + y^2))
  }
  marg <- function(x) {
    (1 / sqrt(2 * pi)) * exp(-0.5 * x^2)
  }
  
  # create correlated pdf function and grid
  corr_joint <- function(x,y,p) {
    (1 / ((2 * pi) * sqrt(1-p^2))) * exp(-0.5 * (x^2 + y^2 - 2*p * x * y) / (1-p^2))
  }
  
  
  # create xy grid then expand to 3d and apply joint function
  x <- seq(-3, 3, length.out = 50)
  y <- seq(-3, 3, length.out = 50) 
  grid <- expand.grid(x = x,y = y)
  grid$z <- joint_normal(grid$x, grid$y)
  z <- matrix(grid$z, nrow = length(x), ncol = length(y))
  
  #### 3d plot w/ correlation----
  output$normPlot <- renderUI({
    corr_grid <- expand.grid(x = x,y = y)
    corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
    corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
    plotlyObj <- plot_ly(x = x, y = y, z = corr_z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF", colorscale = 'Jet') %>%
      layout(scene = list(
        zaxis = list(title = "Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2)))
      ),
      dragmode = FALSE) %>%
      # add marginal paths and scale
      add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black')) %>%
      add_paths(x = -3, y = y, z = (1 / sqrt(2 * pi)) * marg(y), hovertext = "Marginal PDF of Y", name = 'Marginal PDF of Y', line = list(color = 'black'))
    config(plotlyObj, displaylogo = FALSE, displayModeBar = FALSE)
  })
  
  #### Button Views ----
  # marginal of y view
  observeEvent(
    eventExpr = input$marg_y, 
    handlerExpr = {
      output$normPlot <- renderUI({
        corr_grid <- expand.grid(x = x,y = y)
        corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
        corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
        
        plotlyObj <- plot_ly(x = x, y = y, z = corr_z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF", colorscale = 'Jet') %>%
          layout(scene = list(
            zaxis = list(title = "Density", hoverformat = '.3f'),
            xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
            yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
            camera = list(eye = list(x = -2.25, y = 0, z = -0.5))
          ),
          dragmode = FALSE) %>%
          # add marginal paths and scale
          add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black')) %>%
          add_paths(x = -3, y = y, z = (1 / sqrt(2 * pi)) * marg(y), hovertext = "Marginal PDF of Y", name = 'Marginal PDF of Y', line = list(color = 'black'))
        config(plotlyObj, displaylogo = FALSE, displayModeBar = FALSE)
      })
    })
  
  # marginal of x view
  observeEvent(
    eventExpr = input$marg_x, 
    handlerExpr = {
      output$normPlot <- renderUI({
        corr_grid <- expand.grid(x = x,y = y)
        corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
        corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
        
        plotlyObj <- plot_ly(x = x, y = y, z = corr_z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF", colorscale = 'Jet') %>%
          layout(scene = list(
            zaxis = list(title = "Density", hoverformat = '.3f'),
            xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
            yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
            camera = list(eye = list(x = 0, y = -2.25, z = -0.5))
          ),
          dragmode = FALSE) %>%
          # add marginal paths and scale
          add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black')) %>%
          add_paths(x = -3, y = y, z = (1 / sqrt(2 * pi)) * marg(y), hovertext = "Marginal PDF of Y", name = 'Marginal PDF of Y', line = list(color = 'black'))
        config(plotlyObj, displaylogo = FALSE, displayModeBar = FALSE)
      })
    })
  
  # Default view button
  observeEvent(
    eventExpr = input$defaultView,
      handlerExpr = {
        output$normPlot <- renderUI({
          corr_grid <- expand.grid(x = x,y = y)
          corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
          corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
          plotlyObj <- plot_ly(x = x, y = y, z = corr_z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF", colorscale = 'Jet') %>%
            layout(scene = list(
              zaxis = list(title = "Density", hoverformat = '.3f'),
              xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
              yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2)))
            ),
            dragmode = FALSE) %>%
            # add marginal paths and scale
            add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black')) %>%
            add_paths(x = -3, y = y, z = (1 / sqrt(2 * pi)) * marg(y), hovertext = "Marginal PDF of Y", name = 'Marginal PDF of Y', line = list(color = 'black'))
          config(plotlyObj, displaylogo = FALSE, displayModeBar = FALSE)
        })
    }
  )
  
  
  
  # create contour map
  output$contourMap <- renderPlot({
    corr_grid <- expand.grid(x = x,y = y)
    corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
    corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
    filled.contour(x,y,corr_z, asp = 1, color.palette = colorRampPalette(c("darkblue", "cyan", "yellow", "red")),
                   plot.title = title(main = "Contour Map", xlab = 'x', ylab = 'y'))
  })
  
  
  #### conditional w/ correlation ----
  output$condCorr <- renderUI({
    grid2 <- expand.grid(x = x,y = y)
    grid2$z <- corr_joint(grid2$x, grid2$y, p = input$corrVal)
    z <- matrix(grid2$z, nrow = length(x), ncol = length(y))
    plotlyObj <- plot_ly(x = x, y = y, z = z, type = 'surface', showscale = FALSE, opacity = 0, colorscale = list(c(0, 1), c(boastPalette[8], boastPalette[8])),
                         hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(
        zaxis = list(title = "Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 1), ticktext = as.character(seq(-3,3,by = 1))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 1), ticktext = as.character(seq(-3,3,by = 1)))
      ),
      dragmode = FALSE)
    # create and add conditional plane
    x_val <- matrix(input$condSliderPos, nrow = length(y), ncol = length(z))
    z[z >= corr_joint(input$condSliderPos, y, p = input$corrVal)] <- NA
    plotlyObj <- plotlyObj %>% add_surface(
      x = x_val,
      y = y,
      z = z,
      type = 'surface',
      opacity = 1,
      showscale = FALSE,
      hovertext = "Conditional Plane")
    # add mesh effect
    plotlyObj <- plotlyObj %>% add_surface(x = ~x, y = ~y, opacity = 0, showscale = FALSE, 
                                           contours = list(
                                             x = list(show = TRUE, color = 'grey30', width = 1, start = -3, end = 3, size = 0.5),
                                             y = list(show = TRUE, color = 'grey30', width = 1, start = -3, end = 3, size = 0.5)
                                           ))
    config(plotlyObj, displaylogo = FALSE, displayModeBar = FALSE)
  })
  
  # plane subplot
  output$condCorrPlane <- renderPlot({
    x <- seq(-3, 3, length.out = 125)
    y <- seq(-3, 3, length.out = 125) 
    cond_z <- corr_joint(input$condSliderPos,y, p = input$corrVal) / marg(input$condSliderPos)
    ggplotObj <- ggplot(data = data.frame(y = y, cond_z = cond_z), 
                        mapping = aes(x = y, y = cond_z)) +
      geom_line()
    ggplotObj
  })


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

