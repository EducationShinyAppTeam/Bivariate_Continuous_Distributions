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
      title = "App Template", # You may use a shortened form of the title here
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
          p("This is a sample Shiny application for BOAST. Remember, this page
            will act like the front page (home page) of your app. Thus you will
            want to have this page catch attention and describe (in general terms)
            what the user can do in the rest of the app."),
          h2("Instructions"),
          p("This information will change depending on what you want to do."),
          tags$ol(
            tags$li("Review any prerequiste ideas using the Prerequistes tab."),
            tags$li("Explore the Exploration Tab."),
            tags$li("Challenge yourself."),
            tags$li("Play the game to test how far you've come.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "go1",
              label = "GO!",
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
            "This version of the app was developed and coded by Neil J.
            Hatfield  and Robert P. Carey, III.",
            br(),
            "We would like to extend a special thanks to the Shiny Program
            Students.",
            br(),
            br(),
            "Cite this app as:",
            br(),
            citeApp(),
            br(),
            br(),
            div(class = "updated", "Last Update: 9/1/2024 by NP.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("In order to get the most out of this app, please review the
            following:"),
          tags$ul(
            tags$li("Pre-req 1--Technical/Conceptual Prerequisites are ideas that
                    users need to have in order to engage with your app fully."),
            tags$li("Pre-req 2--Contextual Prerequisites refer to any information
                    about a context in your app that will enrich a user's
                    understandings."),
            tags$li("Pre-req 3"),
            tags$li("Pre-req 4")
          ),
          p("Notice the use of an unordered list; users can move through the
            list any way they wish."),
          box(
            title = strong("Null Hypothesis Significance Tests (NHSTs)"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            "In the Confirmatory Data Analysis tradition, null hypothesis
            significance tests serve as a critical tool to confirm that a
            particular theoretical model describes our data and to make a
            generalization from our sample to the broader population
            (i.e., make an inference). The null hypothesis often reflects the
            simpler of two models (e.g., 'no statistical difference',
            'there is an additive difference of 1', etc.) that we will use to
            build a sampling distribution for our chosen estimator. These
            methods let us test whether our sample data are consistent with this
            simple model (null hypothesis)."
          ),
          box(
            title = strong(tags$em("p"), "-values"),
            status = "primary",
            collapsible = TRUE,
            collapsed = FALSE,
            width = '100%',
            "The probability that our selected estimator takes on a value at
            least as extreme as what we observed given our null hypothesis. If
            we were to carry out our study infinitely many times and the null
            hypothesis accurately modeled what we're studying, then we would
            expect for our estimator to produce a value at least as extreme as
            what we have seen 100*(p-value)% of the time. The larger the
            p-value, the more often we would expect our estimator to take on a
            value at least as extreme as what we've seen; the smaller, the less
            often."
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
          br(),
          p(tags$strong("Standard Normally Distributed 3d Density Graph")),
          fluidRow(
            column(
              width = 8,
              uiOutput("normPlot")
            )
          ),
          br(),
          br(),
          p(tags$strong("Normal Graph w/ Correlation + Contour Map")),
          fluidRow(
            column(
              width = 6,
              uiOutput("corrPlot")
            ),
            column(
              width = 6,
              plotOutput("contourMap")
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                sliderInput(
                  inputId = 'correlationSlider',
                  label = 'Adjust the slider to change the correlation value p',
                  min = -0.9,
                  max = 0.9,
                  value = 0,
                  step = 0.01
                ))
            )
          )
        ),
        #### Set up conditional Explore Page ----
        tabItem(
          tabName = "explore2",
          withMathJax(),
          h2("Conditioning"),
          br(),
          p(tags$strong("Working with Conditional Plane (Std. Norm)")),
          fluidRow(
            column(
              width = 6,
              uiOutput("condPlot3d")
            ),
            column(
              width = 6,
              plotOutput('condPlane')
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                sliderInput(
                  inputId = 'condSlider',
                  label = 'Adjust the slider to move the positioning of the conditional plane',
                  min = -2.5,
                  max = 2.5,
                  value = 0,
                  step = 0.1
                ))
            )
          ),
          br(),
          br(),
          p(tags$strong("Conditional Plane w/ Correlation Value (p = 0.7)")),
          fluidRow(
            column(
              width = 6,
              uiOutput("condCorr")
            ),
            column(
              width = 6,
              plotOutput('condCorrPlane')
            )
          ),
          br(),
          fluidRow(
            column(
              width = 6,
              wellPanel(
                sliderInput(
                  inputId = 'condSlider2',
                  label = 'Adjust the slider to move the positioning of the conditional plane',
                  min = -2.5,
                  max = 2.5,
                  value = 0,
                  step = 0.1
                ))
            )
          )
        ),
        #### Set up a Challenge Page ----
        tabItem(
          tabName = "challenge",
          withMathJax(),
          h2("Challenge Yourself"),
          p("The general intent of a Challenge page is to have the user take
            what they learned in an Exploration and apply that knowledge in new
            contexts/situations. In essence, to have them challenge their
            understanding by testing themselves."),
          p("What this page looks like will be up to you. Something you might
            consider is to re-create the tools of the Exploration page and then
            a list of questions for the user to then answer.")
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

  ## Set up Info button ----
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
  
  ## Create Explore Page Graphs ----
  
  # create standard norm functions
  joint_normal <- function(x,y) {
    (1 / (2 * pi)) * exp(-0.5 * (x^2 + y^2))
  }
  marg <- function(x) {
    (1 / sqrt(2 * pi)) * exp(-0.5 * x^2)
  }
  
  # create xy grid then expand to 3d and apply joint function
  x <- seq(-4, 4, length.out = 50)
  y <- seq(-4, 4, length.out = 50) 
  grid <- expand.grid(x = x,y = y)
  grid$z <- joint_normal(grid$x, grid$y)
  z <- matrix(grid$z, nrow = length(x), ncol = length(y))
  
  #### normal 3d plot ----
  output$normPlot <- renderUI({
    plotlyObj <- plot_ly(x = x, y = y, z = z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(
        zaxis = list(title = "Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-4,4,by = 2), ticktext = as.character(seq(-4,4,by = 2))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-4,4,by = 2), ticktext = as.character(seq(-4,4,by = 2))),
        camera = list(eye = list(x = -2.25, y = 0, z = 0))
      ),
      dragmode = FALSE) %>%
      # add marginal paths and scale
      add_paths(x = x, y = -4, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black')) %>%
      add_paths(x = -4, y = y, z = (1 / sqrt(2 * pi)) * marg(y), hovertext = "Marginal PDF of Y", name = 'Marginal PDF of Y', line = list(color = 'black'))
    config(plotlyObj, displaylogo = FALSE, displayModeBar = TRUE,
           modeBarButtonsToRemove = list('orbitRotation', 'tableRotation', 'pan3d', 'resetCameraLastSave3d', 'toImage'))
  })
  
  # create correlated pdf function and grid
  corr_joint <- function(x,y,p) {
    (1 / ((2 * pi) * sqrt(1-p^2))) * exp(-0.5 * (x^2 + y^2 - 2*p * x * y) / (1-p^2))
  }
  
  #### normal dist. w/ correlation value ----
  output$corrPlot <- renderUI({
    corr_grid <- expand.grid(x = x,y = y)
    corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
    corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
    plot_ly(x = x, y = y, z = corr_z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(zaxis = list(title = "Density")))
  })
  
  # create contour map
  output$contourMap <- renderPlot({
    corr_grid <- expand.grid(x = x,y = y)
    corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
    corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
    filled.contour(x,y,corr_z, asp = 1, color.palette = viridis,
                   plot.title = title(main = "Contour Map", xlab = 'x', ylab = 'y'))
  })
  
  
  #### conditional plot w/ plane ----
  output$condPlot3d <- renderUI({
    plotlyObj <- plot_ly(x = x, y = y, z = z, type = 'surface', showscale = FALSE, opacity = 0, colorscale = list(c(0, 1), c(boastPalette[8], boastPalette[8])),
                         hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(
        zaxis = list(title = "Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-4,4,by = 2), ticktext = as.character(seq(-4,4,by = 2))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-4,4,by = 2), ticktext = as.character(seq(-4,4,by = 2)))
      ))
    # create and add conditional plane
    x_val <- matrix(input$condSlider, nrow = length(y), ncol = length(z))
    z[z >= joint_normal(input$condSlider, y)] <- NA
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
                                             x = list(show = TRUE, color = 'grey30', width = 1, start = -4, end = 4, size = 0.4),
                                             y = list(show = TRUE, color = 'grey30', width = 1, start = -4, end = 4, size = 0.4)
                                           ))
  })
  
  # plane subplot (std. norm)
  output$condPlane <- renderPlot({
    cond_z <- joint_normal(input$condSlider,y) / marg(input$condSlider)
    ggplotObj <- ggplot(data = data.frame(y = y, cond_z = cond_z), 
                        mapping = aes(x = y, y = cond_z)) +
      geom_line()
    ggplotObj
  })
  
  
  
  #### conditional w/ correlation ----
  output$condCorr <- renderUI({
    grid2 <- expand.grid(x = x,y = y)
    grid2$z <- corr_joint(grid2$x, grid2$y, p = 0.7)
    z <- matrix(grid2$z, nrow = length(x), ncol = length(y))
    plotlyObj <- plot_ly(x = x, y = y, z = z, type = 'surface', showscale = FALSE, opacity = 0, colorscale = list(c(0, 1), c(boastPalette[8], boastPalette[8])),
                         hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(
        zaxis = list(title = "Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-4,4,by = 2), ticktext = as.character(seq(-4,4,by = 2))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-4,4,by = 2), ticktext = as.character(seq(-4,4,by = 2)))
      ))
    # create and add conditional plane
    x_val <- matrix(input$condSlider2, nrow = length(y), ncol = length(z))
    z[z >= corr_joint(input$condSlider2, y, p = 0.7)] <- NA
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
                                             x = list(show = TRUE, color = 'grey30', width = 1, start = -4, end = 4, size = 0.4),
                                             y = list(show = TRUE, color = 'grey30', width = 1, start = -4, end = 4, size = 0.4)
                                           ))
  })
  
  # plane subplot (w/ correlation)
  output$condCorrPlane <- renderPlot({
    cond_z <- corr_joint(input$condSlider2,y, p = 0.7) / marg(input$condSlider2)
    ggplotObj <- ggplot(data = data.frame(y = y, cond_z = cond_z), 
                        mapping = aes(x = y, y = cond_z)) +
      geom_line()
    ggplotObj
  })


}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)
