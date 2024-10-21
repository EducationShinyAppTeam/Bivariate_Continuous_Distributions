# Load Packages ----
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(boastUtils)
library(ggplot2)
library(plotly)
library(graphics)

# Load additional dependencies and setup functions
# source("global.R")

# Define UI for App ----
ui <- list(
  ## Create the app page ----
  dashboardPage(
    skin = "blue",
    ### Create the app header ----
    dashboardHeader(
      title = "Bivariate Distributions", # You may use a shortened form of the title here
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
        menuItem("Joint and Marginal", tabName = "explore1", icon = icon("wpexplorer")),
        menuItem("Conditioning", tabName = "explore2", icon = icon("wpexplorer")),
        #menuItem("Challenge", tabName = "challenge", icon = icon("gears")),
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
          p("This app will explore bivariate continuous probability distributions by examining the relationship
          between joint and marginal density functions along with the conditional independence.
            "),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequisite ideas using the Prerequisites tab."),
            tags$li("Navigate to the Joint and Marginal tab to work with interactive 3D
                    graphs of the joint and marginal PDFs with the button below."),
            tags$li("Continue on to the Conditioning tab to explore representations
                    of conditional and joint PDFs to see how they compare.")
            # tags$li("Test your knowledge on the Challenge tab by answering questions
            #         about material involving joint, marginal, and conditional distributions.")
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
            div(class = "updated", "Last Update: 10/21/2024 by NP.")
          )
        ),
        #### Set up the Prerequisites Page ----
        tabItem(
          tabName = "prerequisites",
          withMathJax(),
          h2("Prerequisites"),
          p("Here, you can review some topics involved in bivariate continuous distributions,
            if necessary. Simply click on the plus/minus sign on each tab to expand/minimize them."),
          br(),
          box(
            title = strong("Distribution Information"),
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
              tags$strong("PDF:"), 'A probability density function, or PDF, of a continuous distribution
              provides probability per unit of', tags$em('x'), 'over the values of a random variable, thus it integrates to 1 over the whole range.',
              br(),
              br(),
              tags$strong("Expected Value:"), "The expected value for a random variable measures its central
              tendency and is a synonym for the mean. It is denoted by 'E(X)' and it can be calculated
              by integrating", tags$em('x') ,"times f(x) over the possible range of the random variable.",
              br(),
              br()
            )
          ),
          box(
            title = strong("Joint Distributions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(tags$strong('Definition:'), "A joint density function describes the probability distribution of two or more variables simultaneously. 
            If you were to integrate over all values of the variables, it would equal the total probability space,
              which is 1. The joint PDF of two variables X and Y can be denoted: \\[f_{X,Y}(x,y)\\]",
              br(),
              tags$strong("Bivariate Normal:"), 'For the Bivariate Normal setting, both X and Y are normally distributed with', HTML("&#x03C1;"), 'being the correlation value.',
              'The joint PDF of two random variables that follow a Bivariate Normal distribution is: 
              $$f_{X,Y}(x,y) = \\frac{1}{2\\pi \\sigma_X \\sigma_Y \\sqrt{1 - \\rho^2}} \\exp\\left(-\\frac{1}{2(1 - \\rho^2)}\\left(\\frac{(x - \\mu_X)^2}{\\sigma_X^2} + \\frac{(y - \\mu_Y)^2}{\\sigma_Y^2} - \\frac{2\\rho (x - \\mu_X)(y - \\mu_Y)}{\\sigma_X \\sigma_Y}\\right)\\right)$$')
          ),
          box(
            title = strong("Marginal Distributions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(tags$strong("Definition:"), "Marginal distributions describe the probability distribution of one of the variables
              within a multivariate setting. For continuous random variables, it is found by integrating the joint PDF with respect 
              to the other variable(s) to remove them from the equation. For example, the marginal PDF formula of X is:
              \\[f_X(x)=\\int_{-\\infty}^{\\infty} f_{X,Y}(x,y)dy\\]",
              br(),
              tags$strong("Normal Distribution:"), 'When it comes to the Bivariate Normal setting, the marginal distribution
            is also normally distributed. The formula for the marginal PDF of X in a Bivariate Normal distribution is: 
              \\[f_X(x) = \\frac{1}{\\sigma_X \\sqrt{2\\pi}} \\exp\\left(-\\frac{(x-  \\mu_X)^2}{2\\sigma_X^2}\\right)\\]'
            )
          ),
          box(
            title = strong("Conditional Distributions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(tags$strong('Definition:'), 'Conditioning about one of the variables in a joint distribution involves analyzing the behavior
              of one of the variables while the other one is being held at a specific value. For example, if we were conditioning about a specific', tags$em('x'),
              'value, the conditional PDF formula of Y given (X =', tags$em('x'), ') would be as follows:
              $$f_{Y|X}(y|X = x) = \\frac{f_{X,Y}(x,y)}{f_X(x)}$$')
          ),
          box(
            title = strong("Independence"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(tags$strong('Definition:'), 'Independence, when it comes to a bivariate distribution, refers to the situation
              where neither random variable has any influence on the other. Two variables X and Y are independent when their joint distribution 
              can be factored into the PDF function of X times the PDF function of Y which becomes: \\[f_{X,Y}(x,y) = f_X(x) * f_Y(y)\\]',
              'Conditioning can also help us to determine variable interdependence. For instance, when two variables are independent, the conditional and marginal PDFs are equal. 
              This is represented below: \\[f_{Y|X}(y|X = x) = f_Y(y)\\]')
          )
        ),
        #### Note: you must have at least one of the following pages. You might
        #### have more than one type and/or more than one of the same type. This
        #### will be up to you and the goals for your app.
        #### Set up joint and marginals Page ----
        tabItem(
          tabName = "explore1",
          withMathJax(),
          h2("Joint and Marginal PDFs"),
          tabsetPanel(
            type = 'tabs',
            tabPanel(
              title = 'Normal',
              br(),
              p('This explore page features the two-dimensional PDF function of two independent standard normal
            random variables as a 3D plot with some correlation value,', HTML("&#x03C1;"), ', that can be
            adjusted using the slider. The joint PDF graph also features the normalized
            marginal PDFs of each random variable. The graph on the bottom is a contour plot that shows
            an aerial view of the spread of the joint distribution as the correlation slider
            is adjusted. Use the slider to adjust the correlation value and see how both plots respond,
            then use the buttons to change between the marginal and joint perspectives.'),
              p(tags$strong('Guiding Questions: How do the marginal and joint PDFs change as the correlation value is adjusted?
                        What does that tell you about the Bivariate Normal distribution?')),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = 'correlationSlider',
                      label = p('Correlation value,', HTML("&#x03C1;")),
                      min = -0.9,
                      max = 0.9,
                      value = 0,
                      step = 0.05),
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
                      label = "Joint X,Y View", 
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 8,
                  uiOutput("normPlot"),
                  p('3D Joint PDF plot where the color represents the value of the joint density according to the color scale. The marginal
                density scales would peak at 1/root(2π) ≈ 0.399'),
                  align = 'center',
                  plotOutput("contourMap", width = "60%"),
                  p('Contour plot that depicts an aerial view of the joint distribution spread of X and Y')
                )
              )
            ),
            tabPanel(
              title = 'Exponential/Gamma',
              br(),
              p('X = the number of minutes that the customer spends on the call including both waiting in the queue for service and ten receiving service by the IRS staff.', br(), ' 
                Y = the amount of time in the queue.', br(),
                'Thus,  X-Y is the amount of time they actually talk to the IRS staff which averages about 20 minutes at any time.', br(), 
                'Suppose the joint density function of X and Y is given by f(x,y) = 𝛌µ*exp(-(µ-𝛌)y-µx) for 0≤y≤x≤∞ (and = 0 otherwise)', br(), 
                'Here 1/µ is the mean time in the queue  (typically this average is around 2 or 3 minutes but can be higher on certain times of the day and much higher at certain times of the year, and 1/𝛌 is the mean time talking to IRS staff.'),
              fluidRow(
                column(
                  width = 4
                ),
                column(
                  width = 8,
                  uiOutput("expoPlot"),
                )
              )
            )
          )
        ),
        #### Set up conditional Explore Page ----
        tabItem(
          tabName = "explore2",
          withMathJax(),
          h2("Conditioning"),
          p("This explore page features a 3D graph of the joint PDF of X and Y with a conditional slice at a chosen value
          of", tags$em('x'), "cutting through it. The conditional PDF of Y given (X = ", tags$em('x'), ") is shown below.",
          "The value of", HTML("&#x03C1;") ,"in the joint density and the positioning of the conditioning plane 
          can be adjusted using the sliders on the left. Also utilize the play button below the plane positioning slider to see a moving animation
            of the conditional slice."),
          p(tags$strong('Guiding Question: How does the conditional PDF respond when the conditoning
                        plane slider is shifted with and without a correlation value?')),
          fluidRow(
            column(
              width = 4,
              wellPanel(
                sliderInput(
                  inputId = 'corrVal',
                  label = p('Correlation value,', HTML("&#x03C1;")),
                  min = -0.9,
                  max = 0.9,
                  value = 0,
                  step = 0.05),
                br(),
                sliderInput(
                  inputId = 'condSliderPos',
                  label = p('Conditional plane (X =', tags$em('x'), ')'),
                  min = -2.5,
                  max = 2.5,
                  value = 0,
                  step = 0.1,
                  animate = animationOptions(interval = 2000, playButton = icon('forward')))
              )
            ),
            column(
              width = 8,
              uiOutput("condCorr"),
              uiOutput('page2Caption1'),
              br(),
              align = 'center',
              plotOutput('condCorrPlane'),
              uiOutput('page2Caption2')
            )
          )
        ),
        #### Set up a Challenge Page ----
        # tabItem(
        #   tabName = "challenge",
        #   withMathJax(),
        #   h2("Challenge Yourself"),
        #   p("This page will have single questions and the user can move onto
        #     the next level when they get it right (instead of having one large quiz).
        #     I think the challenge will consist of maybe five questions from a question
        #     bank of maybe ten or so general questions.")
        # ),
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
          p(
            class = 'hangingindent',
            "Carey, R., Hatfield, N. (2023). boastUtils: BOAST utilities. 
            (v. 0.1.11.3). [R package]. Available from 
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(
            class = 'hangingindent',
            "Chang, W., Cheng, J., Allaire, J., Sievert, C., Schloerke. B., Xie, Y., Allen, J., McPherson, J., Dipert, A., and Borges, B. (2024). 
            shiny: Web application framework for R. (v. 1.8.1.1). [R package]. 
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          p(
            class = 'hangingindent',
            "Chang, W., and Borges Ribeiro, B. (2021). shinydashboard: 
            Create dashboards with 'Shiny'. (v. 0.7.2). [R package]. Available from 
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(
            class = 'hangingindent',
            "Perrier, V., Meyer, F., and Granjon, D. (2024). shinyWidgets: 
            Custom inputs widgets for shiny. (v. 0.8.6). [R package]. Available from 
            https://CRAN.R-project.org/package=shinyWidgets"
          ),
          p(
            class = 'hangingindent',
            "R Core Team. (2023). graphics: The R Graphics Package. (v. 4.3.1). 
            [R package]. Available from 
            https://cran.r-project.org/package=graphics"
          ),
          p(
            class = 'hangingindent',
            "Sievert, C. (2023). plotly: Create Interactive Web Graphics via 'plotly.js'. 
            (v. 4.10.1). [R package]. 
            Available from https://cran.r-project.org/package=plotly"
          ),
          p(
            class = 'hangingindent',
            "Wickham, H. (2016). ggplot2: Elegant graphics for data analysis.
            Springer-Verlag:New York. (v 3.3.6) [R package]. Available from
            https://ggplot2.tidyverse.org"
          ),
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
  
  # create joint/marg functions
  joint_normal <- function(x,y) {
    (1 / (2 * pi)) * exp(-0.5 * (x^2 + y^2))
  }
  marg <- function(x) {
    (1 / sqrt(2 * pi)) * exp(-0.5 * x^2)
  }
  
  # create correlated pdf function
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
        zaxis = list(title = "Joint Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2)))
      ),
      dragmode = FALSE,
      title = list(text = 'Joint PDF Plot',
                   font = list(size = 18),
                   x = 0.43,
                   y = 0.95)) %>%
      # add marginal paths and scale
      add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black', dash = 'dash')) %>%
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
          dragmode = FALSE,
          title = list(text = 'Joint PDF Plot',
                       font = list(size = 18),
                       x = 0.43,
                       y = 0.95)) %>%
          # add marginal paths and scale
          add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black', dash = 'dash')) %>%
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
          dragmode = FALSE,
          title = list(text = 'Joint PDF Plot',
                       font = list(size = 18),
                       x = 0.43,
                       y = 0.95)) %>%
          # add marginal paths and scale
          add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black', dash = 'dash')) %>%
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
            dragmode = FALSE,
            title = list(text = 'Joint PDF Plot',
                         font = list(size = 18),
                         x = 0.43,
                         y = 0.95)) %>%
            # add marginal paths and scale
            add_paths(x = x, y = -3, z = (1 / sqrt(2 * pi)) * marg(x), hovertext = "Marginal PDF of X", name = 'Marginal PDF of X', line = list(color = 'black', dash = 'dash')) %>%
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
                   plot.title = title(main = "Joint Contour Plot", cex.main = 1.4, xlab = 'x', ylab = 'y', cex.lab = 1.5),
                   plot.axes = {axis(1, cex.axis = 1.3); axis(2, cex.axis = 1.3)})
  })
  
  
  #### Expo tab ----
  
  # create functions
  expo_gamma <- function(x,y,lambda,mu) {
    ifelse(y <= x, lambda*mu*exp(-(mu - lambda)*y - mu*x), 0)
  }
  
  
  output$expoPlot <- renderUI({
    x <- seq(0, 30, length.out = 100)
    y <- seq(0, 10, length.out = 100) 
    expo_grid <- expand.grid(x = x,y = y)
    expo_grid$z <- expo_gamma(expo_grid$x, expo_grid$y, lambda = 1/20, mu = 1/3)
    expo_z <- matrix(expo_grid$z, nrow = length(x), ncol = length(y))
    plotlyObj <- plot_ly(x = x, y = y, z = expo_z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Expo/Gamma", colorscale = 'Jet')
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
      dragmode = FALSE,
      title = list(text = 'Joint PDF Plot',
                   font = list(size = 18),
                   x = 0.52,
                   y = 0.95))
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
    if (abs(input$corrVal) > 0.5){
      size_arg = 0.3
    } else{
      size_arg = 0.5
    }
    plotlyObj <- plotlyObj %>% add_surface(x = ~x, y = ~y, opacity = 0, showscale = FALSE, 
                                           contours = list(
                                             x = list(show = TRUE, color = 'grey30', width = 1, start = -3, end = 3, size = size_arg),
                                             y = list(show = TRUE, color = 'grey30', width = 1, start = -3, end = 3, size = size_arg)
                                           ))
    config(plotlyObj, displaylogo = FALSE, displayModeBar = FALSE)
  })
  
  # plane subplot
  output$condCorrPlane <- renderPlot({
    x <- seq(-4.5, 4.5, length.out = 125)
    y <- seq(-4.5, 4.5, length.out = 125) 
    cond_z <- corr_joint(input$condSliderPos,y, p = input$corrVal) / marg(input$condSliderPos)
    ggplotObj <- ggplot(data = data.frame(y = y, cond_z = cond_z), 
                        mapping = aes(x = y, y = cond_z)) +
      labs(
        title = HTML('Conditional PDF at X =', input$condSliderPos), 
        x = "y", y = "Density") +
      geom_line(color = boastPalette[8], linewidth = 1.25) +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)
    ) +
      scale_x_continuous(expand = expansion(mult = c(0,0), add = 0), limits = c(-4.5,4.5)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = c(0,0.03)))
  })
  
  
  # captions
  output$page2Caption1 <- renderUI({
    withMathJax(
    p('Wire frame diagram of the joint density of X,Y, the blue shaded region shows 
    the joint density when X =', input$condSliderPos,': \\[f_{X,Y}(',input$condSliderPos,', y)\\]'))
  })
  
  output$page2Caption2 <- renderUI({
    withMathJax(
      p('This plot represents the conditional PDF:
        \\[f_{Y|X}(y|', input$condSliderPos,') = \\frac{f_{X,Y}(',input$condSliderPos,',y)}{f_X(',input$condSliderPos,')}\\]')
    )
  })



}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

