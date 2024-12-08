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
      title = "Bivariate Distributions", 
      titleWidth = 250,
      tags$li(class = "dropdown", actionLink("info", icon("info"))),
      tags$li(
        class = "dropdown",
        boastUtils::surveyLink(name = "Bivariate_Continuous_Distributions")
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
          between joint, marginal, and conditional density functions.
            "),
          h2("Instructions"),
          tags$ol(
            tags$li("Review any prerequisite ideas using the Prerequisites tab, which you can access
                    using the button below."),
            tags$li("Navigate to the Joint and Marginal tab to work with interactive 3D
                    graphs of joint and marginal PDFs."),
            tags$li("Explore both the Normal and Exponential sections on the Joint and Marginal tab
                    to examine two distinctly different contexts with different parameters."),
            tags$li("Continue on to the Conditioning tab to explore representations
                    of conditional and joint PDFs to see how they compare for both contexts.")
            # tags$li("Test your knowledge on the Challenge tab by answering questions
            #         about material involving joint, marginal, and conditional distributions.")
          ),
          ##### Go Button--location will depend on your goals
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "prerequisitesButton",
              label = "Prerequisites",
              size = "large",
              icon = icon("book"),
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
            div(class = "updated", "Last Update: 12/9/2024 by NP.")
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
            If you were to integrate over all values of the variables, it would equal the total probability space over the sample space,
              which is 1. The joint PDF of two variables X and Y can be denoted: \\[f_{X,Y}(x,y)\\]",
              br(),
              tags$strong("Bivariate Normal:"), 'For the Bivariate Normal setting, both X and Y are normally distributed with \\( \\rho \\) being the correlation value.',
              'The joint PDF of two random variables that follow a Bivariate Normal distribution is: 
              $$f_{X,Y}(x,y) = \\frac{1}{2\\pi \\sigma_X \\sigma_Y \\sqrt{1 - \\rho^2}} \\exp\\left(-\\frac{1}{2(1 - \\rho^2)}\\left(\\frac{(x - \\mu_X)^2}{\\sigma_X^2} + \\frac{(y - \\mu_Y)^2}{\\sigma_Y^2} - \\frac{2\\rho (x - \\mu_X)(y - \\mu_Y)}{\\sigma_X \\sigma_Y}\\right)\\right)$$')
          ),
          box(
            title = strong("Marginal Distributions"),
            status = "primary",
            collapsible = TRUE,
            collapsed = TRUE,
            width = '100%',
            p(tags$strong("Definition:"), "Marginal distributions describe the probability distribution of one, or a subset of the variables,
              within a multivariate setting. For continuous random variables, it is found by integrating the joint PDF with respect 
              to the other variable(s) to remove them from the equation. For example, the marginal PDF of X in the bivariate setting is:
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
              p('This explore page features the two-dimensional PDF function of two standard normal
            random variables as a 3D plot with some correlation value, \\( \\rho \\), that can be
            adjusted using the slider. The joint PDF graph also features the non-standardized
            marginal PDFs of each random variable. The graph on the bottom is a contour plot that shows
            an aerial view of the joint distribution as the correlation slider
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
                      label = p('Correlation value, \\( \\rho \\)'),
                      min = -0.9,
                      max = 0.9,
                      value = 0,
                      step = 0.05),
                    bsButton(
                      inputId = "marg_y", 
                      label = "Marginal PDF of Y", 
                      size = "large"
                    ),
                    bsButton(
                      inputId = "marg_x", 
                      label = "Marginal PDF of X", 
                      size = "large"
                    ),
                    bsButton(
                      inputId = "defaultView", 
                      label = "Joint X,Y PDF", 
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 8,
                  uiOutput("normPlot"),
                  uiOutput('page1Caption1'),
                  align = 'center',
                  plotOutput("contourMap", width = "100%"),
                  p('Contour plot that depicts an aerial view of the joint distribution of X and Y')
                )
              )
            ),
            tabPanel(
              title = 'Exponential',
              br(),
              withMathJax(
              p("The exponential section of this explore page features an alternative context with the joint density function of an exponential variable and the sum of two exponential variables, 
                X and Y in a real-world setting. The top plot is the joint density function of the two variables and the contour plot
                below it shows the aerial view of the joint distribution. It deals with the time spent by customers who completed a call with the IRS, where X represents 
                the number of minutes that a customer spends on the call both waiting in the queue for service and then receiving
                service, and Y represents simply the amount of time in the queue. Thus,  X-Y is the amount of time they actually 
                talk to the IRS staff which averages about 20 minutes at any time. Suppose the joint density function of X and Y is 
                given by: \\[f_{X,Y}(x,y) = \\lambda \\mu \\exp\\left(-(\\mu - \\lambda)y - \\mu x)\\right) \\] \\[ \\text{for} \\ 0 \\leq y \\leq x \\leq \\infty \\quad \\text{(0 otherwise)}\\]
                Here \\( \\frac{1}{\\mu} \\) is the mean time in the queue (typically this average is around 2 or 3 minutes but can be higher 
                on certain times of the day and much higher at certain times of the year), and \\( \\frac{1}{\\lambda} \\) is the mean time talking to IRS staff.
                You can adjust these parameters using the sliders below and observe how both the joint and marginal PDFs respond.
                Be sure to utilize the buttons below the sliders to change between joint and marginal PDFs.")),
              p(tags$strong('Guiding Question: How is the constraint of the domain reflected in the graph of the joint PDF and the contour plot?')),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    sliderInput(
                      inputId = 'lambdaSlider',
                      label = p('Mean time talking to staff, \\( \\lambda \\) (mins)'),
                      min = 5,
                      max = 30,
                      value = 15,
                      step = 1),
                    br(),
                    sliderInput(
                      inputId = 'muSlider',
                      label = p('Mean time in queue, \\( \\mu \\) (mins)'),
                      min = 0.5,
                      max = 15,
                      value = 2,
                      step = 0.5),
                    br(),
                    
                    bsButton(
                      inputId = "expo_marg_y", 
                      label = "Marginal PDF of Y", 
                      size = "large"
                    ),
                    bsButton(
                      inputId = "expo_marg_x", 
                      label = "Marginal PDF of X", 
                      size = "large"
                    ),
                    bsButton(
                      inputId = "expo_defaultView", 
                      label = "Joint X,Y PDF", 
                      size = "large"
                    )
                  )
                ),
                column(
                  width = 8,
                  uiOutput("expoPlot"),
                  align = 'center',
                  plotOutput("contourMap2", width = "100%"),
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
          tabsetPanel(
            type = 'tabs',
            tabPanel(
              title = 'Normal',
              br(),
              p("This explore page features a 3D graph of the joint PDF of standard normal random variables X and Y (correlation \\( \\rho \\)) with a conditional slice at a chosen value
          of", tags$em('x'), "cutting through it. The conditional PDF of Y given (X = ", tags$em('x'), ") is shown below.",
                "The value of \\( \\rho \\) in the joint density and the positioning of the conditioning plane 
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
                      label = p('Correlation value, \\( \\rho \\)'),
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
                  ),
                  uiOutput('page2Caption1')
                ),
                column(
                  width = 8,
                  uiOutput("condCorr"),
                  br(),
                  align = 'center',
                  plotOutput('condCorrPlane')
                )
              )
            ),
            tabPanel(
              title = 'Exponential',
              br(),
              p("Now, let's begin conditioning based on the IRS exponential scenario from the Joint and Marginal page.
              The top plot is the joint density plot of X and Y with a blue conditional slice cutting through it that can be adjusted
              and the plot below it is the conditional PDF plot. Again, X represents the number of minutes that a customer 
              spends on the call both waiting in the queue for service and then receiving service, and Y represents the 
              amount of time in the queue. Utilize the checkbox to choose which variable to condition on and then adjust the 
                sliders to change parameter values along with the positioning of the conditional slice."),
              p(tags$strong('Guiding Question: How does the shape of the conditional PDF (bottom plot) change as the conditioning variable', tags$em('x'), 'or', tags$em('y'), 'gets swapped?')),
              fluidRow(
                column(
                  width = 4,
                  wellPanel(
                    selectInput(inputId = 'condition_x_or_y',
                                label = 'Choose which variable to condition',
                                choices = c("total time (x)" = 'x','queue time (y)' = 'y'),
                                selected = 'x'),
                    br(),
                    sliderInput(
                      inputId = 'lambdaSlider2',
                      label = p('Mean time talking to staff, \\( \\lambda \\) (mins)'),
                      min = 5,
                      max = 30,
                      value = 15,
                      step = 1),
                    br(),
                    sliderInput(
                      inputId = 'muSlider2',
                      label = p('Mean time in queue, \\( \\mu \\) (mins)'),
                      min = 0.5,
                      max = 15,
                      value = 2,
                      step = 0.5),
                    br(),
                    uiOutput("slider_update")
                  ),
                  uiOutput('page2Caption2')
                ),
                column(
                  width = 8,
                  uiOutput("condExpo"),
                  br(),
                  align = 'center',
                  uiOutput('condExpoPlane')
                )
              )
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
            "Anon. (2023). “Building on Filing Season 2023 Success, IRS Continues to 
            Improve Service, Pursue High-Income Individuals Evading Taxes, Modernize 
            Technology.”",tags$em('Internal Revenue Service.'), "Available at
            https://www.irs.gov/newsroom/building-on-filing-season-2023-success-irs-continues-to-improve-service-pursue-high-income-individuals-evading-taxes-modernize-technology"
          ),
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
        text = "Explore joint and marginal PDF plots
        on the Joint and Marginal page, and the interactive conditioning scenario on the 
        Conditioning page."
      )
    }
  )
  
  observeEvent(
    eventExpr = input$prerequisitesButton,
    handlerExpr = {
      updateTabItems(
        session = session,
        inputId = "pages",
        selected = "prerequisites"
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
  
  
  # create reactive elements for perspective changes
  view <- reactiveVal('3D')
  observeEvent(
    eventExpr = input$defaultView,
    handlerExpr = {
      view('3D')
    })
  observeEvent(
    eventExpr = input$marg_x,
    handlerExpr = {
      view('marg_x')
    })
  observeEvent(
    eventExpr = input$marg_y,
    handlerExpr = {
      view('marg_y')
    })
  
  output$normPlot <- renderUI({
    if (view() == "3D") {
      uiOutput('plotly_3d')
    } else if (view() == 'marg_x') {
      plotOutput('marginal_x')
    } else if (view() == 'marg_y') {
      plotOutput('marginal_y')
    }
  })
  
  # create xy grid then create grid and apply joint function
  x <- seq(-3, 3, length.out = 50)
  y <- seq(-3, 3, length.out = 50) 
  grid <- expand.grid(x = x,y = y)
  grid$z <- joint_normal(grid$x, grid$y)
  z <- matrix(grid$z, nrow = length(x), ncol = length(y))
  
  
  #### Joint vs Marginal Plots ----
  # 3d normal plot w/ correlation
  output$plotly_3d <- renderUI({
    corr_grid <- expand.grid(x = x,y = y)
    corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
    corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
    plotlyObj <- plot_ly(x = x, y = y, z = corr_z, type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF", colorscale = 'Jet') %>%
      layout(scene = list(
        zaxis = list(title = "Joint Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
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
  
  # normal - marginal of y view
  output$marginal_y <- renderPlot({
    y <- seq(-4.5, 4.5, length.out = 125)
    z <- marg(y)
    ggplotObj <- ggplot(data = data.frame(y = y, z = z), 
                        mapping = aes(x = y, y = z)) +
      labs(
        title = 'Marginal PDF of Y', 
        x = "y", y = "Marginal Density") +
      geom_line(color = 'black', linewidth = 1.25) +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)
      ) +
    scale_x_continuous(expand = expansion(mult = c(0,0), add = 0), limits = c(-4.5,4.5)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = c(0,0.03)))
  })
  
  # normal - marginal of x view
  output$marginal_x <- renderPlot({
    x <- seq(-4.5, 4.5, length.out = 125)
    z <- marg(x)
    ggplotObj <- ggplot(data = data.frame(x = x, z = z), 
                        mapping = aes(x = x, y = z)) +
      labs(
        title = 'Marginal PDF of X', 
        x = "x", y = "Marginal Density") +
      geom_line(color = 'black', linewidth = 1.25, linetype = 'dashed') +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)
    ) +
      scale_x_continuous(expand = expansion(mult = c(0,0), add = 0), limits = c(-4.5,4.5)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = c(0,0.03)))
  })
  
  
  # normal - create contour map
  output$contourMap <- renderPlot({
    corr_grid <- expand.grid(x = x,y = y)
    corr_grid$z <- corr_joint(grid$x, grid$y, input$correlationSlider)
    corr_z <- matrix(corr_grid$z, nrow = length(x), ncol = length(y))
    filled.contour(x,y,corr_z, asp = 1, color.palette = colorRampPalette(c("darkblue", "cyan", "yellow", "red")),
                   plot.title = title(main = "Joint PDF Contour Plot", cex.main = 1.4, xlab = 'x', ylab = 'y', cex.lab = 1.5),
                   plot.axes = {axis(1, cex.axis = 1.3); axis(2, cex.axis = 1.3)})
  })
  
  
  #### Exponential tab ----
  
  # create functions
  expo_gamma <- function(x,y,lambda,mu) {
    result <- lambda*mu*exp(-(mu*y + lambda*x - lambda*y))
    result[!(y <= x & x >= 0 & y >= 0)] <- 0
    return(result)
  }
  
  expo_y <- function(y,mu) {
    mu*exp(-(mu*y))
  }
  
  # need to make two separate cases for when lambda and mu are equal
  expo_x <- function(x, lambda, mu) {
    if (lambda == mu) {
      lambda*mu*x*exp(-mu*x)
    } else {
      (lambda*mu/(mu-lambda))*exp(-mu*x)*(1 - exp(-(mu-lambda)*x))
    }
  }
  
  
  # create reactive elements for perspective changes
  view2 <- reactiveVal('expo_3D')
  observeEvent(
    eventExpr = input$expo_defaultView,
    handlerExpr = {
      view2('expo_3D')
    })
  observeEvent(
    eventExpr = input$expo_marg_x,
    handlerExpr = {
      view2('expo_marg_x')
    })
  observeEvent(
    eventExpr = input$expo_marg_y,
    handlerExpr = {
      view2('expo_marg_y')
    })
  
  output$expoPlot <- renderUI({
    if (view2() == "expo_3D") {
      uiOutput('expo_3d')
    } else if (view2() == 'expo_marg_x') {
      plotOutput('expo_marginal_x')
    } else if (view2() == 'expo_marg_y') {
      plotOutput('expo_marginal_y')
    }
  })
  
  
  # create 3d expo plot
  output$expo_3d <- renderUI({
    x <- seq(-1,15, length.out = 125)
    y <- seq(-1,15, length.out = 125) 
    expo_grid <- expand.grid(x = x,y = y)
    expo_grid$z <- expo_gamma(expo_grid$x, expo_grid$y, lambda = 1/input$lambdaSlider, mu = 1/input$muSlider)
    expo_z <- matrix(expo_grid$z, nrow = length(x), ncol = length(y))
    # had to transpose z because of graph orientation w/ contour map
    plotlyObj <- plot_ly(x = x, y = y, z = t(expo_z), type = 'surface', hoverinfo = 'x+y+z+text', hovertext = "Joint PDF", colorscale = 'Jet') %>%
      layout(scene = list(
        zaxis = list(title = "Joint Density"),
        xaxis = list(title = 'Total time (x)'),
        yaxis = list(title = 'Queue time (y)'),
        camera = list(eye = list(x = 1.1, y = 2, z = 1.5))
      ),
      title = list(text = 'Joint PDF Plot',
                   font = list(size = 18),
                   x = 0.47,
                   y = 0.95)) %>%
      config(displaylogo = FALSE, displayModeBar = TRUE, scrollZoom = FALSE, 
             modeBarButtonsToRemove = c('toImage', "pan3d", "orbitRotation", "zoom3d", "resetCameraDefault"))
  })
  
  
  # expo - marginal of y view
  output$expo_marginal_y <- renderPlot({
    y <- seq(0,10, length.out = 125) 
    z <- expo_y(y, mu = 1/input$muSlider)
    ggplotObj <- ggplot(data = data.frame(y = y, z = z), 
                        mapping = aes(x = y, y = z)) +
      labs(
        title = 'Marginal PDF of Y', 
        x = "Queue time (y)", y = "Marginal Density") +
      geom_line(color = 'black', linewidth = 1.25) +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)
    ) +
      scale_x_continuous(expand = expansion(mult = c(0,0), add = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = 0))
  })
  
  
  # expo - marginal of x view
  output$expo_marginal_x <- renderPlot({
    x <- seq(0,10, length.out = 125) 
    z <- expo_x(x, lambda = 1/input$lambdaSlider, mu = 1/input$muSlider)
    ggplotObj <- ggplot(data = data.frame(x = x, z = z), 
                        mapping = aes(x = x, y = z)) +
      labs(
        title = 'Marginal PDF of X', 
        x = "Total time (x)", y = "Marginal Density") +
      geom_line(color = 'black', linewidth = 1.25) +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)
    ) +
      scale_x_continuous(expand = expansion(mult = c(0,0), add = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = 0))
  })
  
  
  # create expo contour map
  output$contourMap2 <- renderPlot({
    x <- seq(0,15, length.out = 125)
    y <- seq(0,15, length.out = 125) 
    expo_grid <- expand.grid(x = x,y = y)
    expo_grid$z <- expo_gamma(expo_grid$x, expo_grid$y, lambda = 1/input$lambdaSlider, mu = 1/input$muSlider)
    expo_z <- matrix(expo_grid$z, nrow = length(x), ncol = length(y))
    filled.contour(x,y,expo_z, asp = 1, color.palette = colorRampPalette(c("darkblue", "cyan", "yellow", "red")),
                   plot.title = title(main = "Joint PDF Contour Plot", cex.main = 1.4, xlab = 'Total time (x)', ylab = 'Queue time (y)', cex.lab = 1.5),
                   plot.axes = {axis(1, cex.axis = 1.3); axis(2, cex.axis = 1.3)})
  })
  
  
  
  #### conditional normal  ----
  output$condCorr <- renderUI({
    grid2 <- expand.grid(x = x,y = y)
    grid2$z <- corr_joint(grid2$x, grid2$y, p = input$corrVal)
    z <- matrix(grid2$z, nrow = length(x), ncol = length(y))
    plotlyObj <- plot_ly(x = x, y = y, z = z, type = 'surface', showscale = FALSE, opacity = 0, colorscale = list(c(0, 1), c(boastPalette[5], boastPalette[5])),
                         hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(
        zaxis = list(title = "Joint Density", hoverformat = '.3f'),
        xaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
        yaxis = list(hoverformat = '.3f', tickvals = seq(-3,3,by = 2), ticktext = as.character(seq(-3,3,by = 2))),
        camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
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
  
  
  # normal cond plane subplot
  output$condCorrPlane <- renderPlot({
    x <- seq(-4.5, 4.5, length.out = 125)
    y <- seq(-4.5, 4.5, length.out = 125) 
    cond_z <- corr_joint(input$condSliderPos,y, p = input$corrVal) / marg(input$condSliderPos)
    ggplotObj <- ggplot(data = data.frame(y = y, cond_z = cond_z), 
                        mapping = aes(x = y, y = cond_z)) +
      labs(
        title = HTML('Conditional PDF of Y with X Equal to', input$condSliderPos), 
        x = "y", y = "Conditional Density") +
      geom_line(color = boastPalette[5], linewidth = 1.25) +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)
    ) +
      scale_x_continuous(expand = expansion(mult = c(0,0), add = 0), limits = c(-4.5,4.5)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = c(0,0.03)))
  })
  
  
  # normal caption text
  output$page1Caption1 <- renderUI({
    if (view() == "3D") {
      uiOutput('joint_caption')
    } else if (view() == 'marg_x' | view() == 'marg_y') {
      uiOutput('marginal_caption')
    } 
  })
  
  
  
  #### Exponential Conditional Plots ----
  # update cond plot based on selectinput conditioning x/y
  output$condExpo <- renderUI({
    if (input$condition_x_or_y == 'x') {
      uiOutput('conditioning_x')
    } else if (input$condition_x_or_y == 'y') {
      uiOutput('conditioning_y')
    }
  })
  
  # joint plot when conditioning x
  output$conditioning_x <- renderUI({
    x <- seq(-1,15, length.out = 75)
    y <- seq(-1,15, length.out = 75) 
    expo_grid <- expand.grid(x = x,y = y)
    expo_grid$z <- expo_gamma(expo_grid$x, expo_grid$y, lambda = 1/input$lambdaSlider2, mu = 1/input$muSlider2)
    expo_z <- matrix(expo_grid$z, nrow = length(x), ncol = length(y))
    expo_z <- t(expo_z)
    plotlyObj <- plot_ly(x = x, y = y, z = expo_z, type = 'surface', showscale = FALSE, opacity = 0, colorscale = list(c(0, 1), c(boastPalette[5], boastPalette[5])),
                         hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(
        zaxis = list(title = "Joint Density", hoverformat = '.3f'),
        xaxis = list(title = "Total time (x)", hoverformat = '.3f'),
        yaxis = list(title = "Queue time (y)", hoverformat = '.3f'),
        camera = list(eye = list(x = 1.1, y = 2, z = 1.5))
      ),
      dragmode = TRUE,
      title = list(text = 'Joint PDF Plot',
                   font = list(size = 18),
                   x = 0.52,
                   y = 0.95))
    # create and add conditional plane
    x_val <- matrix(input$condSliderPos2, nrow = length(y), ncol = length(expo_z))
    expo_z[expo_z >= expo_gamma(x = input$condSliderPos2, y, mu = 1/input$muSlider2, lambda = 1/input$lambdaSlider2)] <- 0
    plotlyObj <- plotlyObj %>% add_surface(
      x = x_val,
      y = y,
      z = expo_z,
      type = 'surface',
      opacity = 1,
      showscale = FALSE,
      hovertext = "Conditional Plane")
    # add mesh effect
    plotlyObj <- plotlyObj %>% add_surface(x = ~x, y = ~y, opacity = 0, showscale = FALSE, 
                                           contours = list(
                                             x = list(show = TRUE, color = 'grey30', width = 1, start = -1, end = 15, size = 0.75),
                                             y = list(show = TRUE, color = 'grey30', width = 1, start = -1, end = 15, size = 0.75)
                                           ))
    config(plotlyObj, displaylogo = FALSE, displayModeBar = TRUE, scrollZoom = FALSE,
           modeBarButtonsToRemove = c('toImage', "pan3d", "orbitRotation", "zoom3d", "resetCameraDefault"))
  })
  
  # joint plot when conditioning y
  output$conditioning_y <- renderUI({
    x <- seq(-1,15, length.out = 75)
    y <- seq(-1,15, length.out = 75) 
    expo_grid <- expand.grid(x = x,y = y)
    expo_grid$z <- expo_gamma(expo_grid$x, expo_grid$y, lambda = 1/input$lambdaSlider2, mu = 1/input$muSlider2)
    expo_z <- matrix(expo_grid$z, nrow = length(x), ncol = length(y))
    expo_z <- t(expo_z)
    plotlyObj <- plot_ly(x = x, y = y, z = expo_z, type = 'surface', showscale = FALSE, opacity = 0, colorscale = list(c(0, 1), c(boastPalette[5], boastPalette[5])),
                         hoverinfo = 'x+y+z+text', hovertext = "Joint PDF") %>%
      layout(scene = list(
        zaxis = list(title = "Joint Density", hoverformat = '.3f'),
        xaxis = list(title = "Total time (x)", hoverformat = '.3f'),
        yaxis = list(title = "Queue time (y)", hoverformat = '.3f'),
        camera = list(eye = list(x = 1.1, y = 2, z = 1.5))
      ),
      dragmode = TRUE,
      title = list(text = 'Joint PDF Plot',
                   font = list(size = 18),
                   x = 0.52,
                   y = 0.95))
    # create and add conditional plane
    y_val <- matrix(input$condSliderPos2, nrow = length(x), ncol = length(expo_z))
    expo_z[expo_z >= expo_gamma(x = x, y = input$condSliderPos2, mu = 1/input$muSlider2, lambda = 1/input$lambdaSlider2)] <- 0
    plotlyObj <- plotlyObj %>% add_surface(
      x = x,
      y = y_val,
      z = expo_z,
      type = 'surface',
      opacity = 1,
      showscale = FALSE,
      hovertext = "Conditional Plane")
    # add mesh effect
    plotlyObj <- plotlyObj %>% add_surface(x = ~x, y = ~y, opacity = 0, showscale = FALSE, 
                                           contours = list(
                                             x = list(show = TRUE, color = 'grey30', width = 1, start = -1, end = 15, size = 0.75),
                                             y = list(show = TRUE, color = 'grey30', width = 1, start = -1, end = 15, size = 0.75)
                                           ))
    config(plotlyObj, displaylogo = FALSE, displayModeBar = TRUE, scrollZoom = FALSE,
           modeBarButtonsToRemove = c('toImage', "pan3d", "orbitRotation", "zoom3d", "resetCameraDefault"))
  })
  
  
  # conditional plane plots
  output$condExpoPlane <- renderUI({
    if (input$condition_x_or_y == 'x') {
      plotOutput('cond_pdf_of_y')
    } else if (input$condition_x_or_y == 'y') {
      plotOutput('cond_pdf_of_x')
    }
  })
  
  # conditional pdf of y
  output$cond_pdf_of_y <- renderPlot({
    x <- seq(-1,15, length.out = 125)
    y <- seq(-1,15, length.out = 125) 
    expo_z <- expo_gamma(input$condSliderPos2, y, lambda = 1/input$lambdaSlider2, mu = 1/input$muSlider2) / expo_x(x = input$condSliderPos2, lambda = 1/input$lambdaSlider2, mu = 1/input$muSlider2)
    ggplotObj <- ggplot(data = data.frame(y = y, expo_z = expo_z), 
                        mapping = aes(x = y, y = expo_z)) +
      labs(
        title = HTML('Conditional PDF of Y with X Equal to', input$condSliderPos2), 
        x = "Queue time (y)", y = "Conditional Density") +
      geom_line(color = boastPalette[5], linewidth = 1.25) +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)) +
      scale_x_continuous(expand = expansion(mult = c(0,0), add = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = c(0,0.03)))
  })
  
  # conditional pdf of x
  output$cond_pdf_of_x <- renderPlot({
    x <- seq(-1,15, length.out = 125)
    y <- seq(-1,15, length.out = 125) 
    expo_z <- expo_gamma(x, y = input$condSliderPos2, lambda = 1/input$lambdaSlider2, mu = 1/input$muSlider2) / expo_y(y = input$condSliderPos2, mu = 1/input$muSlider2)
    ggplotObj <- ggplot(data = data.frame(x = x, expo_z = expo_z), 
                        mapping = aes(x = x, y = expo_z)) +
      labs(
        title = HTML('Conditional PDF of X with Y Equal to', input$condSliderPos2), 
        x = "Total time (x)", y = "Conditional Density") +
      geom_line(color = boastPalette[5], linewidth = 1.25) +
      theme_bw()
    ggplotObj + theme(
      plot.title = element_text(size = 22),
      axis.title = element_text(size = 18),
      axis.text = element_text(size = 16)) +
      scale_x_continuous(expand = expansion(mult = c(0,0), add = 0)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.01), add = c(0,0.03)))
  })
  
  
  
  # change slider input label based on conditioned variable
  output$slider_update <- renderUI({
    if (input$condition_x_or_y == 'x') {
      sliderInput(
        inputId = 'condSliderPos2',
        label = p('Conditional plane (X =', tags$em('x'), ')'),
        min = 0.5,
        max = 14,
        value = 5,
        step = 0.5)
    } else if (input$condition_x_or_y == 'y'){
      sliderInput(
        inputId = 'condSliderPos2',
        label = p('Conditional plane (Y =', tags$em('y'), ')'),
        min = 0,
        max = 14,
        value = 5,
        step = 0.5)
    }
  })
  
  
  
  # captions
  output$joint_caption <- renderUI({
    withMathJax(
      p('3D Joint PDF plot where the color represents the value of the joint density according to the color scale. Note that the marginal
                densities are scaled and would peak at \\( \\frac{1}{\\sqrt{2\\pi}} \\approx 0.399 \\) in order to integrate to one')
    )
  })
  output$marginal_caption <- renderUI({
    p('Marginal PDF plot with the true marginal density (non-scaled)')
  })
  
  
  output$page2Caption1 <- renderUI({
    withMathJax(
      p('The top plot is a wire frame diagram of the joint density of X,Y, the blue shaded region shows 
        the joint density when X =', input$condSliderPos,'as represented by: \\[f_{X,Y}(',input$condSliderPos,', y)\\]'),
      
      p('The bottom plot represents the conditional PDF of Y given X when X =', input$condSliderPos, 'as represented by:',
        '\\[f_{Y|X}(y|', input$condSliderPos,') = \\frac{f_{X,Y}(',input$condSliderPos,',y)}{f_X(',input$condSliderPos,')}\\]')
    )
  })
  
  # need to make two captions for one output due to switching of variable
  output$page2Caption2 <- renderUI({
    if (input$condition_x_or_y == 'x') {
      uiOutput('caption2_conditioning_x')
    } else if (input$condition_x_or_y == 'y') {
      uiOutput('caption2_conditioning_y')
    }
  })
  
  output$caption2_conditioning_x <- renderUI({
    withMathJax(
      p('The top plot is a wire frame diagram of the joint density of X,Y, the blue shaded region shows 
        the joint density when X =', input$condSliderPos2,'as represented by: \\[f_{X,Y}(',input$condSliderPos2,', y)\\]'),
      
      p('The bottom plot represents the conditional PDF of Y given X when X =', input$condSliderPos2, 'as represented by:',
        '\\[f_{Y|X}(y|', input$condSliderPos2,') = \\frac{f_{X,Y}(',input$condSliderPos2,',y)}{f_X(',input$condSliderPos2,')}\\]')
    )
  })
  
  output$caption2_conditioning_y <- renderUI({
    withMathJax(
      p('The top plot is a wire frame diagram of the joint density of X,Y, the blue shaded region shows 
        the joint density when Y =', input$condSliderPos2,'as represented by: \\[f_{X,Y}(x, ',input$condSliderPos2,')\\]'),
      
      p('The bottom plot represents the conditional PDF of X given Y when Y =', input$condSliderPos2, 'as represented by:',
        '\\[f_{X|Y}(x|', input$condSliderPos2,') = \\frac{f_{X,Y}(y, ',input$condSliderPos2,')}{f_Y(',input$condSliderPos2,')}\\]')
    )
  })
  



}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server)

