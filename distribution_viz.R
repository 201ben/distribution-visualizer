library(shiny)
library(shinyjs)
library(plotly)
library(stats)
library(VGAM)

plot_color <- list(
  cdf = '#F1BF98',
  pdf = '#92B9BD'
)

# formuas
latex_formulae <- list(
  Normal = list(
    pdf = '$$f(x)= \\frac{1}{\\sigma \\sqrt{2\\pi}}e^{-\\frac{1}{2}\\big(\\frac{x-\\mu}{\\sigma}\\big)^2}$$',
    cdf = '$$F(x)=\\Phi(x)$$'
  ),
  Lognormal = list(
    pdf = '$$f(x)=\\frac{\\phi\\Big(\\frac{\\ln(x)-\\mu}{\\sigma}\\Big)}{\\sigma x}$$',
    cdf = '$$F(x)=\\phi\\Big(\\frac{\\ln(x)-\\mu}{\\sigma}\\Big)$$'
  ),
  Exponential = list(
    pdf = '$$f(x)=\\frac{1}{\\theta}e^{-\\frac{x}{\\theta}}$$',
    cdf = '$$F(x)=1-e^{-\\frac{x}{\\theta}}$$'
  ),
  Gamma = list(
    pdf = '$$f(x)=\\frac{\\big(\\frac{x}{\\theta}\\big)^\\alpha e^{-\\frac{x}{\\theta}}}{x\\Gamma(\\alpha)}$$',
    cdf = '$$F(x)=\\Gamma\\Big(\\alpha;\\frac{x}{\\theta}\\Big)$$'
  ),
  Pareto = list(
    pdf = '$$f(x)=\\frac{\\alpha\\theta^\\alpha}{x^{\\alpha+1}}$$',
    cdf = '$$F(x) = 1-\\Big(\\frac{\\theta}{x}\\Big)^\\alpha$$'
  ),
  Weibull = list(
    pdf = '$$f(x)=\\frac{\\tau\\big(\\frac{x}{\\theta}\\big)^\\tau e^{-(\\frac{x}{\\theta})^\\tau}}{x}$$',
    cdf = '$$F(x)=1-e^{-(\\frac{x}{\\theta})^\\tau}$$'
  ),
  Beta = list(
    pdf = '$$f(x)=\\frac{\\Gamma(a+b)}{\\Gamma(a)\\Gamma(b)}\\Big(\\frac{x}{\\theta}\\Big)^a\\Big(1-\\frac{x}{\\theta}\\Big)^{b-1}\\frac{1}{x}$$',
    cdf = '$$F(x)=\\beta\\Big(a;b;\\frac{x}{\\theta}\\Big)$$'
  ))

ui <- fluidPage(
  useShinyjs(),
  # css (should prob just make a .css file at this point)
  tags$head(
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.2/MathJax.js?config=TeX-MML-AM_CHTML", type = "text/javascript"),
    tags$link(rel = "stylesheet", href = 'https://fonts.googleapis.com/css2?family=Tajawal:wght@500&display=swap'),
    tags$style(HTML('
      body {
        background-color: #363636;
        color: #aaa;
        font-family: "Tajawal", sans-serif;
      }
    #sidebar {
      background-color: #363636;
      padding: 45px 45px 45px 45px; 
      width: 45%;
      border: 6px solid rgba(0, 0, 0, 0.3);
      box-shadow: 20px 20px 0 rgba(0, 0, 0, 0.3);
      border-radius: 25px;
  }
    #equations {
    width: 15%;
      font-size: 40px;
      color: #aaa;
      text-align: right;
      text-shadow: -1px -1px 1px #242424, 1px -1px 1px #242424, -1px 1px 1px #242424, 1px 1px 1px #242424;
    }
  .mainpanel {background-color: #363636;}
    '))),
br(),
fluidRow(
  column(6, 
         class = "mainpanel",
         plotlyOutput("pdfPlot")
  ),
  br(), 
  br(),
  column(6, id = "sidebar",
         tags$style("#distribution {background-color:#363636;
                                      border: 3px solid #242424;
                                      color:#aaa}"),
         selectInput("distribution", "Select Distribution:",
                     choices = c("Normal", 
                                 "Lognormal", 
                                 "Exponential", 
                                 "Gamma", 
                                 "Pareto", 
                                 "Weibull", 
                                 "Beta"), 
                     selectize = FALSE),
         tags$style("#mean, #sd, #meanlog, #sdlog, #rate, #shape, #scale, #shape1, #shape2{
                        background-color:#363636;
                        border: 3px solid #242424;
                        color:#aaa}"),
         uiOutput("parameterInputs"))),
br(),
fluidRow(
  column(6,class = "mainpanel",
         plotlyOutput("cdfPlot")),
  column(6, id = "equations", 
         uiOutput("pdf_eq"), 
         uiOutput("cdf_eq"))))


server <- function(input, output) {
output$cdf_eq <- renderUI({
  fluidRow(
    column(12, withMathJax(paste0(latex_formulae[[input$distribution]]$cdf)))
  )
})

output$pdf_eq <- renderUI({
  fluidRow(
    column(12, withMathJax(paste0(latex_formulae[[input$distribution]]$pdf)))
  )
})
  
output$parameterInputs <- renderUI({
  distribution <- input$distribution
  if (distribution == "Normal") {
    fluidRow(
      column(6, numericInput("mean", "Mean (\u03BC):", value = 0)),
      column(6, numericInput("sd", "Standard Deviation (\u03C3):", value = 1))
    )
  } else if (distribution == "Lognormal") {
    fluidRow(
      column(6, numericInput("meanlog", "Log-Mean (\u03BC):", value = 0)),
      column(6, numericInput("sdlog", "Log-Standard Deviation (\u03C3):", value = 1))
    )
  } else if (distribution == "Exponential") {
    fluidRow(
      column(6, numericInput("rate", "Rate Parameter (1/\u03B8):", value = 1))
    )
  } else if (distribution == "Gamma") {
    fluidRow(
      column(6, numericInput("shape", "Shape Parameter (\u03B1): ", value = 1)),
      column(6, numericInput("scale", "Scale Parameter (\u03B8):", value = 1))
    )
  } else if (distribution == "Pareto") {
    fluidRow(
      column(6, numericInput("shape", "Shape Parameter (\u03B1): ", value = 1)),
      column(6, numericInput("scale", "Scale Parameter (\u03B8):", value = 1))
    )
  } else if (distribution == "Weibull") {
    fluidRow(
      column(6, numericInput("shape", "Shape Parameter (\u03C4)", value = 1)),
      column(6, numericInput("scale", "Scale Parameter (\u03B8):", value = 1))
    )
  } else if (distribution == "Beta") {
    fluidRow(
      column(6, numericInput("shape1", "Shape Parameter 1 (a):", value = 1)),
      column(6, numericInput("shape2", "Shape Parameter 2 (b):", value = 1))
    )
  }
})
  
output$cdfPlot <- renderPlotly({
  distribution <- input$distribution
  params <- getParameters(input)
  
  data <- data.frame(x = seq(getXMin(distribution), getXMax(distribution), length.out = 100))
  data$y <- switch(distribution,
                   "Normal" = pnorm(data$x, mean = params$mean, sd = params$sd),
                   "Lognormal" = plnorm(data$x, meanlog = params$meanlog, sdlog = params$sdlog),
                   "Exponential" = pexp(data$x, rate = params$rate),
                   "Gamma" = pgamma(data$x, shape = params$shape, scale = params$scale),
                   "Pareto" = ppareto(data$x, shape = params$shape, scale = params$scale),
                   "Weibull" = pweibull(data$x, shape = params$shape, scale = params$scale),
                   "Beta" = pbeta(data$x, shape1 = params$shape1, shape2 = params$shape2))
    
plot_ly(data, 
        x = ~x, 
        y = ~y, 
        type = "scatter", 
        mode = "lines",
        fill = 'tozeroy', 
        line = list(color = plot_color[['cdf']]), 
        fillcolor = paste0('rgba(', paste0(col2rgb(plot_color[['cdf']]), collapse = ','), ',0.4)')) %>%
  layout(xaxis = list(color = "#aaa",title = "x", gridcolor = "#363636", gridwidth = 1),
         yaxis = list(color = "#aaa", title = "F(x)", range = c(0, 1), gridcolor = "#242424", gridwidth = .75), 
         plot_bgcolor = 'rgba(0, 0, 0, 0)',
         paper_bgcolor = 'rgba(0,0,0,0)',
         annotations = list(
           list(text = "<b>Cumulative Distribution Function (CDF)</b>",
                xref = "paper", yref = "paper",
                x = .5, y = 1.07,
                showarrow = FALSE,
                font = list(size = 19, color = "#242424"),
                xshift = 3, yshift = 0),
           list(text = "<b>Cumulative Distribution Function (CDF)</b>",
                xref = "paper", yref = "paper",
                x = .5, y = 1.07,
                showarrow = FALSE,
                font = list(size = 19, color = plot_color[['cdf']]))))
})
  
  
output$pdfPlot <- renderPlotly({
  distribution <- input$distribution
  params <- getParameters(input)
  
  data <- data.frame(x = seq(getXMin(distribution), getXMax(distribution), length.out = 100))
  data$y <- switch(distribution,
                   "Normal" = dnorm(data$x, mean = params$mean, sd = params$sd),
                   "Lognormal" = dlnorm(data$x, meanlog = params$meanlog, sdlog = params$sdlog),
                   "Exponential" = dexp(data$x, rate = params$rate),
                   "Gamma" = dgamma(data$x, shape = params$shape, scale = params$scale),
                   "Pareto" = dpareto(data$x, shape = params$shape, scale = params$scale),
                   "Weibull" = dweibull(data$x, shape = params$shape, scale = params$scale),
                   "Beta" = dbeta(data$x, shape1 = params$shape1, shape2 = params$shape2))
  
plot_ly(data, 
        x = ~x, 
        y = ~y, 
        type = "scatter", 
        mode = "lines",
        fill = 'tozeroy', 
        line = list(color = plot_color[['pdf']]), 
        fillcolor = paste0('rgba(', paste0(col2rgb(plot_color[['pdf']]), collapse = ','), ',0.4)')) %>%
  layout(xaxis = list(color = "#aaa",title = "x", gridcolor = "#363636", gridwidth = 1),
         yaxis = list(color = "#aaa", title = "f(x)", range = c(0, 1), gridcolor = "#242424", gridwidth = .75),
         plot_bgcolor = 'rgba(0, 0, 0, 0)',
         paper_bgcolor = 'rgba(0,0,0,0)',
         annotations = list(
           list(text = "<b>Probability Density Function (PDF)</b>",
                xref = "paper", yref = "paper",
                x = .5, y = 1.07,
                showarrow = FALSE,
                font = list(size = 19, color = "#242424"),
                xshift = 3, yshift = 0),
           list(text = "<b>Probability Density Function (PDF)</b>",
                xref = "paper", yref = "paper",
                x = .5, y = 1.07,
                showarrow = FALSE,
                font = list(size = 19, color = plot_color[['pdf']]))))
})
  
getParameters <- function(input) {
  distribution <- input$distribution
  params <- switch(distribution,
                   "Normal" = list(mean = input$mean, sd = input$sd),
                   "Lognormal" = list(meanlog = input$meanlog, sdlog = input$sdlog),
                   "Exponential" = list(rate = input$rate),
                   "Gamma" = list(shape = input$shape, scale = input$scale),
                   "Pareto" = list(shape = input$shape, scale = input$scale),
                   "Weibull" = list(shape = input$shape, scale = input$scale),
                   "Beta" = list(shape1 = input$shape1, shape2 = input$shape2))
  return(params)
}
  
  getXMin <- function(distribution) {
    if (distribution == "Normal") {
      return(-10)
    } else if (distribution == "Exponential") {
      return(0)
    } else if (distribution == "Gamma") {
      return(0)
    } else if (distribution == "Pareto") {
      return(0)
    } else if (distribution == "Weibull") {
      return(0)
    } else if (distribution == "Beta") {
      return(0)
    }
    return(0)
  }
  
  getXMax <- function(distribution) {
    if (distribution == "Normal") {
      return(10)
    } else if (distribution == "Exponential") {
      return(10)
    } else if (distribution == "Gamma") {
      return(10)
    } else if (distribution == "Pareto") {
      return(10)
    } else if (distribution == "Weibull") {
      return(10)
    } else if (distribution == "Beta") {
      return(1)
    }
    return(10)
  }

}

shinyApp(ui, server)