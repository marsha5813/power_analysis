library(shiny)
library(pwr)
library(ggplot2)

# Define UI for the application
ui <- fluidPage(
  titlePanel("Power Analysis for Chi-squared Test"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("w",
                  "Effect Size (Cohen's w):",
                  min = 0.1,
                  max = 0.5,
                  value = 0.3,
                  step = 0.01),
                  helpText("Cohen's w is a measure of effect size used for chi-squared tests. ",
                           "Set this to 0.1 if you want to be able to detect a small  ",
                           "difference, or 0.3 (medium) to 0.5 (large)."),
      sliderInput("groups",
                  "Number of Groups:",
                  min = 2,
                  max = 10,
                  value = 2),
      helpText("How many groups are you comparing? For instance, set this to 2 if you're comparing  ",
               "two groups, such as Asian Americans who are randomly assigned to two survey ballots. ",
               "If you want to compare Asian Americans AND a comparison sample, each of whom are assigned  ",
               "to two different survey ballots, you'd set this to 4"),
      sliderInput("responses",
                  "Number of Response Options:",
                  min = 2,
                  max = 10,
                  value = 4),
      helpText("How many response options in a categorical survey question would you want to compare? If the  ",
               "response options are 'Christian', 'Catholic' and 'Other', set this to 3."),
      sliderInput("sigLevel",
                  "Significance Level (alpha):",
                  min = 0.01,
                  max = 0.1,
                  value = 0.05,
                  step = 0.01),
      sliderInput("power",
                  "Desired Power:",
                  min = 0.7,
                  max = 0.99,
                  value = 0.8,
                  step = 0.01),
      helpText("Probability of finding an effect if it exists. 0.8 is pretty typical for power analyses.  ",
               "Higher values indicate a greater likelihood of detecting true effects, enhancing the test's reliability."),
      actionButton("calc", "Calculate Optimal n")
    ),
    
    mainPanel(
      plotOutput("powerPlot")
    )
  )
)

# Define server logic for the power analysis and plotting
server <- function(input, output) {
  observeEvent(input$calc, {
    # Calculate degrees of freedom based on user input
    df <- (input$groups - 1) * (input$responses - 1)
    
    # Calculate the required sample size
    tryCatch({
      power_result <- pwr.chisq.test(w = input$w, 
                                     df = df,
                                     power = input$power, 
                                     sig.level = input$sigLevel)
      

      
      # Generate plotting data only if power_result is valid
      if (!is.na(power_result$N)) {
        n_values <- seq(from = 20, to = ceiling(power_result$N) * 1.5, by = 10)
        powers <- sapply(n_values, function(n) {
          pwr.chisq.test(w = input$w, df = df, N = n, sig.level = input$sigLevel)$power
        })
        
        plot_data <- data.frame(n = n_values, Power = powers)
        
        # Generate the plot
        output$powerPlot <- renderPlot({
          gg <- ggplot(plot_data, aes(x = n, y = Power)) +
            geom_line() +
            geom_vline(xintercept = power_result$N, color = "red", linetype = "dashed") +
            geom_text(aes(x = power_result$N, y = 0.5, label = paste("Optimal n for each group:", round(power_result$N))), vjust = -1) +
            labs(title = "Power Analysis for Chi-squared Test",
                 x = "Sample Size (n)",
                 y = "Power",
                 subtitle = paste("Effect size (w):", input$w,
                                  "Degrees of freedom:", df,
                                  "Significance level:", input$sigLevel,
                                  "Desired power:", input$power)) +
            theme_minimal()
          print(gg)
        })
      }
      }, error = function(e) {
      output$powerPlot <- renderPlot({
        ggplot() + 
          labs(title = "Error in computation",
               subtitle = "Please adjust input parameters")
      })
    })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
