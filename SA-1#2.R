# Define the UI
ui <- fluidPage(
  titlePanel("Discrete Random Variable Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("variable_type", "Select Random Variable Type:",
                   c("Univariate" = "univariate",
                     "Bivariate" = "bivariate")),
      
      numericInput("num_values", "Number of Values:", 2, min = 2, step = 1),
      
      uiOutput("value_inputs"),
      uiOutput("value_inputs_y"),
      uiOutput("probability_inputs"),
      
      actionButton("calculate", "Calculate")
    ),
    
    mainPanel(
      h4("Results"),
      verbatimTextOutput("results"),
      plotOutput("pdf_plot"),
      plotOutput("cdf_plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically generate value and probability inputs
  output$value_inputs <- renderUI({
    num_values <- input$num_values
    lapply(1:num_values, function(i) {
      textInput(paste0("value_x_", i), paste0("Value X", i), "")
    })
  })
  
  output$value_inputs_y <- renderUI({
    if (input$variable_type == "bivariate") {
      num_values <- input$num_values
      lapply(1:num_values, function(i) {
        textInput(paste0("value_y_", i), paste0("Value Y", i), "")
      })
    }
  })
  
  output$probability_inputs <- renderUI({
    num_values <- input$num_values
    total_probabilities <- if (input$variable_type == "univariate") num_values else num_values^2
    
    lapply(1:total_probabilities, function(i) {
      numericInput(paste0("probability_", i), paste0("Probability ", i), 0, 0, 1, step = 0.01)
    })
  })
  
  
  # Perform calculations and validity checks
  calculations <- reactive({
    req(input$calculate)
    
    # Check if probabilities sum to one
    probabilities <- sapply(1:input$num_values, function(i) input[[paste0("probability_", i)]])
    if (sum(probabilities) != 1) {
      showModal(modalDialog(
        title = "Error",
        "Probabilities must sum to 1.",
        easyClose = TRUE
      ))
      return(NULL)
    }
    
    list(probabilities = probabilities)
  })
  
  # Calculate the mean and variance, and plot the pdf and cdf
  output$results <- renderPrint({
    req(calculations())
    
    probabilities <- calculations()$probabilities
    
    if (input$variable_type == "univariate") {
      values <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_x_", i)]]))
      mean <- sum(values * probabilities)
      variance <- sum((values - mean)^2 * probabilities)
      
      cat("Mean:", mean, "\nVariance:", variance)
    } else if (input$variable_type == "bivariate") {
      values_x <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_x_", i)]]))
      values_y <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_y_", i)]]))
      joint_probabilities <- matrix(0, nrow = length(values_x), ncol = length(values_y))
      counter <- 1
      for (i in 1:length(values_x)) {
        for (j in 1:length(values_y)) {
          joint_probabilities[i, j] <- probabilities[counter]
          counter <- counter + 1
        }
      }
      
      
      
      # Marginal distributions
      marginal_prob_x <- colSums(joint_probabilities)
      marginal_prob_y <- rowSums(joint_probabilities)
      
      # Conditional distributions
      conditional_prob_x_given_y <- joint_probabilities / marginal_prob_y
      conditional_prob_y_given_x <- joint_probabilities / marginal_prob_x
      
      cat("Marginal Probability X:", marginal_prob_x, "\nMarginal Probability Y:", marginal_prob_y,
          "\nConditional Probability X|Y:", conditional_prob_x_given_y,
          "\nConditional Probability Y|X:", conditional_prob_y_given_x)
    }
  })
  
  # PDF plot
  output$pdf_plot <- renderPlot({
    req(calculations())
    probabilities <- calculations()$probabilities
    
    if (input$variable_type == "univariate") {
      values <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_x_", i)]]))
      ggplot(data.frame(x = values, y = probabilities), aes(x, y)) +
        geom_col(fill = "steelblue") +
        labs(x = "Values", y = "Probabilities", title = "Probability Density Function (PDF)")
    } else if (input$variable_type == "bivariate") {
      values_x <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_x_", i)]]))
      values_y <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_y_", i)]]))
      joint_probabilities <- matrix(0, nrow = length(values_x), ncol = length(values_y))
      counter <- 1
      for (i in 1:length(values_x)) {
        for (j in 1:length(values_y)) {
          joint_probabilities[i, j] <- probabilities[counter]
          counter <- counter + 1
        }
      }
      
      
      ggplot(data.frame(x = as.vector(values_x), y = as.vector(values_y), z = as.vector(joint_probabilities)), aes(x, y, fill = z)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(x = "Values X", y = "Values Y", fill = "Probability", title = "Joint Probability Density Function (PDF)")
    }
  })
  
  # CDF plot
  output$cdf_plot <- renderPlot({
    req(calculations())
    probabilities <- calculations()$probabilities
    
    if (input$variable_type == "univariate") {
      values <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_x_", i)]]))
      ggplot(data.frame(x = values, y = cumsum(probabilities)), aes(x, y)) +
        geom_step(direction = "hv", color = "steelblue") +
        labs(x = "Values", y = "Cumulative Probabilities", title = "Cumulative Distribution Function (CDF)")
    } else if (input$variable_type == "bivariate") {
      values_x <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_x_", i)]]))
      values_y <- sapply(1:input$num_values, function(i) as.numeric(input[[paste0("value_y_", i)]]))
      joint_probabilities <- matrix(0, nrow = length(values_x), ncol = length(values_y))
      counter <- 1
      for (i in 1:length(values_x)) {
        for (j in 1:length(values_y)) {
          joint_probabilities[i, j] <- probabilities[counter]
          counter <- counter + 1
        }
      }
      
      cumulative_prob <- cumsum(cumsum(joint_probabilities))
      
      ggplot(data.frame(x = as.vector(values_x), y = as.vector(values_y), z = as.vector(cumulative_prob)), aes(x, y, fill = z)) +
        geom_tile() +
        scale_fill_gradient(low = "white", high = "steelblue") +
        labs(x = "Values X", y = "Values Y", fill = "Cumulative Probability", title = "Joint Cumulative Distribution Function (CDF)")
    }
  })
}

shinyApp(ui = ui, server = server)
                        