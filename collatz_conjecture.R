library(shiny)
library(igraph)

# Define UI ----
ui <- fluidPage(
  titlePanel("Collatz Conjecture"),
  h5("Prof. Dr. Dennis Klinkhammer (2023)"),
  sidebarLayout(
    sidebarPanel(
      strong("Theoretical Background:"),
      h5("The Collatz conjecture is one of the most famous unsolved problems in mathematics. The conjecture asks whether repeating two simple arithmetic operations will eventually transform every positive integer into 1. It concerns sequences of integers in which each term is obtained from the previous term as follows: if the previous term is even, the next term is one half of the previous term. If the previous term is odd, the next term is 3 times the previous term plus 1. The conjecture is that these sequences always reach 1, no matter which positive integer is chosen to start the sequence.", align="justify"),
      numericInput("number", "Enter a starting number:", value = 1, min = 1),
      actionButton("go", "Let's go!")
    ),
    mainPanel(
      h3("(1) Collatz Sequence"),
      plotOutput("sequence_plot"),
      h3("(2) Numbers of the Collatz Sequence"),
      verbatimTextOutput("output"),
      h3("(3) Visualization of the Collatz Sequence"),
      plotOutput("network_plot"),
    )
  )
)

# Define server ----
server <- function(input, output) {
  
  # Create a reactive function that computes the Collatz sequence
  collatz <- reactive({
    x <- input$number
    seq <- c(x)
    while (x != 1) {
      if (x %% 2 == 0) {
        x <- x/2
      } else {
        x <- 3*x + 1
      }
      seq <- c(seq, x)
    }
    return(seq)
  })
  
  # Display the Collatz sequence as a line chart
  output$sequence_plot <- renderPlot({
    seq <- collatz()
    plot(seq, type = "l", xlab = "Steps of the Sequence", ylab = "Numbers",
         ylim = c(0, max(seq)))
  })
  
  # Display the individual steps of the Collatz sequence
  output$output <- renderPrint({
    seq <- collatz()
    paste0("Starting number: ", input$number, "\n\n")
    for (i in 1:length(seq)) {
      step <- paste0(seq[i])
      if (i != length(seq)) {
        step <- paste0(step, " -> ")
      }
      cat(step)
    }
  })
  
  # Create a reactive function that generates a network plot of the Collatz sequence
  output$network_plot <- renderPlot({
    seq <- collatz()
    edges <- data.frame(from = seq[-length(seq)], to = seq[-1])
    g <- graph_from_data_frame(edges)
    layout <- layout_with_fr(g)
    plot(g, vertex.label = NA, layout = layout)
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)
