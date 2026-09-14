
library(shiny)

ui <- fluidPage(
  
  titlePanel("Multiplication Practice"),
  
  tags$head(
    tags$style(HTML("
      .question {
        font-size: 60px;
        font-weight: bold;
        text-align: center;
        margin-top: 80px;
        margin-bottom: 50px;
      }
      
      .button-container {
        text-align: center;
      }
      
      .big-button {
        font-size: 24px;
        padding: 15px 30px;
        margin: 10px;
      }
      
      .checkbox-container {
        font-size: 18px;
      }
      
      .mode-container {
        margin-top: 25px;
        font-size: 18px;
      }
    "))
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      div(
        class = "checkbox-container",
        
        checkboxGroupInput(
          inputId = "numbers",
          label = "Choose numbers for the first number:",
          choices = 1:12,
          selected = 1:12
        )
      ),
      
      div(
        class = "mode-container",
        
        radioButtons(
          inputId = "mode",
          label = "Question mode:",
          choices = c(
            "Random" = "random",
            "Sequential" = "sequential"
          ),
          selected = "random"
        )
      )
    ),
    
    mainPanel(
      
      div(
        class = "question",
        textOutput("question")
      ),
      
      div(
        class = "button-container",
        
        actionButton(
          inputId = "answer",
          label = "Answer",
          class = "big-button btn-success"
        ),
        
        actionButton(
          inputId = "new_question",
          label = "New Question",
          class = "big-button btn-primary"
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  # Store the current question
  question <- reactiveVal(NULL)
  
  # Keep track of whether the answer is visible
  show_answer <- reactiveVal(FALSE)
  
  # Sequential mode counters
  sequential_first_index <- reactiveVal(1)
  sequential_second_number <- reactiveVal(1)
  
  
  # Generate a question
  generate_question <- function() {
    
    # Get the CURRENTLY checked numbers
    selected_numbers <- as.numeric(input$numbers)
    
    # If nothing is checked, clear the question
    if (length(selected_numbers) == 0) {
      question(NULL)
      show_answer(FALSE)
      return()
    }
    
    # Sort the selected numbers so sequential mode
    # always goes from lowest to highest.
    selected_numbers <- sort(selected_numbers)
    
    
    # --------------------------------------------------
    # RANDOM MODE
    # --------------------------------------------------
    
    if (input$mode == "random") {
      
      # If only one number is checked, always use that number.
      # Otherwise, randomly select from the checked numbers.
      if (length(selected_numbers) == 1) {
        num1 <- selected_numbers[1]
      } else {
        num1 <- sample(selected_numbers, size = 1)
      }
      
      # Second number is randomly selected from 1 through 12
      num2 <- sample(1:12, size = 1)
    }
    
    
    # --------------------------------------------------
    # SEQUENTIAL MODE
    # --------------------------------------------------
    
    else {
      
      # Make sure the current index is still valid.
      # This is useful if the user changes the selected numbers.
      if (sequential_first_index() > length(selected_numbers)) {
        sequential_first_index(1)
      }
      
      # First number comes from the selected numbers,
      # in ascending order.
      num1 <- selected_numbers[sequential_first_index()]
      
      # Second number goes from 1 through 12.
      num2 <- sequential_second_number()
    }
    
    
    # Store the question
    question(
      list(
        num1 = num1,
        num2 = num2
      )
    )
    
    # Hide the answer
    show_answer(FALSE)
  }
  
  
  # Generate the initial question and regenerate whenever
  # the checkbox selection changes
  observeEvent(input$numbers, {
    
    # Reset the sequential sequence whenever
    # the selected numbers change.
    sequential_first_index(1)
    sequential_second_number(1)
    
    generate_question()
    
  }, ignoreInit = FALSE)
  
  
  # Reset the sequential sequence when the mode changes
  observeEvent(input$mode, {
    
    sequential_first_index(1)
    sequential_second_number(1)
    
    generate_question()
    
  })
  
  
  # Generate a new question when the button is clicked
  observeEvent(input$new_question, {
    
    # In sequential mode, advance the sequence
    # BEFORE generating the next question.
    if (input$mode == "sequential") {
      
      selected_numbers <- sort(as.numeric(input$numbers))
      
      if (length(selected_numbers) > 0) {
        
        # If we just displayed x12, move to the next
        # selected first number and start at x1.
        if (sequential_second_number() == 12) {
          
          sequential_second_number(1)
          
          if (sequential_first_index() >= length(selected_numbers)) {
            # After the last selected number, wrap
            # back to the first selected number.
            sequential_first_index(1)
          } else {
            sequential_first_index(
              sequential_first_index() + 1
            )
          }
          
        } else {
          
          # Otherwise simply move from x1 to x2 ... x12.
          sequential_second_number(
            sequential_second_number() + 1
          )
        }
      }
    }
    
    generate_question()
  })
  
  
  # Show the answer
  observeEvent(input$answer, {
    show_answer(TRUE)
  })
  
  
  # Display the question
  output$question <- renderText({
    
    q <- question()
    
    if (is.null(q)) {
      return("Please select at least one number.")
    }
    
    if (show_answer()) {
      
      paste(
        q$num1,
        "x",
        q$num2,
        "=",
        q$num1 * q$num2
      )
      
    } else {
      
      paste(
        q$num1,
        "x",
        q$num2,
        "= ?"
      )
    }
  })
}


shinyApp(ui = ui, server = server)
