

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
    
    # If only one number is checked, ALWAYS use that number.
    # Otherwise, randomly select from the checked numbers.
    if (length(selected_numbers) == 1) {
      num1 <- selected_numbers[1]
    } else {
      num1 <- sample(selected_numbers, size = 1)
    }
    
    # Second number is always randomly selected from 1 through 12
    num2 <- sample(1:12, size = 1)
    
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
    generate_question()
  }, ignoreInit = FALSE)
  
  
  # Generate a new question when the button is clicked
  observeEvent(input$new_question, {
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


