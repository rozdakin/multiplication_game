

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
    "))
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      numericInput(
        inputId = "lower",
        label = "Lower number:",
        value = 1,
        min = 0
      ),
      
      numericInput(
        inputId = "upper",
        label = "Higher number:",
        value = 12,
        min = 1
      )
    ),
    
    mainPanel(
      
      # Question and answer appear together on the same line
      div(
        class = "question",
        textOutput("question")
      ),
      
      # Buttons
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
  
  # Generate the first question automatically
  question <- reactiveVal(
    list(
      num1 = sample(1:12, 1),
      num2 = sample(1:12, 1)
    )
  )
  
  # Keep track of whether the answer is visible
  show_answer <- reactiveVal(FALSE)
  
  # Generate a new question
  observeEvent(input$new_question, {
    
    # Make sure the lower number isn't greater than the upper number
    if (input$lower > input$upper) {
      return()
    }
    
    num1 <- sample(input$lower:input$upper, 1)
    num2 <- sample(1:12, 1)
    
    question(
      list(
        num1 = num1,
        num2 = num2
      )
    )
    
    # Hide the answer for the new question
    show_answer(FALSE)
  })
  
  # Show the answer
  observeEvent(input$answer, {
    show_answer(TRUE)
  })
  
  # Display question and, if requested, answer
  output$question <- renderText({
    
    q <- question()
    
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


