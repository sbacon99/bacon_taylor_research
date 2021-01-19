# An incentive survey
# Sam Bacon

#install.packages("shinydashboard")
#install.packages("shinythemes")

library(shiny)
library(shinythemes)
library(googlesheets4)
gs4_auth(cache = ".secrets", email = TRUE, use_oob = TRUE)


# Define UI for application that draws a histogram
ui <- fluidPage(

    theme = shinytheme("flatly"),
    
    # Application title
    titlePanel("Incentive Survey"),

    textInput("codeInput", "Please enter your incentive code.", 
              value = "", width = 700, placeholder = NULL),
    textInput("emailInput", "Please enter your email address", 
              value = "", width = 700, placeholder = NULL),

    actionButton("submit","Submit"),
    
    # Panel
    conditionalPanel(condition = "input.submit == 1",
            br(),
            textOutput("incentiveCode")        
    ) 

    
    

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$incentiveCode <- renderText({
        

        keyLocation <- gs4_get('https://docs.google.com/spreadsheets/d/1i_kF-5rrdqSD9eXss-eyOmmhGi3_Y5wDqFCEk2kPkus/edit#gid=0')
        sent_keys <- read_sheet(keyLocation, sheet = "sent_keys")   
        used_keys <- read_sheet(keyLocation, sheet = "used_keys") 
        criteria1 <- input$codeInput %in% sent_keys
        criteria2 <- length(intersect(input$codeInput,used_keys))==0
 
        if(criteria1 && criteria2){
            ss <- gs4_get('https://docs.google.com/spreadsheets/d/1i_kF-5rrdqSD9eXss-eyOmmhGi3_Y5wDqFCEk2kPkus/edit#gid=0')
            ss %>% sheet_append(as.data.frame(input$codeInput), sheet = "used_keys")
            ss %>% sheet_append(as.data.frame(input$emailInput), sheet = "emails")
            
            output <- "Thank you for submitting! You will recieve your incentive via email."
            
        }
        else{
            output <- "Sorry, the code you have entered is not valid."
        }
        
        paste(output)
    })
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
