
library(shiny)
library(shinythemes)
library(bslib)
library(rclipboard)

rand_letters_nums <- function(n){
  Choices = c(letters,LETTERS,0:9)
  Ind = sample(1:length(Choices),n+1,replace = TRUE)
  Output = Choices[Ind]
  Indicator = c("A","E","I","O","U")
  return(c(Indicator,Output))
}

Scrambler <- function(Message, Seed = 123, Noise = 100, Indicator = TRUE){
  Mess_split = strsplit(Message, split = "")[[1]]
  
  Mess_split = c(Mess_split, rand_letters_nums(Noise))
  
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Mess_split),length(Mess_split),replace = FALSE)
  
  Mess_Scrammed = paste(Mess_split[Scrambed_Ind], collapse="")
  return(Mess_Scrammed)
}

Unscrambler <- function(Scrambled, Seed = 123){
  Message_Scrammed = strsplit(Scrambled, split = "")[[1]]
  
  set.seed(Seed)
  Scrambed_Ind = sample(1:length(Message_Scrammed),length(Message_Scrammed),replace = FALSE)
  
  Mess_Unscrammed = rep(0,length(Message_Scrammed))
  counter = 0
  for(i in Scrambed_Ind){
    counter = counter + 1
    Mess_Unscrammed[i] <- Message_Scrammed[counter]
  }
  
  Tentative_Output = paste(Mess_Unscrammed, collapse = "")
  Output = strsplit(Tentative_Output, split = paste(c("A","E","I","O","U"), collapse = ""))[[1]]
  return(Output[1])
}

Pass_Index <- function(Input){
  Input <- strsplit(Input,split = "")[[1]]
  
  Pos <- as.character(c(""," ", letters,LETTERS,"?",".","!",0:9,"'",
                        "/","+","-","<",">","@","$","%","#","^","&","*","(",")","_","=",","))
  
  for(i in 1:length(Input)){
    Counter = 0
    for(j in 1:length(Pos)){
      if(Counter == 0){
        if(Input[i] == Pos[j]){
          Input[i] = j; Counter = 1
        }
      }
    }
  }
  
  return(Input)
}

# Function adapted to not scramble the original text.
Password_Protect <- function(Message, Password = "Crypto", Noise = 10){
  Mess_init = Message
  Pass_seq = Pass_Index(Password)
  
  for(i in Pass_seq){
    Message = Scrambler(Message, Seed = i, Noise = Noise)
  }
  
  if(Mess_init == "Enter a Message!"){Message = " "}
  return(Message)
}

# Function adapted to not unscramble the original text.
Password_Remove <- function(Scrambled, Password = "Crypto"){
  Scrambled_init = Scrambled
  Pass_seq = rev(Pass_Index(Password))
  
  for(i in Pass_seq){
    Scrambled = Unscrambler(Scrambled, Seed = i)
  }
  
  if(Scrambled_init == "Enter Scrambled Text!"){Scrambled = " "}
  return(Scrambled)
}


ui <- fluidPage(theme = shinytheme("cosmo"),
    rclipboardSetup(),
    navbarPage(
    "Message Scrambler and Unscrambler",

    tabPanel("Scramble",
      sidebarLayout(
          sidebarPanel(
              textInput("Mess",
                      "Enter Message",
                      value = "Enter a Message!"),
              textInput("Pass",
                      "Enter your Password",
                      value = "Crypto"),
              submitButton("Submit")
          ),
          mainPanel(
            h1("Scrambled Output"),
            verbatimTextOutput("Encoded"),
            uiOutput("clip1"),
            h4("Made by statswithr.com")
          )
      ) 
     
    ),
    
    tabPanel("Unscramble",
             sidebarLayout(
               sidebarPanel(
                 textInput("Scram",
                           "Enter Scrambled Message",
                           value = "Enter Scrambled Text!"),
                 textInput("Pass2",
                           "Enter your Password",
                           value = "Crypto"),
                 submitButton("Submit")
               ),
               mainPanel(
                 h1("Unscrambled Output"),
                 verbatimTextOutput("Decoded"),
                 uiOutput("clip2"),
                 h4("Made by statswithr.com")
               )
             ) 
            )
  )
)

server <- function(input, output) {

  output$Encoded <- renderText({
   Password_Protect(Message = input$Mess, Password = input$Pass)
  })
  
  output$Decoded <- renderText({
   Password_Remove(Scrambled = input$Scram, Password = input$Pass2)
  })
  
  output$clip1 <- renderUI({
    rclipButton("clipbtn",label = "Copy Output", 
      clipText = Password_Protect(Message = input$Mess, Password = input$Pass))
  })
  
  output$clip2 <- renderUI({
    rclipButton("clipbtn",label = "Copy Output", 
      clipText = Password_Remove(Scrambled = input$Scram, Password = input$Pass2))
    })

}


shinyApp(ui = ui, server = server)
