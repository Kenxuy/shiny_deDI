# test commit/push
library(shiny)
library(readr)
library(dplyr)
library(magrittr)
library(digest)
library(DT)
library(stringr)
library(janitor)
library(tidyr)
library(magrittr)
library(purrr)
library(rio)


# Define the UI
ui <- fluidPage(
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("De-identify", value = "panel1", 
                 # File input
                 fileInput("file", "Choose raw data file"),
                 
                 # Separator selection
                 selectInput("separator", "Select separator", choices = c(",", ";", "\t"), selected = ","),
                 # Header selection
                 checkboxInput("header", "File contains headers", value = TRUE),
                 
                 # Variable selection
                 selectInput("variables", "Select identifier variables", choices = NULL, multiple = TRUE),
                 selectInput("variablesrem", "Select removable variables", choices = NULL, multiple = TRUE),
                 textInput("salt","Insert Salt",width = "1000px"),
                 # Text inputs for variable prefixes
                 uiOutput("variable_prefixes"),
                 
                 # Generate output file button
                 downloadButton("key", "Generate Key"),
                 downloadButton("tokenft", "Tokenise Free Text"),
                 radioButtons("uploadMask", "Choose Masknames Source:",
                              choices = c("Upload File", "No Masknames"),
                              selected = "No Masknames"),
                 conditionalPanel(
                   condition = "input.uploadMask == 'Upload File'",
                   fileInput("masknames", "Masked FT")
                 ),
                 downloadButton("ddi", "Generate De-identified Data")),
        tabPanel("Review", value = "panel2", actionButton("DI_data", "De-Identify Data"))
      )
      
    ),
    # Main panel
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Raw Data", DTOutput("data_table")),
                  tabPanel("De-Identified Data", DTOutput("DI_table")),
                  tabPanel("Summary", tableOutput("summary"), tableOutput("records"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Read the uploaded raw data file
  data <- reactive({
    req(input$file)
    read_delim(input$file$datapath, delim = input$separator, col_names = input$header)
  })
  
  # Read the uploaded freetext identifier file
  freetext <- reactive({
    if (input$uploadMask == "Upload File") {
      req(input$masknames)
      import(input$masknames$datapath)
    } else {
      data.frame(Identifiers = NA)  # Dummy data if no masknames file is uploaded
    }
  })
  
  # Dynamically update variable prefixes based on selected variables
  output$variable_prefixes <- renderUI({
    variable_names <- input$variables
    if (length(variable_names) > 0) {
      lapply(variable_names, function(variable) {
        textInput(paste0("prefix_", variable), paste("Prefix for", variable), value = "")
      })
    }
  })
  
  # Reactive function to get the prefixes for all selected variables
  variable_prefixes <- reactive({
    variable_names <- input$variables
    prefixes <- data.frame(
      Variable = variable_names,
      Prefix = sapply(variable_names, function(variable) {
        input[[paste0("prefix_", variable)]]
      }),
      stringsAsFactors = FALSE
    )
    prefixes
  })
  
  anonymise<-function(x){
    salt=input$salt
    front_salt=substring(salt,1,nchar(salt)/2)
    end_salt=substring(salt,(nchar(salt)/2)+1,nchar(salt))
    x<-paste0(front_salt,x,end_salt)
    anon_x<-sapply(x, function(y) digest(y,"sha256"))
    unname(anon_x)
  }
  
  # Update variable selection choices based on the uploaded file
  observe({
    req(data())
    updateSelectInput(session, "variables", choices = names(data()))
  })
  observe({
    req(data())
    updateSelectInput(session, "variablesrem", choices = names(data()))
  })
  
  # Display the table
  output$data_table <- renderDT({
    req(data())
    selected <- data() %>% select(-input$variablesrem)
    datatable(selected)
  })
  
  # Generate a summary of the data ----
  output$summary <- renderTable({
    columns_info <- data.frame(
      Index = seq_len(ncol(data())),
      Column_Name = names(data()),
      Data_Type = sapply(data(), function(x) class(x)[1]),
      Number_of_records = sapply(data(), function(x) length(unique(x)))
    )
  })
  
  
  # Generate key
  key <- reactive({
    req(data(), input$variables)
    # distinct identifier columns
    selected <- data()[, input$variables, drop = FALSE]%>% distinct()
    # Get the prefixes data frame
    prefixes <- variable_prefixes()
    
    key_data<-data()[, input$variables, drop = FALSE]%>% distinct()%>%
      rename_with(~paste0("Anon", .), everything())%>%
      mutate(across(everything(), ~anonymise(.)))%>%
      mutate(across(everything(), ~paste0(str_sub(.,1,5),str_sub(.,-5,-1))))%>%
      bind_cols(selected)
    
    # Add prefixes to the corresponding columns
    for (i in seq_along(prefixes$Variable)) {
      variable <- prefixes$Variable[i]
      prefix <- prefixes$Prefix[i]
      key_data[[paste0("Anon", variable)]] <- paste0(prefix, key_data[[paste0("Anon", variable)]])
    }
    
    key_data
    
  })
  
  # anonymise data
  anony_data <- reactive({
    # Progress indicator starts
    withProgress(message = 'Anonymizing Data...', value = 0, {
      for (i in 1:100) {
        # Simulate key generation process (replace this with your actual key generation logic)
        Sys.sleep(0.01)
        incProgress(1/100, detail = paste(i, "%"))
      }
      req(data(), input$variables, key())
      ddi<-data()%>% left_join(key())%>%
        select(-input$variables)%>%
        select(-input$variablesrem)%>%
        select(contains("Anon"),everything()) %>% 
        # mask free-text identifiers if any
        mutate(across(everything(), function(x) {
          if (length(freetext()$Identifiers!=0)) {
            str_replace_all(x, paste0(freetext()$Identifiers,collapse="|"), "XXX")
          } else {x}}))
    })
  })
  
  # render De-identified Data
  observeEvent(input$DI_data, {
    output$DI_table <- renderDT(
      anony_data()
    )
  })
  
  
  # export key
  output$key <- downloadHandler(
    filename = function() {
      paste("key", Sys.Date(), ".csv", sep = "")
    },content = function(file) {
      # Progress indicator starts
      withProgress(message = 'Generating Key...', value = 0, {
        for (i in 1:100) {
          # Simulate key generation process (replace this with your actual key generation logic)
          Sys.sleep(0.01)
          incProgress(1/100, detail = paste(i, "%"))
        }
        req(key())
        key <- key()
        write.csv(key, file, row.names = FALSE)
      })
    }
  )
  
  # export anonymized data
  output$ddi <- downloadHandler(
    filename = function() {
      paste("de-i-file", Sys.Date(), ".csv", sep = "")
    },content = function(file) {
      req(anony_data())
      anony_data_result <- anony_data()
      write.csv(anony_data_result, file, row.names = FALSE)
    }    
  )
  
  
  # export tokenized text
  output$tokenft<-downloadHandler(
    filename = function() {
      paste("freetext", Sys.Date(), ".csv", sep = "")
    },content = function(file) {
      req(data(), input$variables)
      selected <- data()[]%>%select(-input$variables)%>%
        select_if(is.character)%>%
        select_at(vars(-c(contains("Date"))))%>%
        pivot_longer(everything())%>% distinct()
      
      preprocess_and_tokenize <- function(df, col_names) {
        # Define a function to preprocess and tokenize text in a single column
        preprocess_text <- function(text) {
          text <- str_replace_all(text, "[^a-zA-Z ]", "") # Remove numbers and symbols
          words <- str_split(text, "\\s+")           # Tokenize by space
          words <- unlist(words)
          words <- words[words != ""]                # Remove empty strings
          return(words)
        }
        
        # Apply the preprocessing function to each specified column
        processed_text <- df %>%
          select(all_of(col_names)) %>%
          mutate(across(all_of(col_names), ~map(.x, preprocess_text))) %>%
          unlist() %>%
          unlist()
        
        # Create a data frame with word frequencies
        word_freq <- table(processed_text)
        return(word_freq)
      }
      
      words <- preprocess_and_tokenize(selected, c("value"))
      write.csv(words, file, row.names = FALSE)
    }
  )
}
# Run the application 
shinyApp(ui = ui, server = server)
