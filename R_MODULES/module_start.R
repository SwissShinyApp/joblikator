## Module Start Tab 0


#####-------------------- UI RELATED FUNCTIONS ----------------------------------#####

lead_txt <- 
"JOBlikatoR is a free, Open-Source Web-App, which helps you to mass-customize
your job applicaton. It uses GPT-3.5 Model to tailor your CV and motivation letter to each 
specific job announcement and to write your motivation letter. It uses RMarkdown and the vitae 
package to automatically print your document and organizes your CVs and letters nicely. 
Software is developed by Michael C. Rubin.
"

# write either a list of params %in% which the value must be or a funciton using x
get_start_checks <- function(r_data){
  list(
    user_id = list(fnc = list(r_data$user$user_id), txt = "Select a valid user or create a new one.")
    
   # user_data = list(fnc = "dir.exists(x)", txt = "Select a valid user or create a new one.")
    #project_dir = list(fnc = "dir.exists(x)", txt = "The Directory is not valid.")
  )
  
}

# UI form for start page
start_ui_form <- function(ns, params){
  tagList(
    tags$hr(),
    fluidRow(
      column(3, h5("Select User:")),
      column(6, normal_picker(ns, "user_id", data = NA, ttl = NULL)),
      column(3, btn_line(c("btn_go"),ns = ns, params = params))
    ),
    tags$hr(),
    fluidRow(
      column(9, h5("Set Up a new User:")),
      column(3, btn_line(c("btn_newuser"),ns = ns, params = params))
      ),
    tags$hr(),
    fluidRow(
      column(9, h5("Upload Data and Files:")),
      column(3, btn_line(c("btn_upload"),ns = ns, params = params))
    ),
    tags$hr(),

  )
} 

# # REnders the ui elements ready
start_reset <- function(session, params, r_data, r_control){
  # reset the inputs

  user <- r_data$user %>% df_to_lst(vals = "user_id", nms = "user_name")
  updatePickerInput(session, "user_id", choices = user)
  
  
  # reset reactives
  r_data$user_data = NULL
  r_data$job_data = NULL
  r_data$cv_data = NULL
  r_data$letter_data = NULL
  
  # control reset
  r_control$ready_user <- FALSE
  r_control$ready_job <- FALSE
  r_control$ready_cv <- FALSE
  r_control$ready_letter <- FALSE
  
  
  # returns the text
  set_ui_ready(session = session, params = params, "How you like to Start.")
}


#####-------------------- SERVER RELATED FUNCTIONS ----------------------------------#####

# Loads the user data
load_user_data <- function(user_id, user){

  #user_id <- "carina.cardoso16@gmail.com"
 # project_dir <- "/Users/michaelrubin/Downloads/jobs_carina_rubin"
  project_dir <- user%>% dplyr::filter(user_id == !!user_id) %>% dplyr::pull(project_dir)
  # personal data
  personal_data <- read_csv(paste(project_dir, "data", "meta_data", "personal.csv", sep="/")) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    as.list()

  personal_data$birthday <- readLines(paste(project_dir, "data", "meta_data", "birthday.txt", sep="/"))
  personal_data$phone <- readLines(paste(project_dir, "data", "meta_data", "phone.txt", sep="/"))

  # # default values
  # default_highlight <- read_csv(paste(project_dir, "data", "meta_data", "default_highlight.csv", sep="/")) %>%
  #   pivot_wider(names_from = "variable", values_from = "value") %>%
  #   as.list()
  
  # laugnage and job type
  languages <- read_csv(paste(project_dir, "data", "meta_data", "language.csv", sep="/")) %>% df_to_lst(vals = "id", nms = "name")
  cv_type <- read_csv(paste(project_dir, "data", "meta_data", "cv_type.csv", sep="/")) %>% df_to_lst(vals = "id", nms = "name")
  
  # read job
  all_job <- read_csv(paste(project_dir, "data", "meta_data", "all_job.csv", sep="/"))
  
  # read job status
  job_status <- read_csv(paste(project_dir, "data", "meta_data", "job_status.csv", sep="/"))%>%
    mutate(time_stamp = parse_time_format(time_stamp))

  user_data <- list(
    project_dir = project_dir,
    personal_data = personal_data,
    #default_highlight = default_highlight,
    languages = languages,
    cv_type = cv_type,
    all_job = all_job,
    job_status = job_status
  )
    
  return(user_data)
}


#####-------------------- SHINY MODULES UI ----------------------------------#####
MODULE_START_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    # renders all basic UI elements // hides certain elements
    column(6,offset = 3,
           fluidRow(tab_header("Welcome to JOBlikatoR", "Leverage AI to land your dream Job!", 'https://cdn.odapes.app/img/hex_joblikator.png')),
           tags$hr(),
           fluidRow(h6(lead_txt)),  
           tags$hr(),
           #fluidRow(uiOutput(ns("top_text"))),
           # page status
           fluidRow(id = ns("success_ui"), user_sts_page("sts_success", ns, params))%>%shinyjs::hidden(),
            #  # job form ui elements
            fluidRow(id = ns("ui_form"), start_ui_form(ns, params))%>%shinyjs::hidden(),
           #  buttons
            fluidRow(id = ns("btrw_next"), btn_line(c("btn_reset", "btn_next"),ns = ns, params = params))%>%shinyjs::hidden(),
           tags$br(),
           tags$br()
    )
  )
}



#####-------------------- SHINY MODULES SERVER -------------------------------#####

MODULE_START_SERVER <- function(id, r_data, r_control,r_comm, params) {
  moduleServer(id,function(input, output, session) {
    
    ##-------------- APERO----------------
    ns <- session$ns
    # creating new Temp Var for transfer inside module // SOME OR ALL WILL COME FROM CENTRALIZED R_ PART
    temp_vals<-reactiveValues(
      check_list = NULL
    )

    
    ##-------------- START AND TAB HANDLING ----------------

    # If the App is ready, I turn the status to ready and show the job_form
    
    # Sets or Resets initial state or success state depending if i have already user data
    observeEvent(r_control$ready_system, {
      req(r_control$ready_system)
      temp_vals$check_list <- get_start_checks(r_data)
      r_comm$footer_txt <- start_reset(session, params, r_data, r_control)
      
    })
    
    # reset the page
    observeEvent(input$btn_reset,{
      req(r_control$ready_system)
      temp_vals$check_list <- get_start_checks(r_data)
      r_comm$footer_txt <- start_reset(session, params, r_data, r_control)
    })
    
    # jumps to next tab
    observeEvent(input$btn_next,{r_control$goto_tab <- 1})
    
    # jumps to next tab
    observeEvent(input$btn_newuser,{
      r_control$newuser =  input$btn_newuser
      r_control$goto_tab <- 11
    })
    
    # jumps to next tab
    observeEvent(input$btn_upload,{r_control$goto_tab <- 12})
    
    # # rendering top text (guiding) // is a UI because changes color
    # output$top_text <- renderUI({temp_vals$toptxt})
    
    ##-------------- INPUT VALUE CHECKER ----------------    
    # Reactive handler of user form information // updates live text
    # This is the reactive which reacts if any of the Variable_selectors has changed or added
    input_listener_d<-reactive({
      req(temp_vals$check_list)
      map(names(temp_vals$check_list), ~ input[[.x]])%>%setNames(names(temp_vals$check_list))
    })%>% debounce(500)
    
    # Reactive live data Checker
    observe({
      req(input_listener_d())
      print("CHECKS")
      r_comm$footer_txt  <- Input_Checker(
        input_vec = input_listener_d(),
        check_list = temp_vals$check_list,
        params = params)
    })
    
    
    ##-------------- AFTER CONFIRMATION HANDLING ----------------

    # On click go, loads the users data
    observeEvent(input$btn_go, {
      req(input$btn_go)
      req(input$user_id)
      # load user data
      
      r_data$user_data <- tryCatch({
        user_data <- load_user_data(input$user_id, r_data$user)
        r_control$ready_user <- TRUE
        user_data
      }, error = function(e) {
        showNotification("Something went wrong. Try again.", type = "error")
        r_data$user_data
      })
    })
    
    
    # renders the confirmation ui and goes to next tab
    observeEvent(r_control$ready_user,{
      req(r_control$ready_user)
      r_comm$footer_txt <- set_ui_success(session = session, params = params, paste("Your project project_dir has been stored. You can go ahead!"))
      r_control$goto_tab <- 1
    })

  })
}