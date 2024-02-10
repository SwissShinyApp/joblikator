## MODULE NEW USER SETUP


#####-------------------- UI RELATED FUNCTIONS ----------------------------------#####
# write either a list of params %in% which the value must be or a funciton using x
get_newuser_checks <- function(){
  list(
    user_name = list(fnc = "nchar(x)>5 & nchar(x)<40", txt = "Name must be between 5 and 40 characters."),
    position = list(fnc = "nchar(x)>5 & nchar(x)<40", txt = "Position must be between 5 and 40 characters."),
    address = NULL,
    phone = NULL,
    email = list(fnc = "is_valid_email(x)", txt = "Your Email must be valid."),
    www = NULL,
    github = NULL,
    linkedin = list(fnc = "nchar(x)>5", txt = "Linkedin must be informed."),
    highlight_1 = NULL,
    highlight_2 = NULL,
    highlight_3 = NULL,
    birthday = NULL, #list(fnc = "is_date(x) | is.na(x)", txt = "Must be a valid date or nothing."),
    private = NULL,
    openai_api_key = NULL,
    project_dir = list(fnc = "dir.exists(x)", txt = "Select a valid Dir."),
    profilpic = NULL,
    color = list(fnc = "valid_hex_color(x)", txt = "Please provide a valid Hex color, e.g. #2596be.")
  )
}

# creates the form of the user
newuser_ui_form <- function(ns, params){
  tagList(
    # user name input
    fluidRow(id = ns("ui_form_1"),
      txt_inp_normal(ns, "user_name", "Name"),
      txt_inp_normal(ns, "position", "Position"),
      txt_inp_wide(ns, "address", "Address"),
      txt_inp_normal(ns, "phone", "Phone"),
      txt_inp_normal(ns, "email", "E-Mail"),
      txt_inp_normal(ns, "www", "Website"),
      txt_inp_normal(ns, "github", "Github"),
      txt_inp_normal(ns, "linkedin", "Linkedin"),
      txt_inp_normal(ns, "birthday", "Birth date"),
      txt_inp_normal(ns, "private", "Private Information"),
      txt_inp_wide(ns, "openai_api_key", "OpenAI Token"),
      tags$hr(),
      image_upload(ns, "profilpic"),
      txt_inp_normal(ns, "color", "CV decoration color"),
      txt_inp_wide(ns, "highlight_1", "Your Key Strength 1"),
      txt_inp_wide(ns, "highlight_2", "Your Key Strength 2"),
      txt_inp_wide(ns, "highlight_3", "Your Key Strength 3"),
      tags$hr(),
      # Choose directory
      fluidRow(id = ns("ui_form_2"),
               column(12, p("Please provide a Output Directory from your Computer. All data will be stored under this directory. You can select or type it.", style = "font-style: italic;")),
               column(9,txt_inp_wide(ns, "project_dir", "Project Directory")),
               column(3, dir_button(ns, "folder"), style = "margin-top: 24px;")
        ),
      tags$hr(),
    )
  )
}

# REnders the ui elements ready
newuser_reset <- function(session, params, r_data, r_control){

  updateTextInput(session, "user_name", placeholder = "Unique name is required", value = NA)
  updateTextInput(session, "position", placeholder = "Position is required", value = NA)
  updateTextInput(session, "address", placeholder = "your address, not required", value = NA)
  updateTextInput(session, "phone", placeholder = "your phone, not required", value = NA)
  updateTextInput(session, "email", placeholder = "Email is required", value = NA)
  updateTextInput(session, "www", placeholder = "michaelcrubin.github.io/resume_mcr", value = NA)
  updateTextInput(session, "github", placeholder = "michaelcrubin", value = NA)
  updateTextInput(session, "linkedin", placeholder = "michael-rubin-odapes", value = NA)
  updateTextInput(session, "birthday", placeholder = "your birth date, not required", value = NA)
  updateTextInput(session, "private", placeholder = "some additional info, e.g. marriage status", value = NA)
  updateTextInput(session, "openai_api_key", placeholder = "your OpenAI token", value = NA)
  updateTextInput(session, "color", placeholder = "#000000", value = "#000000")
  updateTextInput(session, "highlight_1", value = "I am always on time, reliable and motivated.")
  updateTextInput(session, "highlight_2", value = "Profound experience in peeling pineapples and oranges.")
  updateTextInput(session, "highlight_3", value = "Strong network across different industries and countries.")
  
  # reset reactives
  #r_data$user = NULL
  r_data$user_data = NULL
  r_data$job_data = NULL
  r_data$cv_data = NULL
  r_data$letter_data = NULL
  
  # control reset
  r_control$ready_system <- FALSE
  r_control$ready_user <- FALSE
  r_control$ready_job <- FALSE
  r_control$ready_cv <- FALSE
  r_control$ready_letter <- FALSE
  
  
  set_ui_ready(session = session, params = params, "Important: Your information are stored exclusively on your machine!")
}
nchar("Profound experience in peeling pineapples and oranges.")

#####-------------------- SERVER RELATED FUNCTIONS ----------------------------------#####


## DIRECTORY RELATED ---------

# creates a unique dir // recursive
create_unique_directory <- function(directory, suffix = "", count = 0) {
  if (!dir.exists(directory)) {
    dir.create(directory)
    return(directory)
  }
  
  base_directory <- sub(paste0("_", count), "", directory)
  new_directory <- paste0(base_directory, "_", count + 1)
  return(create_unique_directory(new_directory, suffix, count + 1))
}

# copies the project dir structure + files
copy_proj_dir <- function(parent_dir, source_dir, user_name){

  # new dir path
  new_dir <- file.path(parent_dir, paste0("jobs_", user_name))
  
  # create unique dir
  def_dir <- create_unique_directory(new_dir)

  # Move the contents of the original directory to the temporary directory
  file.copy(from = source_dir, to = def_dir, recursive = TRUE)
  
  
  # parent_dir <- "/Users/michaelrubin/Downloads"
  # source_dir <- here("www", "resources")
  # user_name <- "Ali Baba"
  # Create a temporary directory with a different name
  # temp_dir <- tempfile()
  # dir_list <- generate_dir_list(source_dir) 
  # 
  # create_dir_structure(dir_list, temp_dir, source_dir)
  # 
  # temp_dir <- file.path(parent_dir, names(dir_list)[1])
  # project_dir <- gsub("/[^/]*$", paste0("/", "jobs_", user_name), temp_dir)
  # 
  # # Specify the directory structure
  # file.rename(temp_dir, project_dir)
  
  return(def_dir)
  
}

# checks if dir structure exists
check_dir <- function(dir){
  res1 <- dir.exists(file.path(dir)) %>% isTRUE()
  res2 <- dir.exists(file.path(dir, "data"))%>% isTRUE()
  res3 <- dir.exists(file.path(dir,"data", "meta_data"))%>% isTRUE()
  res4 <- dir.exists(file.path(dir, "data", "cv_data"))%>% isTRUE()
  res5 <- dir.exists(file.path(dir,"data", "appendix"))%>% isTRUE()
  res7 <- file.exists(file.path(dir,"data", "meta_data", "language.csv"))%>% isTRUE()
  res8 <- file.exists(file.path(dir,  "data", "meta_data", "cv_type.csv"))%>% isTRUE()
  
  return(all(res1, res2, res3, res4, res5, res7, res8))
}

## Creates the Project Structure
create_project <- function(parent_dir, user_name){
  source_dir <- here("www", "resources", "data")
  usnm <- make_user_name(user_name) # simplified name for folder
  project_dir <- tryCatch({
    # Call the function to create the dir structure
    copy_proj_dir(parent_dir, source_dir, usnm)
  }, error = function(e) {
    # Error handling code
    NULL
  })
  
  # checks the dir structure
  return(list(success = check_dir(project_dir), project_dir = project_dir))

}


## PERSONAL DATA RELATED ---------

# stores the pic // returns the file name
store_pic <- function(profilpic, project_dir){

  # if it is no fileimport type then I return the default picture
  if (!check_value(profilpic)){return("my_true_self.jpeg")}
  if (!is.data.frame(profilpic)){return("my_true_self.jpeg")}
  if (!"datapath" %in% names(profilpic)){return("my_true_self.jpeg")}
  
  
  file_name <- getExtension(profilpic$datapath) %>% paste0("me", .) 
  file_path <- file.path(project_dir, "data", "meta_data", file_name)
  file.copy(profilpic$datapath, file_path, overwrite = TRUE)
  
  if (file.exists(file_path)){
    return(file_name)
  } else {
    return("my_true_self.jpeg")
  }
}

# stores the personal data returns T/F // attention gets the full input vector and filters the needed values
store_personal_data <- function(input_vec, project_dir){
  # store the image
  
  pic_name <- store_pic(input_vec$profilpic, project_dir)
  
  personal_tostore <-input_vec %>% replace_null_vls(by = "") %>% as_tibble() %>%
    dplyr::select(any_of(c("name" = "user_name" ,"address","phone","email","www","github","linkedin","birthday","private","openai_api_key"))) %>%
    mutate(profilpic = pic_name) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  birthday <- input_vec$birthday
  phone <- input_vec$phone
  

  default_highlight_tostore <-input_vec %>% replace_null_vls(by = "") %>% as_tibble() %>%
    dplyr::select(any_of(c("highlight_1_en" = "highlight_1","highlight_2_en" = "highlight_2","highlight_3_en" = "highlight_3", "position", "color"))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "value")
  
  path <- file.path(project_dir, "data", "meta_data")

  # savely store new csv
  res_save_1 <- save_check_csv(personal_tostore, path, "personal.csv")
  res_save_2 <- save_check_csv(default_highlight_tostore, path, "default_highlight.csv")
  res_save_3 <- writeLines(birthday, paste(path, "birthday.txt", sep = "/"))
  res_save_4 <- writeLines(phone, paste(path, "phone.txt", sep = "/"))
  
  return(list(success = all(res_save_1, res_save_2)))
}


## USER RELATED ---------

# Imports the users and filters by machine id and dir exists
import_user <- function(){
  # filter the users
  user <- read_csv(here("data", "user.csv")) %>% 
    dplyr::filter(machine == Sys.info()["user"]) %>%
    mutate(exists = map_lgl(project_dir, ~check_dir(.x))) %>%
    dplyr::filter(exists) 
}

# stores the user directory file
store_user_dir <- function(email, user_name, project_dir){

  # create user name
  user_old <- read_csv(here("data", "user.csv"))
  # check if double
  double_nm <- email %in% user_old$user_id
  double_dir <- project_dir %in% {user_old %>% dplyr::filter(machine == Sys.info()["user"]) %>% pull(project_dir)}
  
  if (any(double_nm, double_dir)){
    return(list(success = FALSE, user = import_user()))
  }
  
  # if (user_name %in% user_old$user_name) {
  #   user_name <- paste(user_name, as.integer(Sys.time()), sep = "_")
  # }
  # 
  # Check if the last character is numeric
  
  
  # adds the new data
  user_tostore <- user_old %>% 
    add_case(user_id = email,
             user_name = user_name,
             machine = Sys.info()["user"],
             project_dir = project_dir
    ) %>% dplyr::select(any_of(c("user_id", "user_name", "machine", "project_dir")))
  
  # savely store new csv
  res_save <- save_csv_overwrite(user_tostore, here("data"), "user.csv")
  
  return(list(success = res_save, user = import_user()))
}


## MAIN RUNNER ---------
# manages process of new user adding
open_new_user <- function(input_vec, user){

  # create a project dir
  ret_1 <- create_project(input_vec$project_dir, input_vec$user_name)
  project_dir <- ret_1$project_dir
  
  # stores personal data
  ret_2 <- store_personal_data(input_vec, project_dir)
  
  if (!all(ret_1$success, ret_2$success)){
    showNotification("Something went wrong. Try to use another directory.", type = "error")
    return(user)
  }

  # store dir in user table
  ret_3 <- store_user_dir(input_vec$email, input_vec$user_name, project_dir)
  
  if (ret_3$success){
    showNotification("All right. New user created.", type = "message")
  } else {
    showNotification("Something went wrong. Possibly you used a Directory, user name or email, which is already in use.", type = "error")
  }
  return(ret_3$user)
}


#####-------------------- SHINY MODULES UI ----------------------------------#####
MODULE_NEWUSER_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    # renders all basic UI elements // hides certain elements
    column(6,offset = 3,
           
           fluidRow(h2("Set-up a new User")),
           fluidRow(p("Important: All your information are stored exclusively on your machine. The only information stored on our premises are your user name and your project directory.")),
           tags$hr(),
           
           #fluidRow(uiOutput(ns("top_text"))),
           # page status
           fluidRow(id = ns("success_ui"), user_sts_page("sts_success", ns, params))%>%shinyjs::hidden(),
            #  # job form ui elements
            fluidRow(id = ns("ui_form"), newuser_ui_form(ns, params)) %>% shinyjs::hidden(),
           # # buttons
            fluidRow(id = ns("btrw_ready"), btn_line(c("btn_confirm"),ns = ns, params = params)) %>% shinyjs::hidden(),
            fluidRow(id = ns("btrw_next"), btn_line(c("btn_reset", "btn_next"),ns = ns, params = params))%>%shinyjs::hidden(),
           
           tags$br(),
           tags$br()
    )
  )
}



#####-------------------- SHINY MODULES SERVER -------------------------------#####
MODULE_NEWUSER_SERVER <- function(id, r_data, r_control, r_comm, params) {
  moduleServer(id,function(input, output, session) {
    
    ##-------------- APERO----------------
    ns <- session$ns
    # creating new Temp Var for transfer inside module // SOME OR ALL WILL COME FROM CENTRALIZED R_ PART
    temp_vals<-reactiveValues(
      check_list = NULL
    )

    ##-------------- START AND TAB HANDLING ----------------
    # Sets or Resets initial state or success state depending if i have already user data
    
    # If the App is ready, I turn the status to ready and show the job_form
    observeEvent(r_control$newuser, {
      req(r_control$newuser)
      temp_vals$check_list <- get_newuser_checks()
      r_comm$footer_txt <- newuser_reset(session, params, r_data, r_control)
    })

    # reset the page
    observeEvent(input$btn_reset,{
      print("STARTS OVER")
      temp_vals$check_list <- get_newuser_checks()
      r_comm$footer_txt <- newuser_reset(session, params, r_data, r_control)
    })

    # jumps to former tab
    observeEvent(input$btn_back,{r_control$goto_tab <- 0})
    
    # rendering top text (guiding) // is a UI because changes color
    # output$top_text <- renderUI({r_comm$footer_txt})
    
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
    
    ## Choose Directory // returns the directory selected
    shinyDirChoose(
      input,
      'folder',
      roots = c(home = path.expand("~")),
      filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
    )
    
    # reactive, calls project opening
    observeEvent(input$folder, {
      req(!is.null(names(input$folder)))
      req("path" %in% names(input$folder))
      project_dir <- parse_dir(input$folder)
      updateTextInput(session, "project_dir", value = project_dir)
    })
    
    # On click confirm, Opens a new jobs and generates highlights
    observeEvent(input$btn_confirm, {
      req(input$btn_confirm)
      # store the files
      input_vec = input_listener_d()
      # creates a new user
      r_data$user <- tryCatch({
        user <- open_new_user(input_vec, r_data$user)
        r_control$ready_system <- TRUE
        user
      }, error = function(e) {
        showNotification("Something went wrong. Try again.", type = "error")
        r_data$user
      })
    })
    
    # renders the confirmation ui and goes to next tab
    observeEvent(r_control$ready_system ,{
      req(r_control$ready_system )
      r_comm$footer_txt <- set_ui_success(session = session, params = params, paste("New user was created. You can use it!"))
      r_control$goto_tab <- 0
    })

  })
}