## Module Job (Tab 1)


#####-------------------- UI RELATED FUNCTIONS ----------------------------------#####

# write either a list of params %in% which the value must be or a funciton using 
## ATENTION, THIS LIST MUST CONTAIN ALL IDS FROM THE BELOW TAGLIST, EVEN IF ITS NOT CHECKED (THEN WRITE NULL)
get_job_checks <- function(user_data){
  list(
    #job_id = list(fnc = "nchar(x)>5", txt = "Job ID must be at least 5 characters"),
    job_link = NULL,
    language = list(fnc = list(user_data$languages), txt = "Select a valid Language"),
    cv_type = list(fnc =  list(user_data$cv_type), txt = "Select a valid Job Type"),
    openai_api_key = NULL,
    has_letter = NULL,
    job_desc = list(fnc = "nchar(x)>200", txt = "Job Description must be at least 200 characters.")
  )
}

# creates the input form
job_ui_form <- function(ns, params){
  tagList(
    normal_picker(ns, "language", data = NA, ttl = "Language"),
    normal_picker(ns, "cv_type", data = NA, ttl = "Job Type"),
    
    # Choose directory
    tags$hr(),

    fluidRow(id = ns("ui_form_2"),
             column(6, switch_lbl(ns, "is_job", "Job Specific", "General CV")),
             column(6, switch_lbl(ns, "has_letter", "Include Letter", "No Letter")),
    ),
    txt_inp_normal(ns, "job_link", "Link of Job", plh = "www.indeed.com/mydreamjob"),
    txt_area_6line(ns, "job_desc", "Job Description", plh = "paste job description here"),
    txt_inp_wide(ns, "openai_api_key", "OpenAI API Key"),
    tags$hr(),
  )
} 

# REnders the ui elements ready
job_reset <- function(session, params, r_data, r_control){

  user_data <- r_data$user_data
  # reset the UI inputs
  #updateTextInput(session, "job_id", placeholder = "unique job ID", value = NA)
  updateTextInput(session, "openai_api_key", value = user_data$personal_data$openai_api_key)
  updateTextAreaInput(session, "job_desc", placeholder = "paste job description here", value = NA)
  updatePickerInput(session, "language", choices = user_data$languages)
  updatePickerInput(session, "cv_type",  choices = user_data$cv_type)
  updateSwitchInput(session, inputId = "is_job", value = TRUE)
  updateSwitchInput(session, inputId = "has_letter", value = FALSE)
  

  # reset reactives
  r_data$job_data = NULL
  r_data$cv_data = NULL
  r_data$letter_data = NULL
  
  # control reset
  r_control$ready_job <- FALSE
  r_control$ready_cv <- FALSE
  r_control$ready_letter <- FALSE

  # returns the text
  set_ui_ready(session = session, params = params, "Please fill in the Information.")
}

#####-------------------- SERVER RELATED FUNCTIONS ----------------------------------#####
## STORE JOB DESC ---------

# stores the job description
store_job_desc <- function(job_desc_text) {
  
  # Set the file path
  file_path <- here("PY_MODULES",  "job_desc.txt")
  
  # Delete the file if it exists
  if (file.exists(file_path)) {
    file.remove(file_path)
  }
  writeLines(job_desc_text, file_path)
  res <- file.exists(file_path)
  return(res)
}


copy_skill_style <- function(project_dir, cv_type, language){
  # Set the file path
  source_path <-  file.path(project_dir, "data", "cv_data", cv_type)
  file_name <- paste0("skill_style_", language, ".txt")

  res <- copy_file(source_path = source_path, destination_path = here("PY_MODULES"), file_name = file_name, dest_name = file_name)
  return(res)
  
}

# copy CV
copy_cv <- function(project_dir, cv_type, language){
  # Set the file path
  source_path <-  file.path(project_dir, "data", "cv_data", cv_type)
  file_name <- paste0("cv_txt_", cv_type, "_", language, ".txt")
  
  res <- copy_file(source_path = source_path, destination_path = here("PY_MODULES"), file_name = file_name, dest_name = "cv.txt")
  return(res)
  
}

# checks if all selected exist
check_data_exist <- function(project_dir, cv_type, language){
  # project_dir <- "/Users/michaelrubin/Library/Mobile Documents/com~apple~CloudDocs/DEV STUDIO/JOBlikatoR/www/resources/G6SXhrKdEdg8S7zo24xs"
  # cv_type <- "type_2"
  # language <- "en"
  dir <- file.path(project_dir, "data", "cv_data")
  res1 <- dir.exists(file.path(dir, cv_type))%>% isTRUE()
  res2 <- file.exists(file.path(dir, cv_type,  paste0("work_",cv_type,"_",language,".csv")))%>% isTRUE()
  res3 <- file.exists(file.path(dir, cv_type,  paste0("education_",cv_type,"_",language,".csv")))%>% isTRUE()
  res4 <- file.exists(file.path(dir, cv_type,  paste0("cv_txt_",cv_type,"_",language,".txt")))%>% isTRUE()
  res5 <- file.exists(file.path(dir, cv_type,  paste0("CV_",cv_type,"_",language,".Rmd")))%>% isTRUE()
  
  res6 <- file.exists(here("PY_MODULES",  "job_desc.txt"))%>% isTRUE()
  res7 <- file.exists(here("PY_MODULES",  "cv.txt"))%>% isTRUE()
  
  return(all(res1, res2, res3, res4, res5, res6, res7))
}

# # stores letter texts to PY env 
# store_letter <- function(letter_data){
#   # NOTE: I dont need the inputs for the letter section, only the skillset. the rest is standard and uses the same file in py folder
# 
#   if (!check_value(letter_data)){return(FALSE)}
#   file_skill <- here("PY_MODULES",  "skill.txt")
#   # file_strong <- here("PY_MODULES",  "strong.txt")
#   # file_company <- here("PY_MODULES",  "company.txt")
#   
#   # Delete the file if it exists
#   if (file.exists(file_skill)) {file.remove(file_skill)}
#   # if (file.exists(file_strong)) {file.remove(file_strong)}
#   # if (file.exists(file_company)) {file.remove(file_company)}
#   
#   # write the files
#   writeLines(letter_data$skill_default, file_skill)
#   # writeLines(letter_data$strong_default, file_strong)
#   # writeLines(letter_data$company_default, file_company)
#   
#   # check if success
#   res1 <- file.exists(file_skill)
#   res2 <- file.exists(file_strong)
#   res3 <- file.exists(file_company)
#   
#   return(all(res1, res2, res3))
#   
# }


# copy skillset
copy_skillset <- function(project_dir, cv_type, language){
  # Set the file path
  source_path <-  file.path(project_dir, "data", "cv_data", cv_type)
  file_name <- paste0("skillset_", cv_type, "_", language, ".txt")
  res <- copy_file(source_path = source_path, destination_path = here("PY_MODULES"), file_name = file_name, dest_name = "skill_set.txt")
  return(res)
  
}

# import default letter data
import_default_letter <- function(project_dir, cv_type, language){

  file_name <- paste0("letter_", cv_type, "_", language, ".csv")
  file_path <- file.path(project_dir, "data", "cv_data", cv_type, file_name)
  
  if (file.exists(file_path)){
    letter_data <- read_csv(file_path) %>%
      pivot_wider(names_from = "variable", values_from = "value") %>%
      as.list()
    return(letter_data)
  } else {
    return(NULL)
  }
  
}

## MAIN RUNNER ---------
# Onboard the job data
get_job_data <- function(input_vec, user_data){
  # if all good pass values to r_data
  job_data <- list(
    job_link = input_vec$job_link,
    openai_api_key = input_vec$openai_api_key,
    language = tolower(input_vec$language),
    cv_type = input_vec$cv_type,
    job_desc = input_vec$job_desc,
    has_letter = input_vec$has_letter
  )

  # default values
  job_data$default_highlight <- read_csv(paste(user_data$project_dir, "data", "cv_data", input_vec$cv_type, paste0(input_vec$cv_type, "_default_highlight.csv"), sep="/")) %>%
    pivot_wider(names_from = "variable", values_from = "value") %>%
    as.list()
    
  ### HERE WEB SCRAPING???
  # store the job desc
  res1 <- store_job_desc(job_data$job_desc)
  
  # copy the CV data
  res2 <- copy_cv(user_data$project_dir, job_data$cv_type, job_data$language)
  
  res5 <- copy_skill_style(user_data$project_dir, job_data$cv_type, job_data$language)
  

  # import the letter data Is ot checked!
  if (isTRUE(job_data$has_letter)){
    res4 <- copy_skillset(user_data$project_dir, job_data$cv_type, job_data$language)
    job_data$letter_data <- import_default_letter(user_data$project_dir, job_data$cv_type, job_data$language)
  }
  
  
  # check if 
  res3 <- check_data_exist(user_data$project_dir, job_data$cv_type, job_data$language)
  
  if (all(res1, res2, res3)){
    return(job_data)
  } else {
    a+b # provike error
  }

}


#####-------------------- SHINY MODULES UI ----------------------------------#####
MODULE_JOB_UI <- function(id, params) {
  ns <- NS(id)
  tagList(
    # renders all basic UI elements // hides certain elements
    column(6,offset = 3,
           
           fluidRow(h2("Type of CV")),
           tags$hr(),
           # page status
           fluidRow(id = ns("start_ui"), user_sts_page("sts_start", ns = ns, params = params)),
           fluidRow(id = ns("success_ui"), user_sts_page("sts_success", ns, params))%>%shinyjs::hidden(),
            #  # job form ui elements
            fluidRow(id = ns("ui_form"), job_ui_form(ns, params)) %>%shinyjs::hidden(),
           # # buttons
            fluidRow(id = ns("btrw_start"), btn_line(c("btn_start"),ns = ns, params = params)) %>% shinyjs::hidden(),
           fluidRow(id = ns("btrw_ready"), btn_line(c("btn_back", "btn_confirm"),ns = ns, params = params))%>%shinyjs::hidden(),
            fluidRow(id = ns("btrw_next"), btn_line(c("btn_reset", "btn_next"),ns = ns, params = params))%>%shinyjs::hidden(),
           tags$br(),
           tags$br()
    )
  )
}



#####-------------------- SERVER PART ----------------------------------#####
MODULE_JOB_SERVER <- function(id, r_data, r_control,r_comm, params) {
  moduleServer(id,function(input, output, session) {
    
    ##-------------- APERO----------------
    ns <- session$ns
    # creating new Temp Var for transfer inside module // SOME OR ALL WILL COME FROM CENTRALIZED R_ PART
    temp_vals<-reactiveValues(
      check_list = NULL
    )

    ##-------------- START AND TAB HANDLING ----------------
    # Sets or Resets initial state or success state depending if i have already user data
    observeEvent(r_control$ready_user, {
      req(r_control$ready_user)
      temp_vals$check_list <- get_job_checks(r_data$user_data)
      r_comm$footer_txt <- job_reset(session, params,  r_data, r_control)
      
    })

    # reset the page
    observeEvent(input$btn_reset,{
      req(r_control$ready_user)
      temp_vals$check_list = get_job_checks(r_data$user_data)
      r_comm$footer_txt <- job_reset(session, params, r_data, r_control)
    })
    
    # jumps to next tab
    observeEvent(input$btn_next,{r_control$goto_tab <- 2})
    
    # jumps to former tab
    observeEvent(input$btn_back,{r_control$goto_tab <- 0})
    
    # rendering top text (guiding) // is a UI because changes color
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
    
    # controls of specific or general CV
    observeEvent(input$is_job, {
      req(r_control$ready_user)
      r_control$is_job <- input$is_job
    })
    
    
    # controls of specific or general CV
    observeEvent(input$has_letter, {
      req(r_control$ready_user)
      r_control$has_letter <- input$has_letter
    })
    
    
    
    observeEvent(r_control$is_job, {

      req(r_control$ready_user)
      if (isFALSE(r_control$is_job)){
        shinyjs::hide("job_link")
        shinyjs::hide("job_desc")
        updateSwitchInput(session, inputId = "has_letter", value = FALSE)
        shinyjs::hide("has_letter")
        

        shinyjs::hide("openai_api_key")
        updateTextAreaInput(session, "job_desc", value =  paste(rep("xxxxxxx", 60), collapse = ""))
      } else {
        shinyjs::show("job_link")
        shinyjs::show("job_desc")
        shinyjs::show("has_letter")
        shinyjs::show("openai_api_key")
        updateTextAreaInput(session, "job_desc", placeholder = "paste job description here", value = NA)
      }
    })
    
    
    # On click go, loads the users data
    observeEvent(input$btn_confirm, {
      req(input$btn_confirm)
      req(r_data$user_data)
      r_data$job_data <- tryCatch({
        job_data <- get_job_data(input_vec = input_listener_d(), r_data$user_data)
        r_control$ready_job <- TRUE
        job_data
      }, error = function(e) {
        showNotification("Something went wrong. Start over.", type = "error")
        NULL
      })
    })

    # renders the confirmation ui and goes to next tab
    observeEvent(r_control$ready_job,{
      req(r_control$ready_job)
      r_comm$footer_txt <- set_ui_success(session = session, params = params, paste("Job data are stored. You can go ahead!"))
      r_control$goto_tab <- 2

    })

  })
}