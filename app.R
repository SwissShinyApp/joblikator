

# loads all dependencies and libraries
source("global.R", local = F)
# IMPORT R MODULES
# source(here("R_MODULES","core_functions.R"), local = F)
source(here("R_MODULES","ui_helper.R"), local = F)
source(here("R_MODULES","module_start.R"), local = F)
source(here("R_MODULES","module_job.R"), local = F)
source(here("R_MODULES","module_cv.R"), local = F)
source(here("R_MODULES","module_letter.R"), local = F)
source(here("R_MODULES","module_newuser.R"), local = F)
# source(here("R_MODULES","module_control.R"), local = F)
 # source(here("R_MODULES","ods_data_helper.R"), local = F)
#source(here("R_MODULES","ods_table.R"), local = F)


### INITIALIZE THE DATA STRUCTURE -------
params <- list(
  ui_elems = get_ui_elems()
)

get_r_control<-function(){
  r_control <- reactiveValues(
    goto_tab = NULL,
    ready_system = TRUE,
    ready_user = FALSE,
    ready_job = FALSE,
    ready_cv = FALSE,
    ready_letter = FALSE,
    new_user = 0,
    is_job = TRUE,
    newuser = NULL,
  )
  return(r_control)
}

get_r_data <- function(){
  r_data <- reactiveValues(
    user = import_user(), # this is populated when user is created
    user_data = NULL, # this is populated when user is selected
    job_data = NULL, # this is populated when job is selected/confirmed
    cv_data = NULL, # this is populated from the AI
    letter_data = NULL
  )
  return(r_data)
}


### UI APP PART -----------
ui <- navbarPage(id = "main_tabset",

  title =  tags$img(src='https://cdn.odapes.app/img/logo_joblikator_wide1.png', height = "100%"),

  header =  tagList(tags$link(rel = "stylesheet", type = "text/css", href = "CSS/odapes_stylesheet.css"),
                    tags$link(rel = "stylesheet", type = "text/css", href = "CSS/ods_navbar.css"),
                    useShinyjs()
  ),
  # 
  footer = fixedPanel(
    bottom = 8,
    left = 0,
    height = 20,
    width = "100%",
    draggable = FALSE,
    wellPanel(style = "height: 28px; padding: 5px 30px 5px 30px; background-color: #ffffffe3; border: 0px solid transparent; border-radius: 0px; margin-bottom: 0px;",
              fluidRow(uiOutput("footer_txt"))
    )
  ),
  # 
  # Starter panel
  tabPanel(title = "Start", icon = icon("rocket"),
           value = "tab_0",
           MODULE_START_UI("start_module", params=params)
  ),

# job definition panel
tabPanel(title = "Job", icon = icon("briefcase"),
         value = "tab_1",
         MODULE_JOB_UI("job_module", params=params)
),

  # CV tweak panel
  tabPanel(title = "CV", icon = icon("star"),
           value = "tab_2",
           MODULE_CV_UI("cv_module", params=params)
  ),

  # letter tweak panel
  tabPanel(title = "Letter", icon = icon("envelope"),
           value = "tab_3",
           MODULE_LETTER_UI("letter_module", params=params)
  ),

  # settings and menus
  navbarMenu(
    title = NULL,
    icon = icon("bars", lib = "font-awesome"),

    tabPanel(title = "Control", icon = icon("gauge-high"),
             value = "tab_13",
    ),

    tabPanel(title = "New User", icon = icon("star"),
             value = "tab_11",
             MODULE_NEWUSER_UI("user_module", params=params)
    ),
    tabPanel(title = "Upload Data", icon = icon("star"),
             value = "tab_12"),
    tabPanel("More")
  ),
  # 
 
  
)


# Define server logic------- 
server <- function(input, output, session) {
  
  # IMPORTS DATA
  r_data <- get_r_data()
  r_control <- get_r_control()
  r_comm <- reactiveValues(footer_txt = NULL)
  
  # CALL THE MODULES
  MODULE_START_SERVER("start_module", r_data, r_control, r_comm, params)
  
  MODULE_JOB_SERVER("job_module", r_data, r_control, r_comm, params)
  
  MODULE_CV_SERVER("cv_module", r_data, r_control, r_comm, params)
  
  MODULE_LETTER_SERVER("letter_module", r_data, r_control, r_comm, params)
  
  MODULE_NEWUSER_SERVER("user_module", r_data, r_control, r_comm, params)
  
  # Jumps to other tab
  observeEvent(r_control$goto_tab, {
    req(r_control$goto_tab)
    id <- paste0("tab_", r_control$goto_tab)
    updateTabsetPanel(session, "main_tabset",
                      selected = id)
    r_control$goto_tab <- NULL
  })
  
  # rendering top text (guiding) // is a UI because changes color
  output$footer_txt <- renderUI({r_comm$footer_txt})
}

# Run the application 
shinyApp(ui = ui, server = server)