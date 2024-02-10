# Helper
### THIS MODULE HELPS RENDERING AND UPDATING THE UI IN THE MICROSERVICE CONTEXT

# Dark green: #294a23
#   bright green: #36622e
#   Dark red: #6a1515
#   bright red: #851a1a
#   Dark brown: #6F6659
#   bright brown #C8C2BA


# UI DATA --------------
get_ui_elems <- function(){
  list(
    ui_rows = c("ui_form", "success_ui", "row_confirm", "start_ui", "btrw_start", "btrw_back", "btrw_next", "negative_ui", "preview_ui", "ui_form", "btrw_ready"),
    # buttons
    btn_go = list(
      cls = "ui positive button",
      icn = "play",
      txt = "Create CV"
    ),
    
    btn_confirm = list(
      cls = "ui positive button",
      icn = "thumbs-up",
      txt = "Confirm"
    ),
    
    btn_last = list(
      cls = "ui primary button",
      icn = "rotate-left",
      txt = "Last"
    ),
    
    
    btn_analyze = list(
      cls = "ui positive button",
      icn = "thumbs-up",
      txt = "Analyze by AI"
    ),
    
    btn_newuser = list(
      cls = "ui primary button",
      icn = "user-plus",
      txt = "New User"
    ),
    
    btn_upload = list(
      cls = "ui primary button",
      icn = "file-arrow-up",
      txt = "Upload Stuff"
    ),
    
    btn_print = list(
      cls = "ui primary button",
      icn = "print",
      txt = "Print"
    ),

    btn_prompt = list(
      cls = "ui primary button",
      icn = "comments",
      txt = "Run AI"
    ),
    
    ai_wait_txt = tagList(
      p("AI is Generating your CV...this may take a moment.",
        style = "color: #112446; text-align:center; font-weight:700"
        )
      ),
    
    print_wait_txt = tagList(
      p("Your Document is being printed.",
        style = "color: #112446; text-align:center; font-weight:700"
      )
    ),
    
    
    
    # buttons
    btn_start = list(
      cls = "ui positive button",
      icn = "play",
      txt = "Start"
    ),
    
    btn_next = list(
      cls = "ui positive button",
      icn = "forward-step",
      txt = "Next"
    ),
    btn_reset = list(
      cls = "ui negative button",
      icn = "rotate-left",
      txt = "Restart"
    ),

    btn_back = list(
      cls = "ui negative button",
      icn = "backward-step",
      txt = "Go Back"
    ),
    

    
    
    
    # status and texts
    sts_start = list(
      col = "#DCDCDC",
      icn = "question-circle",
      txt = NULL
    ),
    sts_success = list(
      col = "#36622e",
      icn = "check-circle",
      txt = NULL
    ),
    
    sts_negative = list(
      col = "#6a1515",
      icn = "times-circle",
      txt = NULL
    ),
    
    txt_start = list(
      fnc = h6,
      txt = "Please click below to start.",
      stl = "text-align: center;"
    ),
    
    txt_ready = list(
      fnc = h6,
      txt = "Your data are OK. You can go ahead!",
      stl = "text-align: center;"
    ),
    
    txt_success = list(
      fnc = h6,
      txt = "Done!",
      stl = "text-align: center; color: #36622e"
    ),
    
    txt_negative = list(
      fnc = h6,
      txt = "Some data are missing or incorrect.",
      stl = "text-align: center; color: #6a1515"
    ),
    
    txt_novalues = list(
      fnc = h5,
      txt = "No company values identified",
      stl = "text-align: center;color: #C8C2BA"
    ),
    
    txt_values = list(
      fnc = h6,
      txt = "Company values",
      stl = "text-align: left; color: #6F6659"
    ),
    
    
    txt_preview = list(
      fnc = h6,
      txt = "Please Revise and Check-Out",
      stl = "text-align: center;"
    )
  )
}


# UI ELEMENTS -----------
txt_inp_normal <- function(ns, id, ttl = NULL, vl = NULL, plh = NULL){
  textInput(ns(id), ttl, value = vl, placeholder = plh)
}

txt_inp_wide <- function(ns, id, ttl = NULL, vl = NULL, plh = NULL){
  textInput(ns(id), ttl, value = vl, placeholder = plh, width = "100%")
}

txt_area_1line <- function(ns, id, ttl = NULL, vl = NULL, plh = NULL){
  textAreaInput(ns(id), ttl, value = vl, placeholder = plh, width = "100%", rows = 1)
}

txt_subline <- function(ns, id){
  #p("ACWQVREQRV", style = "font-size: 14px; margin: -13px 0px 15px 15px;font-style: italic; color: #DCDCDC;")
  uiOutput(ns(id))
  #p(textOutput(ns(id)), style = "font-size: 14px; margin: -13px 0px 15px 15px;font-style: italic; color: #DCDCDC;")
}
DT_area <- function(ns, id){
  DT::DTOutput(outputId = ns(id), width = "100%")
}

make_subline <- function(txt){
  p(txt, style = "font-size: 14px; margin: -13px 0px 15px 15px;font-style: italic; color: #DCDCDC;")
}


txt_area_3line <- function(ns, id, ttl = NULL, vl = NULL, plh = NULL){
  textAreaInput(ns(id), ttl, value = vl, placeholder = plh, width = "60%", rows = 3)
}


txt_area_6line <- function(ns, id, ttl = NULL, vl = NULL, plh = NULL){
  textAreaInput(ns(id), ttl, value = vl, placeholder = plh, width = "100%", rows = 6)
}


normal_picker <- function(ns, id, data, ttl = NULL){
  pickerInput(inputId =ns(id), label = ttl,
                     choices = data,
         )
}

dir_button <- function(ns, id){
  shinyDirButton(id = ns(id), label = "Choose Directory", title = "Select Project Folder", viewtype = "list")
}

image_upload <- function(ns, id){
  fileInput(inputId = ns(id), label = "Select Profile Picture", multiple = FALSE, accept = c('.png', '.jpg'), placeholder = "Select png or jpg image")
}


slider_100 <- function(ns, id, label){
  sliderTextInput(
    inputId = ns(id),
    label = label,
    choices = seq(1, 100),
    grid = TRUE
  )
}


color_picker <- function(ns, id, label, value = "#000000"){
  
  colourpicker::colourInput(
    inputId = ns(id),
    label = label,
    value = value,
    showColour = "both",
    palette = "square",
    allowTransparent = FALSE,
    closeOnClick = TRUE
  )
}

switch_lbl <- function(ns, id, onLabel, offLabel ){
  
  switchInput(
    inputId = ns(id),
    onLabel = onLabel,
    offLabel = offLabel,
    value = TRUE,
   # labelWidth = "80px"
    
  )
}

# Creates and Renders line with buttons
btn_line <- function(x, ns, params){

  wth <- 12 / length(x)
  elem<-fluidRow(
    purrr::map(x, ~
                 shiny::column(width = wth,
                               actionButton(ns(.x), 
                                            params$ui_elems[[.x]][['txt']],
                                            icon(params$ui_elems[[.x]][['icn']]),
                                            width="100%",
                                            class = params$ui_elems[[.x]][['cls']])
                 )
    )
  )

  return(elem)
  
}

# UI FUNCTIONS: Rendering UI Components-------------------

# Creates a Text element from list // if local is delivered as text renders this ow from params
text_ui <- function(x, params,  local = NULL){
  
  if (is.character(local)){
    fnc <- params$ui_elems[[x]][['fnc']]
    txt<- local
    stl<- params$ui_elems[[x]][['stl']]
    elem <- fnc(txt, style = stl)
    
  } else {
    fnc <- params$ui_elems[[x]][['fnc']]
    txt<- params$ui_elems[[x]][['txt']]
    stl<- params$ui_elems[[x]][['stl']]
    elem <- fnc(txt, style = stl)
  }
  return(elem)
}

# Creates and Renders page with Big Status
user_sts_page<-function(x, ns, params){
  
  col <- params$ui_elems[[x]][['col']]
  icn<- params$ui_elems[[x]][['icn']]
  
  stl<-paste0("font-size:150px; margin-top:-10px; margin-bottom:-30px; color:", col, "; text-align: center;")
  
  elem<-tagList(
    tags$br(),
    fluidRow(icon(icn, lib = "font-awesome"), style=stl),
    tags$br()
  )
  
  return(elem)
}



# UI FUNCTIONS: Updating & Toggling the UI-------------------

# shows only vec rows
show_ui <- function(vec, params){
  
  map(params$ui_elems$ui_rows, ~shinyjs::hide(.x))
  map(vec, ~shinyjs::show(.x))
}

# hides only vec rows
hide_ui <- function(vec, params){
  map(params$ui_elems$ui_rows, ~shinyjs::show(.x))
  map(vec, ~shinyjs::hide(.x))
}


# hides / shows ui elements based on a T/F trigger of a MS
trigger_hide_show <- function(trigger, to_toggle){
  
  
  if (isFALSE(trigger)){
    
    map(to_toggle, ~shinyjs::hide(.x))
    return(to_toggle)
    
  } else if (isTRUE(trigger)){
    map(to_toggle, ~shinyjs::show(.x))
    return(NULL)
  } else {
    #do nothing
    return(NULL)
  }
  
}


# SET UI STATUS -------------------

# Updates and Sets the Start status page
set_ui_start <- function(session, params, txt = NULL){
  
  show_ui(c("btrw_start", "start_ui"), params)
  toptxt <- text_ui(x = 'txt_start', params = params, local = txt)
  return(toptxt)
}



# Updates and Sets the Start status page
set_ui_ready <- function(session, params, txt = NULL){

  show_ui(c("btrw_ready", "ui_form"), params)
  shinyjs::enable("btn_confirm")
  shinyjs::enable("btn_go")
  shinyjs::enable("btn_print")
  toptxt <- text_ui(x = 'txt_ready', params = params, local = txt)
  return(toptxt)
}

# Updates and Sets the Start status page
set_ui_negative <- function(session, params, txt = NULL){
  
  show_ui(c("btrw_ready", "ui_form", "negative_ui"), params)
  shinyjs::disable("btn_confirm")
  shinyjs::disable("btn_go")
  shinyjs::disable("btn_print")
  
  toptxt <- text_ui('txt_negative', params, local = txt)
  return(toptxt)
}


# Updates and Sets the Success status page
set_ui_success <- function(session, params, txt = NULL){
  
  # shows the success ui face and the buttons to go next
  show_ui(c("btrw_next",  "success_ui"), params)
  
  # renders the text.
  toptxt <- text_ui(x = 'txt_success', params = params)
  return(toptxt)
}


tab_header <- function(ttl, txt, icn){

  tagList(tags$div(class = "ms_title",
                   tags$br(),
                   tags$br(),
                   fluidRow(
                     column(1, align="left", offset = 0, img(src = icn)),
                     column(11, align="left", offset = 0,
                            fluidRow(div(class = "ui header",  h1(ttl),
                                         div(class = "sub header", h3(txt)))))
                   ),
                   tags$br(),
                   tags$br()
  )
  )
}



#### CHECKER FOR USER INPUTS -------------------------------

# This checks live the correct inputs in ui forms of a Microservice // User recursive testing and finds the first wrong
# checks if is Truthy and if is expectation, ow returns F
check_input<- function(value, func){
  if (check_value(value)){
    if (is.list(func)){
      return(value %in% unlist(func))
    } else {
      return(ods_evaluate(func = func, x = value))
    }
  } else {
    return(FALSE)
  }
}

# recursively calls the checker until finds forst wrong
recursion_checker <- function(input_vec, check_list){
  
  if (length(input_vec) == 0){return(TRUE)}
  nm <- input_vec[1] %>% names()
  res <- check_input(value = input_vec[[nm]], func = check_list[[nm]][["fnc"]])
  # stopping condition of recursion
  if (res){
    # calls itself again // input is ok
    recursion_checker(input_vec[-1], check_list)
    
  } else {
    # stops and returns // found the first wrong input
    return(nm)
  }
}

# Handlers the MS Testing / iniciates recursive + handles consequences
Input_Checker <- function(input_vec, check_list, params){
  
  check_list <- purrr::keep(check_list, check_value)
  input_vec <- input_vec %>% keep_by_name(names(check_list))
  result <- recursion_checker(input_vec, check_list)
  
  # Consequences // toggle ok button + render text
  if (isTRUE(result)){
    #toptxt <- set_ui_ready(session, params, txt = NULL)
    shinyjs::enable("btn_confirm")
    shinyjs::enable("btn_go")
    shinyjs::enable("btn_printcv")
    shinyjs::enable("btn_analyze")
    
    
    toptxt <- text_ui(x = 'txt_success', params = params, "All data are ok. You can go ahead!")
    
  } else {
    # here I dont need to set the entire ui each time, just the text is ok
    shinyjs::disable("btn_confirm")
    shinyjs::disable("btn_go")
    shinyjs::disable("btn_printcv")
    shinyjs::disable("btn_analyze")
    toptxt <- check_list[[result]][["txt"]] %>% 
      text_ui('txt_negative', params, local = .)
  }
  return(toptxt)
}




## DATA CHECKERS ------------------------------------------------------------------
#' FP: Condition Checker
#'
#' Takes a function in Text format, along with 1 or 2 arguments (\code{x} and \code{y}). It first parses the function, then evaluate it.
#' It returns \code{TRUE} or \code{FALSE}. This function is mainly used in functional programming to evaluate if certain conditions are met (e.g. check user inputs).
#'
#' @param func any function that takes x and y (optional) as input
#' @param x the first input. Can be any format
#' @param y Optional. the second input. Can be any format. default \code{NULL}
#' @return \code{TRUE} or \code{FALSE} If outcome is not \code{truthy}, returns \code{FALSE}
#' @export
#' @importFrom shiny isTruthy
#' @examples
#' ods_evaluate(func = "nchar(x) > 3", x = "alooooooongword")
ods_evaluate <- function(func, x, y=NULL){
  eval(parse(text = paste('f<-function(x, y){return(',func,')}', sep='')))
  return(isTruthy(try(f(x, y), silent = T)))
}


# parses time format // guesses it
parse_time_format <- function(x){
  possible_formats <- lubridate::guess_formats(x, "dmy")
  y <- lubridate::parse_date_time(x, orders = possible_formats) %>% as.POSIXct()
  return(y)
}


#' DATA CHECK: First Valid Value
#'
#' This RECURSIVE function takes a vector of candidate arguments, of which only some might be valid, and returns the first which is valid. The priority is simply the order of the vector.
#' This is useful if you have, e.g. different limits from different sources, like (1. max_field, max_user, max_general) and you want to take the first which is valid (i.e. max_field, but this may not exist, hence takes max_user).
#'
#' @param vec a vector of arguments (valid or not) sorted in priority order.
#' @param ... is used to use with pmap()
#' @return \code{vec[x]} here x is the first which has a valid entry. Type depends on the vector type.
#' @export
#' @examples
#' first_valid(c(NA, NULL, 33, 66))
first_valid <- function(vec, ...){
  x <- vec[[1]]
  if (!check_value(x) & length(vec) >0){
    vec[-1] %>% first_valid()
  } else {
    return(x)
  }
}


#' DATA CHECK: Checks validity of a value
#'
#' Custom made value checker for any function input value. Returns \code{FALSE} if \code{x} is any useless value.
#' Opposed to conventional checkers, it also returns \code{FALSE} if values like \code{"NULL"} or \code{"NA"} come as char.
#'
#' @param x value to check. Can be any format
#' @return \code{TRUE} or \code{FALSE}
#' @export
#' @importFrom shiny isTruthy
#' @examples
#' # To understand, run this vector. Entries 1-7 result in \code{FALSE},
#' # while entries 8-14 result in \code{TRUE}
#' vec <- list(tryerr = try(2 > NULL), emtpy = "", Null = NULL, na = NA, NULLchar = "NULL", NAchar = "NA", none = "none", cha = "absjkf", True = TRUE, False = FALSE,  num = 33, lst = list(a=33), vc = c(33, 66), daf = data.frame(a = c(1,2,3), b = c(5,6,7)))
#' purrr::map(vec, ~check_value(.x))
check_value <- function(x){
  return((isTruthy(x) | isFALSE(x)) & isFALSE(all(x %in% c("NA", "none", "NULL"))))
}

#' DATA CHECK: Secure If condition
#'
#' Many errors happen because of invalid test conditions inside an \code{if ()} clause. This can happen, for example, of the value to be checked is NULL or NA or the variable simply doesn't exist.
#' This helper solve this problem by securely checking a condition. It expects a value \code{x} and a argument \code{condition}, against which it checks (can be single val or vector) and it returns \code{c(TRUE,FALSE)}.
#' The argument \code{condition} can also be a function, such as \code{c(is.numeric, is.character, is.na)}
#' In case anything unexpected happens, it returns \code{FALSE}.
#'
#' @param x value to check. Can be any format
#' @param regex logical, if Regex should be used. Default \code{FALSE}
#' @param condition the condition against which x is compared. Can be vector or single val. It uses the code \code{any(x == condition)} hence gives true if x corresponds to any of the entry. Can also be a function, such as \code{c(is.numeric, is.character, is.na)}
#' @return \code{TRUE} or \code{FALSE}
#' @importFrom magrittr %>%
#' @export
#' @examples
#' check_if(33, 55)
#' check_if(33, 33)
#' check_if(33, is.numeric)
#' check_if(33, c(55, 33, 44))
#' check_if(NULL, 55)
#' check_if(NA, c(55, 33, 44))
#' check_if(33, "55")
#' check_if("foo", "foo")
#' check_if("foo", c("foo", "bar", "other"))
#' check_if("foo", c("fo", "bar", "other"))
#' check_if(33, NA)
check_if <- function(x, condition, regex = FALSE){
  tryCatch({
    if (check_value(x)){
      if (is.function(condition)){
        condition(x)
      } else {
        if (isTRUE(regex)){
          any(grepl(x, condition)) %>% isTRUE
        } else {
          any(x == condition) %>% isTRUE
        }
      }
    }
    else {
      FALSE
    }
  }, error = function(e) {
    FALSE
  })
}


#' DATA CHECK: \code{NULL} or Value
#'
#' Checks an entry using check_value. If is a useful value, returns the same value,
#' otherwise returns a proper \code{NULL}.
#'
#' @param x value to check. Can be any format
#' @return \code{x} or \code{NULL}
#' @export
#' @examples
#' null_or_value(x = NA)
null_or_value <- function(x){
  if (check_value(x)){
    return(x)
  } else {
    return(NULL)
  }
}

#' DATA CHECK: \code{NA} or Value
#'
#' Checks an entry using check_value. If is a useful value, returns the same value,
#' otherwise returns a proper \code{NA}.
#'
#' @param x value to check. Can be any format
#' @return \code{x} or \code{NA}
#' @export
#' @examples
#' na_or_value(x = 34)
na_or_value <- function(x){
  if (check_value(x)){
    return(x)
  } else {
    return(NA)
  }
}

#' DATA CHECK: \code{FALSE} or Value
#'
#' Checks an entry using check_value. If is a useful value, returns the same value,
#' otherwise returns a proper \code{FALSE}.
#'
#' @param x value to check. Can be any format
#' @return \code{x} or \code{FALSE}
#' @export
#' @examples
#' false_or_value(x = NULL)
false_or_value <- function(x){
  if (check_value(x)){
    return(x)
  } else {
    return(FALSE)
  }
}

#' DATA CHECK: \code{TRUE} or Value
#'
#' Checks an entry using check_value. If is a useful value, returns the same value,
#' otherwise returns a proper \code{TRUE}.
#'
#' @param x value to check. Can be any format
#' @return \code{x} or \code{TRUE}
#' @export
#' @examples
#' true_or_value(x = "nice name")
true_or_value <- function(x){
  if (check_value(x)){
    return(x)
  } else {
    return(TRUE)
  }
}

#' DATA CHECK: Text or Value
#'
#' Checks an entry using check_value. If is a useful value, returns the same value,
#' otherwise returns a proper Text value, which by default is \code{""}. Often used to
#' deal with missing values in rendering.
#'
#' @param x value to check. Can be any format.
#' @param rep Text value to return in case of \code{FALSE}, by default \code{""}.
#' @return \code{x} or \code{""} or \code{"any text"}.
#' @export
#' @examples
#' empty_or_value(x = NA, rep = "missing value")
empty_or_value <- function(x, rep = ""){
  if (check_value(x)){
    return(x)
  } else {
    return(rep)
  }
}

#' DATA CHECK: \code{0} or Value
#'
#' Checks an entry using check_value and \code{is.na(as.numeric(x))}.
#' If is either a numeric value or a char which can be turned into numeric returns it.
#' otherwise returns a proper numeric \code{0}.
#'
#' @param x a supposedly numeric value to check.
#' @return \code{x} or \code{0} always in numeric format.
#' @export
#' @examples
#' zero_or_numeric(x = 34.5)
#' zero_or_numeric(x = "34.5")
#' zero_or_numeric(x = "abc")
zero_or_numeric <- function(x){
  if (check_value(x)){
    if (suppressWarnings(!is.na(as.numeric(as.character(x))))){
      return(as.numeric(x))
    } else {
      return(0)
    }
  } else {
    return(0)
  }
}

#' DATA CHECK: Validates Email Format
#'
#' Checks is a string follows the basic structure of an email address. Doesn't check if
#' the email is valid, i.e. if it works.
#'
#' @param x email string to be checked
#' @return \code{TRUE} or \code{FALSE}
#' @export
#' @examples
#' is_valid_email("ali.baba@sesam.com")
is_valid_email <- function(x) {
  X<-grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", as.character(x), ignore.case=TRUE)
  return(isTRUE(X))
}

## FUNCTION: DATA MANIPULATORS ------------------------------------------------------------------

#' WRANGLING: Dataframe to named List
#'
#' Takes a \code{dataframe} and turns it into a named \code{list}. The colnames which go into the values and the
#' names respectively of the nested list are specified. If params is supplied, translates the names of the list.
#'
#' @param df any \code{dataframe} with at least one column
#' @param vals column name of the column which goes to the list values
#' @param nms ecolumn name of the column which goes to the list names
#' @param params the ODAPES parameters list (needed to translate)
#' @return a named \code{list}
#' @importFrom stats setNames
#' @export
df_to_lst <- function(df,  vals, nms, params = NULL){
  if (is.null(params)){
    setNames(as.list(df[[vals]]), df[[nms]])
  } else {
    setNames(as.list(df[[vals]]), params$text$t(df[[nms]]))
  }
}

#' WRANGLING: Unlist and check
#'
#' Takes vector of possibly listed values (i.e. Df column) and turns it into a unlisted vector.
#' not useful values are turns into a proper NA.Also unnames vector. Is often used in combination with \code{pmap} in \code{data.frames}.
#'
#' @param VALUE any list or nested list or named vector or simple vector.
#' @param ... any additional arguments. is needed to enable use in data frames in combination with pmap
#' @return a simple Vector
#' @importFrom purrr map
#' @export
unlist_vals <- function(VALUE, ...){
  if (check_value(VALUE)) {
    VALUE %>% unlist(use.names = F)  %>% purrr::map(na_or_value) %>% unlist(use.names = F)
  } else {
    NA
  }
}

#' WRANGLING: Keep by name
#'
#' Takes a named list and a vector of names, which are kept. The rest of the list elements are discarded.
#' Also discards any entry whose value is NULL.
#'
#' @param l a named list
#' @param keep_names vector with names to keep
#' @param regex should regex be used in the form of \code{grepl(keep_names)} to find names? Default \code{FALSE}
#' @return a named list
#' @importFrom purrr keep
#' @importFrom magrittr %>%
#' @export
keep_by_name <- function(l, keep_names, regex = FALSE) {
  if (regex) {
    return(l[grepl(keep_names,  names(l))])
  } else {
    return(l[keep_names] %>% purrr::keep( ~ !is.null(.)))
  }
}

#' WRANGLING: Discard by name
#'
#' Takes a named list and a vector of names, which are discarded The rest of the list elements are kept.
#'
#' @param l a named list
#' @param discard_names vector with names to discard
#' @param regex should regex be used in the form of \code{grepl(discard_names)} to find names? Default \code{FALSE}
#' @return a named list
#' @importFrom magrittr %>% is_in not extract
#' @export
discard_by_name <- function(l, discard_names, regex = FALSE) {
  if (regex) {
    return(grepl(discard_names, names(l)) %>% not() %>% magrittr::extract(l, .))
  } else {
    return(names(l) %>% is_in(discard_names) %>% not() %>% extract(l, .))
  }
  
}

#' WRANGLING: Replace List \code{NULL} entries
#'
#' Takes a list and replaces any \code{NULL} entry with a \code{NA} or a desired replacement value.
#'
#' @param l a named list
#' @param by a value to replace \code{NULL}. By defauls \code{NA}
#' @return a list
#' @importFrom magrittr %>%
#' @export
replace_null_vls <- function(l, by = NA){
  l %>% lapply(function(x){if(is.null(x)){by}else {x}})
}

#' WRANGLING: Detect which entry changed
#'
#' Takes 2 vectors or lists, an old and a new, and detect, which elements changed. Is used to compare lists of user input values.
#'
#' @param new vector or list
#' @param old vector or list
#' @return a list the length of the \code{new} with \code{TRUE} or  \code{FALSE} depending if changed. returns \code{NULL}
#' if nothing changed and \code{new} if there is no \code{old}
#' @importFrom purrr map_lgl
#' @importFrom magrittr %>%
#' @export
which_changed_2 <- function(new, old){
  if (length(new) == 0) {return(NULL)}
  if (length(old) == 0) {return(names(new))}
  if (identical(old, new)){return(NULL)}
  map_lgl(names(new), ~!identical(new[.x], old[.x])) %>% names(new)[.]
}

#' WRANGLING: Recursive Reducer of List Depth
#'
#' Takes a potentially nested list and reduces its depth, one level at a time, until the depth is only \code{d}. This is used to
#' preserve formats (as opposed to unlist(), which crashes formats). It applies itself recursively until the depth d is reached.
#'
#' @param x vector or list, potentially nested.
#' @param d target level of depth. Defaults = 1
#' @return a list of depth d
#' @importFrom purrr pluck_depth reduce
#' @importFrom magrittr %>%
#' @export
recursive_reducer <- function(x, d = 1){
  if (purrr::pluck_depth(x) > d){
    x %>% purrr::reduce(c) %>% recursive_reducer(d)
  } else {
    return(x)
  }
}

#' WRANGLING: Recursive Dataframe Joiner
#'
#' Often, we have multiple similar Dataframes in a list, for example as a result of a map() function. You want to join these data into one dataframe, where the mutual columns are joined and the different columns are added as new columns.
#' This function takes the list of DF's of similar (not equal) strucutre and joins it to a single Dataframe. It goes recursively through each list element and full_joins it to the former Dataframe.
#' It is often used after a map function.
#'
#' @param lst a list of Dataframes of similar structure. E.g. each DF has the column \code{element_id}, but each has different variable columns like \code{C21A}, \code{C11A}, \code{C31A}.
#' @return a single Dataframe with all columns which appear in all dataframes across the list
#' @importFrom dplyr full_join
#' @importFrom magrittr %>%
#' @export
recursive_joiner <- function(lst){
  
  X <- lst[[1]]
  lst <- lst[-1] # here slice off, like a salami
  if (length(lst) == 0){return(X)}
  lst[[1]] <- X %>% full_join(lst[[1]])
  recursive_joiner(lst) # call recursive
}

#' FILES: Checks if a File exists
#'
#' Takes a file name and a path (optional) and checks if the path file exists. It uses here, i.e. it joins \code{here()} and the
#' path. Hence, it always starts at the project base directory.
#'
#' @param x file name incl extension
#' @param path path or sub-folder. Defaults \code{NULL}
#' @param ... args to use in pmap
#' @return \code{TRUE} or \code{FALSE}
#' @import here
#' @importFrom magrittr %>%
#' @export
check_file <- function(x, path = NULL, ...){
  ifelse(is.null(path),here(x),here(path, x)) %>% file.exists()
}


#' FILES: Gets the file Extension
#'
#' Takes any filename and returns the extension. Leading point can be included or not.
#'
#' @param file file name incl extension
#' @param include_point should the leading point be included. Default \code{TRUE}
#' @param ... args to use in pmap
#' @return char representing the file extension
#' @importFrom magrittr %>%
#' @export
getExtension <- function(file, include_point = TRUE, ...){
  if (isTRUE(include_point)){
    sub(".*\\.", "", file) %>% paste0(".", .)
  } else {
    sub(".*\\.", "", file)
  }
}



#' FORMAT: Formats Values including unit
#'
#' This function formats any type of value for printing. This includes units, rounding, significant digits and dealing with missing values. Each unit comes with a specified number formatting Attention, the units are limited. We extend over time.
#'
#' @param x the value to be formatted. numeric or char. Can be a vector.
#' @param format The unit to be formatted to. Currently supported are US$, R$, ˚C, mm, \%. Default is US$".
#' @param narep a char value to replace NAs. By default \code{""}
#' @param dgt optional. number of significant digits. Default 2.
#' @return a formatted char including unit
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate filter pull
#' @export
#' @examples
#' ods_format(c(1.2223445, 2.34223344, 4.4444444, 5) )
#' ods_format(c(1.2223445, 2.34223344, NA,4.4444444, 5), format = "mm", narep = "missing")
ods_format <- function(x, format = "US$", narep = "", dgt = 2){
  frmt<-function(x, format, inv = F, rnd = 1, dgt = 2){
    y <- x%>% round(digits = rnd)%>%
      format(nsmall = dgt,justify = "right", scientific = FALSE ,big.mark   = "'", big.interval = 3, decimal.mark = ",")
    z <- dplyr::case_when(inv ~ paste(format, y), TRUE ~paste(y, format))
  }
  get_dir <- function(x, format){
    clnm<- gsub("dir", "", format)
    directions %>% dplyr::mutate(match = (x >= min_rad & x <= max_rad)) %>% dplyr::filter(match) %>% dplyr::pull(clnm)
  }
  
  y<- switch(format,
             
             "US$" = {ifelse(is.na(x), narep ,frmt(x, format, inv = TRUE))},
             
             "R$" = {ifelse(is.na(x), narep ,frmt(x, format,inv = TRUE))},
             
             "˚C" = {ifelse(is.na(x), narep ,frmt(x, format, rnd = 2))},
             
             "mm" = {ifelse(is.na(x), narep,frmt(x, format, dgt = 1))},
             
             "m" = {ifelse(is.na(x), narep,frmt(x, format, dgt = 1))},
             
             "HA" = {ifelse(is.na(x), narep,frmt(x, format, dgt = 2))},
             
             "%" = {ifelse(is.na(x), narep,frmt((x*100), format, dgt = 0))},
             
             "dirPT" = {ifelse(is.na(x), narep, get_dir(x, format))},
             
             "unitless" = {ifelse(is.na(x), narep,frmt(x, format = "", dgt = dgt))},
             
             {ifelse(is.na(x), narep,frmt(x, format = "", dgt = dgt))}
  )
  return(y)
}


#' DB: Finds a certain column in a DF (id, name, lat etc..)
#'
#' ODAPES works with many dataframes, which do typically have id columns (from the DB). This function identifies the ID column
#' and returns its name.
#'
#' @param X a dataframe comming from ODS Database
#' @param what what ending of the colname should be looked for. Default is \code{"id"}, typical are things like \code{c("id","name","lng", "lat")}
#' @param n How many id columns does the dataframe have. Defaults is \code{1}
#' @param mode how to find the \code{what} keyword? Can be any of \code{c("end","start","all","regex")}. DEfault \code{"end"}. all refers to start and end.
#' @return a char with the column
#' @importFrom dplyr select ends_with
#' @importFrom magrittr %>% extract
#' @export
find_column <- function(X, what = "id", n = 1, mode = "end"){
  y <- switch(mode,
              
              "end" = {X %>% dplyr::select(ends_with(what)) %>% colnames()},
              
              "start" = {X %>% dplyr::select(starts_with(what)) %>% colnames()},
              
              "all" = {X %>% dplyr::select(matches(paste0(what, "_", "|", "_", what))) %>% colnames()},
              
              "regex" = {X %>% dplyr::select(matches(what)) %>% colnames()}
  )
  
  if (length(y) <= n){
    y %>% magrittr::extract(n)
  } else {
    y %>% magrittr::extract(1)
  }
}


#' REGEX: Sets string to standard string
#'
#' ODAPES deals often with Latin texts and different special characters. For Regex purposes this is difficult. This function sets
#' any text to standard (no Latin accents), lower case and no special char text. It's used to prepare for Regex.
#'
#' @param str any string or vector of strings.
#' @return the same string or vector of string, just standardized.
#' @importFrom magrittr %>%
#' @export
standard_string <- function(str){
  iconv(str,from="UTF-8",to="ASCII//TRANSLIT") %>% gsub("[[:punct:]]", "", .) %>% tolower()
}

#' REGEX: Finds patter in string vectors
#'
#' This function tries to find a string pattern, \code{x}, inside 1 to 3 candidate vectors of strings (e.g. 3 different languages).
#' All strings are first standardized before searched. t1, t2 and t3 must be of the same length.
#'
#' @param x string pattern to look for.
#' @param t1 candidate string vector, in which we search
#' @param t2 optional candidate string vector, in which we search, Default \code{NA}
#' @param t3 optional candidate string vector, in which we search, Default \code{NA}
#' @return Vector of the length of \code{t1} with \code{TRUE} or \code{FALSE} indicating where the pattern appears.
#' @export
find_string_pattern <- function(x, t1, t2=NA, t3=NA ){
  # aqui sergio va trabajar
  grepl(standard_string(x), standard_string(t1)) | grepl(standard_string(x), standard_string(t2)) | grepl(standard_string(x), standard_string(t3))
}

# ## FUNCTION: BINARY + HEX HELPERS ------------------------------------------------------------------

#' DATA: Converts Integer to Binary
#'
#' Takes \code{integer} and turns it into \code{Binary String}. Is recursive.
#'
#' @param x \code{integer}
#' @return \code{Binary String}
#' @export
int_to_bin <- function(x) {
  bin <- ''
  if(x > 1) {
    bin <- int_to_bin(as.integer(x/2))
  }
  bin <- paste0(bin, x %% 2)
  return((bin))
}

#' DATA: Converts an Binary to Integer
#'
#' Takes \code{Binary String} and turns it into \code{integer}.
#'
#' @param x \code{Binary String}
#' @return \code{integer}
#' @export
bin_to_int <- function(x){
  strtoi(x, base = 2)
}

#' DATA: Converts Hex to Integer
#'
#' Takes \code{Hex String} and turns it into \code{integer}.
#'
#' @param x \code{Hex String}
#' @return \code{integer}
#' @export
hex_to_int <- function(x){
  strtoi(x, base = 16)
}

#' DATA: Converts Integer to Hex
#'
#' Takes \code{integer} and turns it into \code{Hex String}.
#'
#' @param x \code{integer}
#' @return \code{Hex String}
#' @importFrom magrittr %>%
#' @export
int_to_hex <- function(x){
  as.hexmode(x) %>% as.character()
}

#' DATA: Converts Binary to Hex
#'
#' Takes \code{Binary String} and turns it into \code{Hex String}. Goes via integer.
#'
#' @param x \code{Binary String}
#' @return \code{Hex String}
#' @importFrom magrittr %>%
#' @export
bin_to_hex <- function(x){
  x %>% bin_to_int() %>% int_to_hex()
}

#' DATA: Converts Hex to Binary
#'
#' Takes \code{Hex String} and turns it into \code{Binary String}. Goes via integer.
#'
#' @param x \code{Hex String}
#' @return \code{Binary String}
#' @importFrom magrittr %>%
#' @export
hex_to_bin <- function(x){
  x %>% hex_to_int() %>% int_to_bin()
}

#' DATA: Random Hex String (length n)
#'
#' Creates a random \code{Hex String} of lenght n.
#'
#' @param n integer. length of string. Default = 10
#' @return \code{Hex String}
#' @importFrom magrittr %>%
#' @export
random_hex_n <- function(n = 10){
  sample(0:15, n, replace = T) %>% as.hexmode() %>% paste(collapse = "")
}

#' DATA: Complete leading Zeros for Binary schedule representation
#'
#' Helper for the scheduling encoding. it completes the leading zeros for a binary codes up to the length needed. for example,
#' if we have 1001, this weekday code would be Wed + Sat. hence it should be 0001001 (make clear Sun-Tues is also 0).
#'
#' @param bin a binary code representing a schedule
#' @param n how long the bin code should be, i.e. 7 for week and 24 for hours. Defauls =7
#' @return \code{Binary String} of length n
#' @importFrom magrittr %>%
#' @export
complete_zeros <- function(bin, n = 7){
  max(n - nchar(bin), 0) %>%
    rep("0", .) %>%
    paste(collapse = "") %>%
    paste0(bin)
}



### NEW FUNCTONS
valid_hex_color <- function(hex) {
  if (!check_value(hex)){return(FALSE)}
  # Remove optional '#' at the start
  sub("^#", "", hex) %>% grepl("^[0-9A-Fa-f]{6}$", .)
}


### DIR RELATED HELPERS --------------------------------


# creates the output dir
create_dir <- function(path, name){
  dir.create(file.path(path, name))
  return(dir.exists(file.path(path, name)))
}

# obtains the UI input of project directory and parses the path
parse_dir <- function(folder){
  fld <-  unlist(folder$path) %>% paste(collapse = "/")
  path <- path.expand("~") %>% paste0(fld)
  return(path)
}

# copies all files from one source to a path
copy_all_files <- function(source_path, destination_path){
  files <- list.files(source_path, full.names = TRUE, include.dirs = FALSE)
  # Copy each file to the destination directory
  for (file in files) {
    file.copy(from = file, to = destination_path, overwrite = TRUE)
  }
}


# copies the cv to the output
copy_file <- function(source_path, destination_path, file_name, dest_name = NULL){
  
  source_file <- file.path(source_path, file_name)
  
  if (check_value(dest_name)){
    destination_file <- file.path(destination_path, dest_name)
  } else {
    destination_file <- file.path(destination_path, file_name)
  }
  
  # Copy the file to the destination + delete old
  file.copy(from = source_file, to = destination_file, overwrite = TRUE)
  
  return(file.exists(destination_file))
}


# creates a Directory list given a source path (recursive)
# used to copy directory structures // if parentdir_name is not sent then its copies, ow changed
generate_dir_list <- function(source_dir) {
  dir_list <- list()
  
  # Get the subdirectories in the origin_dir
  subdirectories <- list.dirs(source_dir, recursive = FALSE, full.names = FALSE)
  
  # Iterate over subdirectories
  for (subdir in subdirectories) {
    subdir_path <- file.path(source_dir, subdir)
    
    # Check if the subdirectory is empty (i.e., has no subdirectories)
    if (length(list.dirs(subdir_path, recursive = FALSE, full.names = FALSE)) == 0) {
      dir_list[[subdir]] <- list()
    } else {
      dir_list[[subdir]] <- generate_dir_list(subdir_path)
    }
  }
  return(dir_list)
}

# Create a new folder within the destination directory
# // recursive // creates dir hierarchy + copy the files
create_dir_structure <- function(dir_list, parent_dir = path.expand("~"), source_dir = here()) {
  
  for (i in seq_along(dir_list)) {
    current_dir <- file.path(parent_dir, names(dir_list)[i])
    current_source_dir <- file.path(source_dir, names(dir_list)[i])
    if (is.list(dir_list[[i]])) {
      dir.create(current_dir)
      copy_all_files(current_source_dir, current_dir)
      
      create_dir_structure(dir_list[[i]], current_dir, current_source_dir)
    } else {
      dir.create(current_dir)
      copy_all_files(current_source_dir, current_dir)
    }
  }
}

# takes either email or regular name and returns a underscore separated user name
make_user_name <- function(x){
  # Remove special characters and spaces, to lower and max 20 chars
  x %>% gsub("[^a-zA-Z0-9]", "_", .)%>% tolower %>% substr(1, 20) %>% gsub("^_+|_+$", "", .)
}


# savely overwrites a new csv file first copies old to placeholder and only deletes once the new is confirmed
save_csv_overwrite <- function(data, path, filename){
  
  # Output file paths
  original_file_path <- file.path(path, filename)
  old_file_path <- file.path(path, paste0("old_", filename))
  
  # Check if original file exists
  if (file.exists(original_file_path)) {
    # Rename the original file
    file.rename(original_file_path, old_file_path)
  }
  
  # Write data to CSV file
  write.csv(data, original_file_path, row.names = FALSE)
  
  # Confirm that the file has been written
  if (file.exists(original_file_path)) {
    # Remove the renamed file
    file.remove(old_file_path)
    return(TRUE)
    
  } else {
    # If the new file was not successfully written, restore the renamed file
    file.rename(old_file_path, original_file_path)
    return(FALSE)
  }
  
}



# savely overwrites a new csv file first copies old to placeholder and only deletes once the new is confirmed
save_check_csv <- function(data, path, filename){
  
  # Output file paths
  file_path <- file.path(path, filename)
  
  # Check if original file exists
  if (file.exists(file_path)) {
    # Rename the original file
    file.remove(file_path)
  }
  
  # Write data to CSV file
  write.csv(data, file_path, row.names = FALSE)
  
  # Confirm that the file has been written
  if (file.exists(file_path)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
  
}


# Define the function to delete files with specific extensions
delete_files_with_extensions <- function(directory, extensions) {
  # Construct the pattern to match files based on the extensions
  pattern <- paste0(".*\\(", paste(extensions, collapse = "|"), ")$")
  
  # List all files in the directory that match the pattern
  files_to_delete <- list.files(directory, pattern = pattern, full.names = TRUE)
  
  # Delete the files
  deleted_files <- file.remove(files_to_delete)
  
  # Print messages based on deletion success
  if (any(deleted_files)) {
    cat("Deleted files:\n", paste(files_to_delete[deleted_files], collapse = "\n"), "\n")
  }
  if (any(!deleted_files)) {
    cat("Failed to delete files:\n", paste(files_to_delete[!deleted_files], collapse = "\n"), "\n")
  }
  if (length(files_to_delete) == 0) {
    cat("No files found with the specified extensions.\n")
  }
}



# Cleans the dir form temporary files
clean_directory <- function(directory, extensions) {
  pattern <- paste0(".*(", paste(extensions, collapse = "|"), ")$")
  files_to_delete <- list.files(directory, pattern = pattern, full.names = TRUE)
  files_to_delete <- files_to_delete[!grepl("header", basename(files_to_delete))]
  
  deleted_files <- file.remove(files_to_delete)
}
