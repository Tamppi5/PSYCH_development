# Testing Non-Normality

script_results_identical <- function(result_name) {
  # Get e
  e <- get('e', parent.frame())
  # Get user's result from global
  if(exists(result_name, globalenv())) {
    user_res <- get(result_name, globalenv())
  } else {
    return(FALSE)
  }
  # Source correct result in new env and get result
  tempenv <- new.env()
  # Capture output to avoid double printing
  temp <- capture.output(
    local(
      try(
        source(e$correct_script_temp_path, local = TRUE),
        silent = TRUE
      ),
      envir = tempenv
    )
  )
  correct_res <- get(result_name, tempenv)
  # Compare results
  identical(user_res, correct_res)
}
plot_results_identical <- function(result_name) {
  e <- get("e", parent.frame())

  student_env <- new.env(parent = .GlobalEnv) 

  # It runs the student's code; if an error occurs, it returns FALSE instead of stopping.
  script_ran_ok <- tryCatch({
    source(e$script_temp_path, local = student_env) 
    TRUE
  }, error = function(err) {
    return(FALSE) # Script failed to run, so it's incorrect.
  })

  if (!script_ran_ok) {
    return(FALSE) 
  }

  if (!exists(result_name, envir = student_env, inherits = FALSE)) {
    return(FALSE) # Script ran but didn't create the expected object.
  }
  
  user_res <- get(result_name, envir = student_env, inherits = FALSE)

  if (!inherits(user_res, "ggplot")) {
    return(FALSE) # The object created isn't a plot.
  }

  correct_env <- new.env(parent = .GlobalEnv)
  source(e$correct_script_temp_path, local = correct_env)
  correct_res <- get(result_name, envir = correct_env, inherits = FALSE)

  # Compare the underlying data of the plots. This is more reliable than comparing the plot objects directly.
  comparison_result <- tryCatch({
    user_built <- ggplot_build(user_res)
    correct_built <- ggplot_build(correct_res)
    isTRUE(all.equal(user_built$data, correct_built$data))
  }, error = function(e) {
    FALSE # An error during comparison means they are not the same.
  })

  return(comparison_result)
}


getState <- function(){
  #Whenever swirl is running, its callback is at the top of its call stack.
  #Swirl's state, named e, is stored in the environment of the callback.
  environment(sys.function(1))$e
}

#Retrieve the log from swirl's state
getLog <- function(){
  getState()$log
}

submit_log <- function(){
  selection <- getState()$val
  if(selection == "Yes"){
    # Please edit the link below
    pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSeQhEsdAyxs5tzPAWzF5WRDAAmTJ7nOHdQujDCeGAYJ3nDQyw/viewform?usp=pp_url&entry.996111921"
    # Do not edit the code below
    if(!grepl("=$", pre_fill_link)){
      pre_fill_link <- paste0(pre_fill_link, "=")
    }
    
    p <- function(x, p, f, l = length(x)){if(l < p){x <- c(x, rep(f, p - l))};x}
    
    temp <- tempfile()
    log_ <- getLog()
    nrow_ <- max(unlist(lapply(log_, length)))
    log_tbl <- data.frame(user = rep(log_$user, nrow_),
                          course_name = rep(log_$course_name, nrow_),
                          lesson_name = rep(log_$lesson_name, nrow_),
                          question_number = p(log_$question_number, nrow_, NA),
                          correct = p(log_$correct, nrow_, NA),
                          attempt = p(log_$attempt, nrow_, NA),
                          skipped = p(log_$skipped, nrow_, NA),
                          datetime = p(log_$datetime, nrow_, NA),
                          stringsAsFactors = FALSE)
    write.csv(log_tbl, file = temp, row.names = FALSE)
    encoded_log <- base64encode(temp)
    browseURL(paste0(pre_fill_link, encoded_log))
  }
}