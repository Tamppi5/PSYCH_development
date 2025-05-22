# One Sample T-Tests

script_results_identical <- function(result_name) {
  # Get e (Swirl's environment)
  e <- get('e', parent.frame())

  # 1. Check if Swirl detected an error during the sourcing of the student's script
  #    e$expr_error is typically set to TRUE by Swirl if an error occurred.
  if (!is.null(e$expr_error) && e$expr_error) {
    return(FALSE) # Student's script had a sourcing error, so fail the test.
  }

  # 2. Check if the student's script produced the expected result object in the global environment
  if (!exists(result_name, globalenv())) {
    # Student's script ran (or Swirl didn't flag a fatal error) but didn't create the object.
    return(FALSE)
  }
  user_res <- get(result_name, globalenv())

  # 3. Source the correct script in a temporary environment and get its result
  tempenv <- new.env()
  correct_script_had_error <- FALSE # Flag to track errors in the correct script

  # Capture output from sourcing the correct script to keep the console clean
  capture.output(
    local(
      tryCatch({
        # source the correct script
        source(e$correct_script_temp_path, local = TRUE, chdir = TRUE) # chdir=TRUE is good practice
      }, error = function(cond) {
        # An error occurred while sourcing the correct script. This is an authoring issue.
        correct_script_had_error <<- TRUE
        # For author debugging: message(paste("Error in correct script:", cond$message))
      }),
      envir = tempenv
    )
  )

  # 4. Check if the correct script ran successfully and produced the result object
  if (correct_script_had_error || !exists(result_name, tempenv)) {
    # If the correct script is faulty, we can't validate the student's answer.
    # For safety, treat this as a failure for the student's current attempt.
    # This also helps identify authoring errors.
    # For author debugging: message("Correct script failed to produce the expected result.")
    return(FALSE)
  }
  correct_res <- get(result_name, tempenv)

  # 5. Compare the student's result with the correct result
  return(identical(user_res, correct_res))
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
    
    pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSc6me07p5aQqP-yrIst2okU64WNzW7KlkYf3cPQktS_TuxK8g/viewform?usp=pp_url&entry.996111921"
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