# Independent Samples T-Test
library(base64enc)

script_results_identical <- function(result_name) {
  e <- get('e', parent.frame())

  # --- Evaluate student's script ---
  student_script_path <- e$script_temp_path
  # Create a new environment for the student's script, inheriting from globalenv
  # so it can access data like 'violence' if needed.
  student_env <- new.env(parent = globalenv())
  student_script_error <- FALSE
  
  # Capture console output during sourcing to keep things clean
  capture.output(
    tryCatch({
      # Source student's script in its own environment
      source(student_script_path, local = student_env, chdir = TRUE, encoding = "UTF-8")
    }, error = function(cond) {
      student_script_error <<- TRUE
      #message(paste("DEBUG: Error sourcing student script:", cond$message))
    })
  )

  # 1. Check if student's script had a sourcing error
  if (student_script_error) {
    #message("DEBUG: Student script had a sourcing error.")
    return(FALSE)
  }

  # 2. Check if the student's script produced the expected result object in its environment
  if (!exists(result_name, envir = student_env)) {
    #message(paste("DEBUG: Result '", result_name, "' not found in student_env.", sep=""))
    return(FALSE)
  }
  user_res <- get(result_name, envir = student_env)

  # --- Evaluate correct script ---
  # Ensure e$correct_script_temp_path is valid (Swirl should provide this)
  if (is.null(e$correct_script_temp_path) || !file.exists(e$correct_script_temp_path)) {
    # This would be an authoring or Swirl setup issue.
    #message("DEBUG: e$correct_script_temp_path is invalid or file does not exist.")
    return(FALSE) 
  }
  correct_script_path <- e$correct_script_temp_path
  
  # Create a new environment for the correct script
  tempenv_correct <- new.env(parent = globalenv())
  correct_script_had_error <- FALSE

  capture.output(
    tryCatch({
      source(correct_script_path, local = tempenv_correct, chdir = TRUE, encoding = "UTF-8")
    }, error = function(cond) {
      correct_script_had_error <<- TRUE
      #message(paste("DEBUG: Error sourcing correct script:", cond$message))
    })
  )

  # 3. Check if the correct script ran successfully and produced the result object
  if (correct_script_had_error || !exists(result_name, envir = tempenv_correct)) {
    #message("DEBUG: Correct script failed or didn't produce result.")
    return(FALSE)
  }
  correct_res <- get(result_name, envir = tempenv_correct)

  # 4. Compare the student's result (from student_env) with the correct result
  are_identical <- identical(user_res, correct_res)
  # For author debugging:
   #if (!are_identical) {
     #message("DEBUG: Student result and correct result are not identical.")
     #message("DEBUG: Student result: "); print(str(user_res))
     #message("DEBUG: Correct result: "); print(str(correct_res))
   #}
  return(are_identical)
}


graph_results_identical <- function(result_name) {
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
  all.equal(user_res, correct_res)
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
    pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLSfef0xZ3xWshJVnOYa1rqDq2_9_7GStVZc-qV5XGdl4iOSuFA/viewform?usp=pp_url&entry.996111921"
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