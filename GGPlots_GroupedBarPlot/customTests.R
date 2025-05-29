# GGplot grouped bar plots

script_results_identical <- function(result_name) {
  # Get e
  e <- get('e', parent.frame())
  
  # First, try to source the user's script to see if it runs without error
  user_script_success <- tryCatch({
    source(e$script_temp_path, local = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # If user's script failed, return FALSE immediately
  if (!user_script_success) {
    return(FALSE)
  }
  
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
  result <- all.equal(user_res, correct_res)
  return(isTRUE(result))
}

plot_results_identical <- function(result_name) {
  e <- get("e", parent.frame())

  ## start with a clean slate
  if (exists(result_name, .GlobalEnv))
    rm(list = result_name, envir = .GlobalEnv)

  ## try to run the student’s file
  ok <- tryCatch({
    source(e$script_temp_path, local = TRUE)
    TRUE
  }, error = function(err) FALSE)
  if (!ok || !exists(result_name, .GlobalEnv)) return(FALSE)
  user_res <- get(result_name, .GlobalEnv)

  ## build the reference object
  ref_env <- new.env()
  capture.output(source(e$correct_script_temp_path, local = TRUE,
                        envir = ref_env))
  correct_res <- get(result_name, ref_env)

  ## helper that removes attributes & orders rows
  normalise <- function(df) {
    df <- as.data.frame(df)
    df <- df[order(names(df)[1], seq_len(nrow(df))), , drop = FALSE]
    attributes(df) <- attributes(df)[c("names", "class")]
    df
  }

  ## compare every layer, ignoring attributes and row order
  same <- tryCatch({
    u <- lapply(ggplot_build(user_res)$data,   normalise)
    c <- lapply(ggplot_build(correct_res)$data, normalise)

    length(u) == length(c) &&
      all(vapply(seq_along(u),
                 \(i) isTRUE(all.equal(u[[i]], c[[i]],
                                       check.attributes = FALSE)),
                 logical(1)))
  }, error = function(err) FALSE)

  same
}

plot_results_identical2 <- function(result_name) {
  # Get e
  e <- get('e', parent.frame())
  
  # Clear any existing result from global environment first
  if(exists(result_name, globalenv())) {
    rm(list = result_name, envir = globalenv())
  }
  
  # First, try to source the user's script to see if it runs without error
  user_script_success <- tryCatch({
    source(e$script_temp_path, local = TRUE)
    TRUE
  }, error = function(e) {
    FALSE
  })
  
  # If user's script failed, return FALSE immediately
  if (!user_script_success) {
    return(FALSE)
  }
  
  # Get user's result from global (should be newly created)
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
  
  # For ggplot objects, compare the built plots
  result <- tryCatch({
    user_built <- ggplot_build(user_res)
    correct_built <- ggplot_build(correct_res)
    # Compare the data components
    all.equal(user_built$data, correct_built$data)
  }, error = function(e) {
    # Fallback to basic comparison
    all.equal(user_res, correct_res)
  })
  
  # Return TRUE only if objects are truly equal
  return(isTRUE(result))
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
    pre_fill_link <- "https://docs.google.com/forms/d/e/1FAIpQLScvlcDi_wL3LPrY3c-GFF7lB0TaMS3PnLANHTZJ66Jw4Qs7Hw/viewform?usp=pp_url&entry.996111921"
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