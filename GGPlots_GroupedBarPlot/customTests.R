# GGplot grouped bar plots


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
  all.equal(user_res, correct_res)
}

plot_results_identical <- function(result_name) {
  e <- get("e", parent.frame())

  student_env <- new.env(parent = .GlobalEnv) 

  script_ran_ok <- tryCatch({
    set.seed(123) # Set seed before running student's script
    source(e$script_temp_path, local = student_env) 
    TRUE
  }, error = function(err) {
    FALSE
  })

  if (!script_ran_ok) {
    return(FALSE) 
  }

  if (exists(result_name, envir = student_env, inherits = FALSE)) {
    user_res <- get(result_name, envir = student_env, inherits = FALSE)
  } else {
    return(FALSE)
  }

  if (!inherits(user_res, "ggplot")) {
    return(FALSE)
  }

  correct_env <- new.env(parent = .GlobalEnv) 

  correct_script_ran_ok <- tryCatch({
    set.seed(123) # Set the SAME seed before running correct script
    source(e$correct_script_temp_path, local = correct_env)
    TRUE
  }, error = function(err) {
    FALSE
  })
  
  if (!correct_script_ran_ok || !exists(result_name, envir = correct_env, inherits = FALSE)) {
    return(FALSE) 
  }
  correct_res <- get(result_name, envir = correct_env, inherits = FALSE)

  if (!inherits(correct_res, "ggplot")) {
    return(FALSE) 
  }

  normalise_df_for_comparison <- function(df) {
    df_norm <- as.data.frame(df)
    if (nrow(df_norm) == 0 || ncol(df_norm) == 0) return(df_norm)

    for (j in seq_along(df_norm)) {
      if (is.factor(df_norm[[j]])) {
        df_norm[[j]] <- as.character(df_norm[[j]])
      }
    }
    
    df_norm <- suppressWarnings(df_norm[do.call(order, df_norm), , drop = FALSE])
    rownames(df_norm) <- NULL 

    return(df_norm)
  }

  comparison_result <- tryCatch({
    # It's good practice to set the seed again right before building,
    # in case other operations might have affected the RNG state,
    # though in this specific flow it might be redundant.
    set.seed(123)
    user_built_layers <- ggplot_build(user_res)$data
    set.seed(123)
    correct_built_layers <- ggplot_build(correct_res)$data

    if (length(user_built_layers) != length(correct_built_layers)) {
      return(FALSE) 
    }

    if (length(user_built_layers) == 0) {
      return(TRUE) 
    }

    all_layers_equal <- all(vapply(seq_along(user_built_layers), function(i) {
      u_layer_data <- normalise_df_for_comparison(user_built_layers[[i]])
      c_layer_data <- normalise_df_for_comparison(correct_built_layers[[i]])
      isTRUE(all.equal(u_layer_data, c_layer_data, check.attributes = FALSE))
    }, logical(1)))

    return(all_layers_equal)
  }, error = function(e) {
    FALSE 
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