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

  # --- Helper function to extract comparable info from a ggplot object ---
  get_plot_structure_and_params <- function(p_obj) {
    if (!inherits(p_obj, "ggplot")) return(NULL)
    
    # It's good practice to set the seed before any operation that might
    # involve randomness, even if it's just building the plot for inspection.
    set.seed(123) 
    # built_plot <- tryCatch(ggplot_build(p_obj), error = function(e) NULL)
    # if (is.null(built_plot)) return(NULL) # We might not need full build for this structural comparison

    plot_info <- list()
    
    # Base aesthetics (convert quosures to character strings)
    plot_info$base_mapping <- lapply(p_obj$mapping, rlang::as_label)
    
    # Number of layers
    plot_info$n_layers <- length(p_obj$layers)
    
    # Layer-specific info
    plot_info$layers_info <- vector("list", plot_info$n_layers)
    if (plot_info$n_layers > 0) {
      for (i in 1:plot_info$n_layers) {
        layer_i <- p_obj$layers[[i]]
        layer_info <- list()
        
        layer_info$geom_class <- class(layer_i$geom)[1]
        layer_info$stat_class <- class(layer_i$stat)[1]
        
        # Layer aesthetics (convert quosures to character strings)
        layer_info$mapping <- lapply(layer_i$mapping, rlang::as_label)
        
        # Key static parameters from geom_params and aes_params
        # We will be more specific based on geom type
        static_params <- list()
        
        # Parameters common to many geoms (from geom_params)
        if (!is.null(layer_i$geom_params$alpha)) static_params$alpha <- layer_i$geom_params$alpha
        if (!is.null(layer_i$geom_params$colour)) static_params$colour <- layer_i$geom_params$colour # Note: 'color' is an alias often resolved by ggplot
        if (!is.null(layer_i$geom_params$color)) static_params$colour <- layer_i$geom_params$color # Store as 'colour' for consistency
        if (!is.null(layer_i$geom_params$fill)) static_params$fill <- layer_i$geom_params$fill
        
        # Parameters from aes_params (static aesthetic values like linewidth)
        if (!is.null(layer_i$aes_params$linewidth)) static_params$linewidth <- layer_i$aes_params$linewidth
        if (!is.null(layer_i$aes_params$size)) { # if 'size' is in aes_params, could be for points or lines not covered by linewidth
            if(layer_info$geom_class %in% c("GeomPoint", "GeomLine") && is.null(static_params$linewidth)) {
                 static_params$size_aes_param <- layer_i$aes_params$size
            } else if (layer_info$geom_class == "GeomErrorbar" && is.null(static_params$linewidth)) {
                 # For errorbar, prefer linewidth if available, else size might be used by older code.
                 # The solution script uses linewidth now.
                 static_params$size_aes_param <- layer_i$aes_params$size
            }
        }


        # Specific geom_params
        if (layer_info$geom_class == "GeomViolin") {
          if (!is.null(layer_i$geom_params$draw_quantiles)) static_params$draw_quantiles <- layer_i$geom_params$draw_quantiles
        } else if (layer_info$geom_class == "GeomPoint" && layer_info$stat_class == "StatSummary") { # For stat_summary point
          if (!is.null(layer_i$geom_params$size)) static_params$point_size <- layer_i$geom_params$size
        } else if (layer_info$geom_class == "GeomErrorbar") { # For stat_summary errorbar
           if (!is.null(layer_i$geom_params$width)) static_params$errorbar_width <- layer_i$geom_params$width
           # linewidth is now preferred for the thickness of the errorbar lines.
           # if 'size' was used in geom_params for line thickness, it's older ggplot2 style.
           # Our correct script now uses 'linewidth' in aes_params for this.
           if (!is.null(layer_i$geom_params$size) && is.null(static_params$linewidth)) static_params$errorbar_geom_param_size <- layer_i$geom_params$size

        } else if (layer_info$geom_class == "GeomJitter") {
          if (!is.null(layer_i$geom_params$width)) static_params$jitter_width <- layer_i$geom_params$width
        }
        
        layer_info$static_params <- static_params
        
        # For stat_summary, check the 'fun' or 'fun.data' (deparse to convert function to string)
        if (layer_info$stat_class == "StatSummary") {
          if (!is.null(layer_i$stat_params$fun)) layer_info$fun <- deparse(layer_i$stat_params$fun)
          if (!is.null(layer_i$stat_params$fun.data)) layer_info$fun.data <- deparse(layer_i$stat_params$fun.data)
        }
        
        plot_info$layers_info[[i]] <- layer_info
      }
    }
    
    # Faceting information (convert formula to character string)
    if (!is.null(p_obj$facet) && class(p_obj$facet)[1] != "FacetNull") {
      facet_params <- p_obj$facet$params
      if(!is.null(facet_params$facets)) { # typically for facet_wrap
        plot_info$facet_formula <- rlang::as_label(facet_params$facets)
      } else if (!is.null(facet_params$rows) || !is.null(facet_params$cols)) { # typically for facet_grid
         row_label <- if(!is.null(facet_params$rows)) rlang::as_label(facet_params$rows) else "."
         col_label <- if(!is.null(facet_params$cols)) rlang::as_label(facet_params$cols) else "."
         plot_info$facet_formula <- paste(row_label, "~", col_label)
      }
    } else {
      plot_info$facet_formula <- NULL
    }
    
    # Remove NULLs from the list for cleaner comparison with all.equal
    remove_nulls <- function(l) {
      if (is.list(l)) {
        l <- l[!sapply(l, is.null)]
        lapply(l, remove_nulls)
      } else {
        l
      }
    }
    plot_info <- remove_nulls(plot_info)

    return(plot_info)
  }

  # --- Main function logic ---
  student_env <- new.env(parent = .GlobalEnv)
  script_ran_ok <- tryCatch({
    set.seed(123) # Set seed before sourcing student's script
    source(e$script_temp_path, local = student_env)
    TRUE
  }, error = function(err) {
    # message(paste("Error sourcing student script:", err$message)) # For debugging
    FALSE
  })

  if (!script_ran_ok || !exists(result_name, envir = student_env, inherits = FALSE)) {
    # message("Student script failed or did not create result_name.") # For debugging
    return(FALSE)
  }
  user_res <- get(result_name, envir = student_env, inherits = FALSE)

  correct_env <- new.env(parent = .GlobalEnv)
  correct_script_ran_ok <- tryCatch({
    set.seed(123) # Set THE SAME seed before sourcing correct script
    source(e$correct_script_temp_path, local = correct_env)
    TRUE
  }, error = function(err) {
    # message(paste("Error sourcing correct script:", err$message)) # For debugging
    FALSE
  })
  
  if (!correct_script_ran_ok || !exists(result_name, envir = correct_env, inherits = FALSE)) {
    # message("Correct script failed or did not create result_name.") # For debugging
    return(FALSE) 
  }
  correct_res <- get(result_name, envir = correct_env, inherits = FALSE)

  user_plot_info <- get_plot_structure_and_params(user_res)
  correct_plot_info <- get_plot_structure_and_params(correct_res)
  
  # --- DEBUGGING OUTPUT (Optional - enable by options(swirl_debug_tests = TRUE) ) ---
  if(isTRUE(getOption("swirl_debug_tests", FALSE))) {
      message("--- DEBUG: User Plot Info (plot_results_identical) ---")
      print(str(user_plot_info)) # Using print(str(...)) for better console output of structure
      # dput(user_plot_info, file = "user_plot_info_debug.txt") # For detailed file output
      message("--- DEBUG: Correct Plot Info (plot_results_identical) ---")
      print(str(correct_plot_info))
      # dput(correct_plot_info, file = "correct_plot_info_debug.txt")
      message("--- DEBUG: all.equal output (plot_results_identical) ---")
      print(all.equal(user_plot_info, correct_plot_info, check.attributes = FALSE, tolerance = 1e-6))
  }
  # --- END DEBUGGING OUTPUT ---

  if (is.null(user_plot_info) || is.null(correct_plot_info)) {
    # message("Plot info extraction failed for user or correct plot.") # For debugging
    return(FALSE)
  }
  
  comparison_status <- isTRUE(all.equal(user_plot_info, correct_plot_info, 
                                        check.attributes = FALSE, tolerance = 1e-6))
  
  return(comparison_status)
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