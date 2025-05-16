#Measures of Variability

#formula <- imager::load.image("https://static1.squarespace.com/static/5a1855c81f318d6ebcce6e54/5af4702e88251b5dfff59172/5af4707e1ae6cf37588f6dac/1525970376649/variance.png?format=1000w")
#formula2 <- imager::load.image("https://static1.squarespace.com/static/5a1855c81f318d6ebcce6e54/5af4702e88251b5dfff59172/5af4707e6d2a7325bf575418/1525970376650/standarddeviation.png?format=1000w")

standdev <- "s^2 = $\\frac{\\sum{}{}(X_i-\\bar{x})^2}{n-1}$ so s= $\\sqrt{\\frac{\\sum{}{}(X_i-\\bar{x})^2}{n-1}}$"
variancepop <- "variance of a population = $\\frac{\\sum{}{}(X_i-\\mu)^2}{N}$"
variancesamp <- "variance of a sample = $\\frac{\\sum{}{}(X_i-\\bar{x})^2}{n-1}$"

plot.equation <- function(equation){
  require(latex2exp)
  plot(TeX(equation), cex=2)
}

# --- Robustly determine data_path ---
lesson_dir <- NULL
data_path <- NULL
method_used <- "Unknown"

# Method 1: Try to use the path of the currently executing script
current_script_file_from_sys_frame <- NULL
tryCatch({
  # sys.frame(1)$ofile should give the path of the sourced file
  # We need to ensure it's a valid character string before using it
  sf_ofile <- sys.frame(1)$ofile
  if (is.character(sf_ofile) && length(sf_ofile) == 1 && nzchar(sf_ofile)) {
    current_script_file_from_sys_frame <- sf_ofile
  }
}, error = function(e) {
  # In case sys.frame(1)$ofile itself errors or is not found
  current_script_file_from_sys_frame <- NULL
})

if (!is.null(current_script_file_from_sys_frame)) {
  lesson_dir <- dirname(current_script_file_from_sys_frame)
  data_path <- file.path(lesson_dir, "stroop.csv")
  method_used <- "sys.frame(1)$ofile"
} else {
  # Method 2: Fallback if sys.frame(1)$ofile is not available/valid.
  # This relies on Swirl having set the working directory to the lesson's directory.
  method_used <- "getwd()"
  lesson_dir <- getwd() # For debugging, show what getwd() is
  data_path <- "stroop.csv" # Attempt to load directly from current working directory
}

# For debugging: print the paths and method used
print(paste("Path determination method used:", method_used))
print(paste("Current working directory reported by getwd():", getwd()))
print(paste("Lesson directory determined as:", lesson_dir))
print(paste("Attempting to load data from data_path:", data_path))
print(paste("Does the target file exist at data_path? ", file.exists(data_path)))


# Load the stroop.csv dataset
data <- read.csv(data_path)

# The UCBAdmissions data is no longer the primary 'data'
# data <- data.frame(UCBAdmissions) # Commented out

calculate_me <- c(512, 89, 120, 53, 24) # This seems to be a generic example vector

set.seed(131)
men <- c(rep("male", 50), rep("female", 50), rep("male", 50), rep("female", 50))
highsd <- c(rep("low SD",100), rep("high SD", 100))
highsd_men <- rnorm(50, mean=4, sd=4)
highsd_women <- rnorm(50, mean=8, sd=3)
lowsd_men <- rnorm(50, mean=4, sd=.5)
lowsd_women <- rnorm(50, mean=8, sd=.3)
scores <- c(lowsd_men, lowsd_women, highsd_men, highsd_women)

data_e <- data.frame(men, highsd, scores)
example <- ggplot2::ggplot(data=data_e, aes(x=men, y=scores, group=highsd)) + 
  geom_point(position = "jitter") + 
  facet_grid(.~ highsd) 
# The 'example' plot is a generic illustration of SD, so it can remain.

rm(men, highsd, highsd_men, highsd_women, lowsd_men, lowsd_women, scores, data_e) # Added data_e to rm

swirl_options(swirl_logging = FALSE)
