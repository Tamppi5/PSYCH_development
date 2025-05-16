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

# --- Finding the stroop.csv file ---
# Current working directory is not where the file is, so we need to search for it

# Start with a function to find files recursively
find_file <- function(filename, start_dir = getwd(), max_depth = 5) {
  if (max_depth < 0) return(NULL)  # Stop if we've gone too deep
  
  # Check if file exists in current directory
  current_path <- file.path(start_dir, filename)
  if (file.exists(current_path)) {
    return(current_path)
  }
  
  # If not, check subdirectories
  subdirs <- list.dirs(start_dir, recursive = FALSE)
  for (dir in subdirs) {
    found_path <- find_file(filename, dir, max_depth - 1)
    if (!is.null(found_path)) {
      return(found_path)
    }
  }
  
  # If we get here, file wasn't found
  return(NULL)
}

# First, try up to 3 directories above the current working directory
found_path <- NULL
current_dir <- getwd()

# Check the current directory and its subdirectories
print(paste("Searching for stroop.csv in:", current_dir))
found_path <- find_file("stroop.csv", current_dir, max_depth = 3)

# If not found, try going up to parent directories and searching
if (is.null(found_path)) {
  for (i in 1:3) { # Try up to 3 levels up
    parent_dir <- dirname(current_dir)
    if (parent_dir == current_dir) break  # Break if we've reached the root
    
    current_dir <- parent_dir
    print(paste("Searching for stroop.csv in:", current_dir))
    found_path <- find_file("stroop.csv", current_dir, max_depth = 3)
    
    if (!is.null(found_path)) break  # Break if found
  }
}

# If still not found, try an educated guess - look for a swirl courses directory
if (is.null(found_path)) {
  # Check if we can find a "swirl_courses" directory
  swirl_path <- find_file("swirl_courses", getwd(), max_depth = 2)
  if (!is.null(swirl_path)) {
    # If we found a swirl_courses directory, look for our lesson in it
    print(paste("Searching in possible swirl_courses directory:", swirl_path))
    found_path <- find_file("stroop.csv", swirl_path, max_depth = 5)
  }
}

print(paste("Search result for stroop.csv:", ifelse(is.null(found_path), "Not found", found_path)))

# If found, use it; otherwise, fallback to trying in the current directory
if (!is.null(found_path)) {
  data_path <- found_path
  print(paste("Found stroop.csv at:", data_path))
} else {
  # Last resort: Hardcoded alternative - try to construct full path to the course directory
  # Look for a directory structure like ".../swirl_courses/PsychMetHCI/Measures_of_Variability"
  possible_course_dir <- file.path(getwd(), "swirl_courses", "PsychMetHCI", "Measures_of_Variability", "stroop.csv")
  
  if (file.exists(possible_course_dir)) {
    data_path <- possible_course_dir
    print(paste("Using hardcoded fallback path:", data_path))
  } else {
    # Absolute last resort: Try directly in current directory
    data_path <- "stroop.csv"
    print(paste("WARNING: Could not find stroop.csv, trying in current directory as last resort:", data_path))
  }
}

print(paste("Final data_path being used:", data_path))
print(paste("Does the file exist at this path?", file.exists(data_path)))

# Attempt to load the data (will error if file is not found)
data <- tryCatch({
  read.csv(data_path)
}, error = function(e) {
  # If loading fails, create a simulated dataset as a last resort
  print("ERROR: Could not load stroop.csv. Creating simulated data to allow lesson to continue.")
  print("This is NOT the real data! Please fix the file path issue for the next run.")
  
  # Create fake data that mimics the structure of stroop.csv
  ppnr <- 1:131
  congruent <- runif(131, 6, 32)  # Random values between 6-32
  incongruent <- congruent + runif(131, 0, 15)  # Slightly higher than congruent
  year <- sample(c(2013, 2014), 131, replace = TRUE)
  
  data.frame(PPNR = ppnr, Congruent = congruent, Incongruent = incongruent, Year = year)
})

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
