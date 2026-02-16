# Instructions for my Lab Research Assistants
  # Study Name: Social RL 

# click on ANY CSV FILE in the DATA FOLDER that's in the SOCIAL RL FOLDER
any_file <- file.choose()

# this gets the folder containing that file
data_dir <- dirname(any_file)
cat("Data folder selected:", data_dir, "\n")

#### ONCE YOU RUN THIS, TYPE THE Participant ID IN THE CONSOLE DIRECTLY!!!
participant_id <- trimws(readline("Enter participant ID: ")) # note here to run, write test1

# this finds only CSVs that match this participant
csv_files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
participant_file <- csv_files[grepl(participant_id, csv_files)]

# safety checks
if (length(participant_file) == 0) stop("No CSV found for this participant ID.")
if (length(participant_file) > 1) stop("Multiple CSVs found for this participant ID. Check filenames.")

# read CSV and analyze the target column
data <- read.csv(participant_file)

# required columns
required_cols <- c("OneOrTwo", "PictureValence")
missing_cols <- setdiff(required_cols, names(data))

if (length(missing_cols) > 0) {
  stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
}

# count rows where:OneOrTwo == 2 AND PictureValence == 1
num_positive_valence <- sum(
  data$OneOrTwo == 2 & data$PictureValence == 1,
  na.rm = TRUE
)

# converting to seconds
total_seconds <- num_positive_valence * 15

# Output
## THIS IS HOW LONG THEY WILL SPEND WITH SOMEONE ELSE IN THE NEXT ACTIVITY

cat("====================================\n")
cat("Participant ID:", participant_id, "\n")
cat("Positive valence trials:", num_positive_valence, "\n")
cat("Total valence time:", total_seconds, "seconds\n")
cat("====================================\n")
