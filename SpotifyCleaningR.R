# 1. Load necessary libraries
library(tidyverse)

# 2. Load the dataset
spotify_data <- read.csv("SpotifyFeatures.csv", stringsAsFactors = FALSE)

# 3. Handle Missing Values & Duplicates
spotify_cleaned <- spotify_data %>%
  drop_na(popularity, acousticness, danceability, energy, instrumentalness, 
          liveness, loudness, speechiness, tempo, valence, mode, genre) %>%
  distinct()

# 4. Feature Engineering
# Convert 'mode' to binary
# Removed 'key' from the selection/processing entirely
spotify_engineered <- spotify_cleaned %>%
  mutate(mode = ifelse(mode == "Major", 1, 0)) %>%
  # Create dummy columns ONLY for time_signature
  dummy_cols(select_columns = c("time_signature"), 
             remove_selected_columns = TRUE)

# 5. Remove Irrelevant Columns
# We explicitly remove 'key'
spotify_prepared <- spotify_engineered %>%
  select(-key)

# 6. Normalize function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

numeric_cols <- c("popularity", "acousticness", "danceability", "duration_ms", 
                  "energy", "instrumentalness", "liveness", "loudness", 
                  "speechiness", "tempo", "valence")

spotify_final <- spotify_prepared %>%
  mutate(across(all_of(numeric_cols), normalize))

# 7. Preview the cleaned data
head(spotify_final)

# 8. Save the cleaned file
write.csv(spotify_final, "Cleaned_Spotify_Features.csv", row.names = FALSE)
