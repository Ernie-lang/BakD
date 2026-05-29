# ============================================================
# Bakalaura darbs — Statistiskā analīze
# Klasisko ģeometrisko proporciju ietekme uz telpiskā mēroga uztveri
# ============================================================
# This script:
# 1. Reads data from 15 Excel questionnaire files
# 2. Calculates descriptive statistics (M, SD) for each room
# 3. Runs repeated measures ANOVA for 3 dependent variables
# 4. Calculates effect sizes (eta squared)
# 5. Runs post-hoc Bonferroni t-tests between room pairs
# ============================================================

# ============================================================
# INSTALL PACKAGES (run once; safe to re-run — skips if present)
# Packages are external libraries that add extra functions to R
# Only two packages are needed: readxl (Excel) and dplyr (data wrangling).
# The ANOVA and effect sizes are computed with base R, so no extra
# statistics packages are required.
# ============================================================
required <- c("readxl", "dplyr")
to_install <- required[!(required %in% rownames(installed.packages()))]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}

# Load the packages (run every time you open R)
library(readxl)    # read_excel()
library(dplyr)     # %>%, group_by(), summarise(), case_when()

# ============================================================
# 1. DATA LOADING
# ============================================================

# Folder containing the 15 Excel files (aptauja_0.xlsx ... aptauja_14.xlsx).
# The files live in the "results" subfolder next to this script, so by default
# we look there. If you run R from inside the "results" folder itself, "."
# is used instead. To point somewhere else, set an absolute path, e.g.:
#   data_dir <- "/Users/ernestsabols/Downloads/thesis-statistics/results"
data_dir <- "results"
if (!dir.exists(data_dir)) data_dir <- "."

# Room names in the order they appear as sheets in Excel
# (sheet 1 is the demographics form, room sheets are sheets 2-7)
room_labels <- c("Kvadrats_1x1",    # Sheet 2 (Telpa 1) — proportion 1:1
                 "Zelta_1x1618",    # Sheet 3 (Telpa 2) — proportion 1:1.618
                 "Palladio_3x4",    # Sheet 4 (Telpa 3) — proportion 3:4
                 "Palladio_2x3",    # Sheet 5 (Telpa 4) — proportion 2:3
                 "Palladio_1x2",    # Sheet 6 (Telpa 5) — proportion 1:2
                 "Ekstrama_1x3")    # Sheet 7 (Telpa 6) — proportion 1:3

# Left-hand (negative) labels of the 7 semantic-differential rows, in order.
# We locate each rating by finding its label rather than by guessing the cell
# position, so the script is robust to small layout shifts between files.
# Items 1-2 = perceptual dimension, items 3-7 = affective dimension.
# NOTE: the Latvian text is written with \u Unicode escapes so the comparisons
# work no matter what locale/encoding R happens to run in (e.g. the "C" locale
# used by Rscript). Each escape is the exact letter shown in the comment.
sd_left_labels <- c("Šaura",        # 1 (perceptual) Šaura      - Plaša
                    "Noslēgta",     # 2 (perceptual) Noslēgta   - Atvērta
                    "Neērta",       # 3 (affective)  Neērta     - Ērta
                    "Saspringta",        # 4 (affective)  Saspringta - Relaksējoša
                    "Neharmoniska",      # 5 (affective)  Neharmoniska - Harmoniska
                    "Nepatīkama",   # 6 (affective)  Nepatīkama - Patīkama
                    "Nepievilcīga") # 7 (affective)  Nepievilcīga - Pievilcīga

# Mapping from Latvian text answers to numbers (1-5) for the scale-rating item
scale_map <- c("Daudz mazāka"          = 1,  # Daudz mazāka       (much smaller)
               "Mazāka"                = 2,  # Mazāka             (smaller)
               "Līdzīga parastajai" = 3, # Līdzīga parastajai (similar to normal)
               "Lielāka"               = 4,  # Lielāka            (larger)
               "Daudz lielāka"         = 5)  # Daudz lielāka      (much larger)

# Force the Latvian text to UTF-8 so it matches the UTF-8 strings read from the
# Excel files even when R runs in the "C" locale (as Rscript does by default).
Encoding(sd_left_labels) <- "UTF-8"
.sm_names <- names(scale_map); Encoding(.sm_names) <- "UTF-8"
names(scale_map) <- .sm_names

# ------------------------------------------------------------
# Helper: read one room sheet and pull out the 7 SD ratings and
# the scale answer. The sheet has an irregular layout (titles,
# legends, a 1-7 column header), so instead of grabbing every
# number on the sheet we:
#   - find each rating by matching its row label (sd_left_labels)
#   - find the scale answer as the row that contains exactly ONE
#     of the five scale options (the legend row lists all five,
#     the answer row lists only the chosen one)
# ------------------------------------------------------------
read_room_sheet <- function(fname, sheet_idx) {
  # col_names = FALSE -> read the raw grid; .name_repair keeps it quiet
  raw <- suppressMessages(read_excel(fname, sheet = sheet_idx,
                                     col_names = FALSE, .name_repair = "minimal"))
  grid <- as.data.frame(raw, stringsAsFactors = FALSE)

  sd_scores <- rep(NA_real_, length(sd_left_labels))
  scale_val <- NA_real_

  for (r in seq_len(nrow(grid))) {
    cells     <- unlist(grid[r, ], use.names = FALSE)
    chars     <- trimws(as.character(cells))
    Encoding(chars) <- "UTF-8"
    nums      <- suppressWarnings(as.numeric(cells))

    # Semantic-differential rating: this row carries one of the 7 labels
    lab_idx <- which(sd_left_labels %in% chars)
    if (length(lab_idx) == 1) {
      rating <- nums[!is.na(nums) & nums >= 1 & nums <= 7]
      if (length(rating) >= 1) sd_scores[lab_idx] <- rating[1]
    }

    # Scale answer: row holding exactly one of the scale options
    key_hits <- names(scale_map)[names(scale_map) %in% chars]
    if (length(key_hits) == 1) scale_val <- scale_map[[key_hits]]
  }

  list(sd_scores = sd_scores, scale = scale_val)
}

# Create an empty data frame to store all results
# Each row will represent one participant's ratings for one room
# Final table will have 15 participants × 6 rooms = 90 rows
all_data <- data.frame()

# Get list of all Excel files in the data folder
# pattern = only files starting with "aptauja_" and ending with ".xlsx"
files <- list.files(data_dir, pattern = "aptauja_.*\\.xlsx$", full.names = TRUE)

# Sort by the participant number in the filename (aptauja_2 before aptauja_10),
# not alphabetically — otherwise participant IDs would be jumbled.
file_nums <- as.integer(sub(".*aptauja_(\\d+).*", "\\1", basename(files)))
ord       <- order(file_nums)
files     <- files[ord]
file_nums <- file_nums[ord]

if (length(files) == 0) {
  stop("No 'aptauja_*.xlsx' files found in '", normalizePath(data_dir), "'. ",
       "Set data_dir to the folder that holds the Excel files.")
}

# Loop through each Excel file (one file = one participant)
for (k in seq_along(files)) {
  fname          <- files[k]
  participant_id <- file_nums[k]   # participant ID = number from the filename

  # Loop through each room sheet (1 to 6)
  for (i in 1:6) {

    # tryCatch handles errors gracefully — if a sheet can't be read,
    # it skips it instead of crashing the whole script
    tryCatch({

      # Read the room sheet. i+1 because sheet 1 is demographics,
      # so the room sheets are sheets 2-7.
      parsed    <- read_room_sheet(fname, sheet_idx = i + 1)
      sd_scores <- parsed$sd_scores   # 7 ratings, in sd_left_labels order
      scale_val <- parsed$scale       # scale answer 1-5 (or NA)

      # Only proceed if all 7 SD ratings were found
      if (sum(!is.na(sd_scores)) == 7) {

        # Perceptual dimension mean (items 1-2: Šaura/Plaša, Noslēgta/Atvērta)
        perceptual <- mean(sd_scores[1:2])

        # Affective dimension mean (items 3-7: Ērta, Relaksējoša,
        # Harmoniska, Patīkama, Pievilcīga)
        affective  <- mean(sd_scores[3:7])

        # Create one row of data for this participant-room combination
        row <- data.frame(
          participant = participant_id,  # which participant (0-14)
          room = room_labels[i],         # which room (e.g. "Kvadrats_1x1")

          # Classify room as Anchor (extreme) or Classical (harmonious)
          room_type = case_when(
            room_labels[i] == "Kvadrats_1x1"  ~ "Anchor",    # 1:1 square
            room_labels[i] == "Ekstrama_1x3"  ~ "Anchor",    # 1:3 extreme
            TRUE                               ~ "Classical"  # all Palladian + Golden
          ),

          perceptual = perceptual,        # mean of Šaura + Noslēgta items (1-7)
          affective  = affective,         # mean of 5 affective items (1-7)
          scale      = as.numeric(scale_val)  # scale rating (1-5)
        )

        # Add this row to the bottom of the full data table
        all_data <- rbind(all_data, row)
      } else {
        cat("Warning: sheet", i + 1, "in", basename(fname),
            "had", sum(!is.na(sd_scores)), "of 7 ratings — skipped\n")
      }

    }, error = function(e) {
      # If sheet can't be read, print a warning and continue
      cat("Warning: could not read sheet", i + 1, "from file", basename(fname),
          "-", conditionMessage(e), "\n")
    })
  }
}

# Convert participant and room to factors
# Factor = categorical variable (not a continuous number)
# participant: IDs are labels, not real numbers (0 is not "less than" 1)
all_data$participant <- as.factor(all_data$participant)

# room: factor with specific order so tables show rooms in correct sequence
all_data$room <- factor(all_data$room, levels = room_labels)

# Print summary to confirm data loaded correctly
cat("Data loaded successfully!\n")
cat("Total rows:", nrow(all_data), "\n")           # should be 90 (15 × 6)
cat("Participants:", length(unique(all_data$participant)), "\n\n")  # should be 15

# ============================================================
# 2. DESCRIPTIVE STATISTICS
# ============================================================
# Calculate mean (M) and standard deviation (SD) for each room
# M = average score across all 15 participants
# SD = how spread out the scores were (small SD = participants agreed)

cat("=== DESCRIPTIVE STATISTICS ===\n\n")

desc_stats <- all_data %>%
  group_by(room) %>%         # split data into 6 groups (one per room)
  summarise(
    n             = n(),      # count of rows (should be 15 per room)
    
    # Perceptual dimension (Plasa + Atverta)
    M_perceptual  = round(mean(perceptual, na.rm = TRUE), 2),  # mean, rounded to 2 decimals
    SD_perceptual = round(sd(perceptual, na.rm = TRUE), 2),    # standard deviation
    
    # Affective dimension (Erta + Relaksejosa + Harmoniska + Patikama + Pievilciga)
    M_affective   = round(mean(affective, na.rm = TRUE), 2),
    SD_affective  = round(sd(affective, na.rm = TRUE), 2),
    
    # Scale rating (1=much smaller, 3=similar, 5=much larger)
    M_scale       = round(mean(scale, na.rm = TRUE), 2),
    SD_scale      = round(sd(scale, na.rm = TRUE), 2)
  )

print(desc_stats)

# ============================================================
# 3. ONE-WAY REPEATED MEASURES ANOVA
# ============================================================
# ANOVA tests whether differences between rooms are statistically significant
# i.e., could the differences have happened by chance?
#
# Formula: dependent_variable ~ room + Error(participant/room)
#   - dependent_variable: what we measure (perceptual, affective, scale)
#   - room: the independent variable (6 proportion conditions)
#   - Error(participant/room): tells R this is repeated measures —
#     the same participant rated all 6 rooms
#
# Output:
#   - F value: ratio of between-room variation to within-room variation
#   - Pr(>F): p-value — probability this result happened by chance
#     if p < 0.05, the result is statistically significant

cat("\n=== ONE-WAY ANOVA ===\n\n")

# ANOVA for perceptual dimension (Plasa + Atverta)
cat("--- Perceptual Dimension (Plasa + Atverta) ---\n")
anova_percept <- aov(perceptual ~ room + Error(participant/room), data = all_data)
summary_percept <- summary(anova_percept)  # extract results table
print(summary_percept)

# ANOVA for affective dimension (5 affective items)
cat("\n--- Affective Dimension (Erta + Relaksejosa + Harmoniska + Patikama + Pievilciga) ---\n")
anova_affect <- aov(affective ~ room + Error(participant/room), data = all_data)
summary_affect <- summary(anova_affect)
print(summary_affect)

# ANOVA for scale rating (1-5 scale)
cat("\n--- Scale Rating (1=much smaller, 5=much larger) ---\n")
anova_scale <- aov(scale ~ room + Error(participant/room), data = all_data)
summary_scale <- summary(anova_scale)
print(summary_scale)

# ============================================================
# 4. EFFECT SIZES (eta squared — η²)
# ============================================================
# eta squared = SS_between / SS_total
# Tells you HOW LARGE the effect is (not just whether it exists)
#
# SS_between = variation explained by room differences
# SS_total = all variation in the data
#
# Interpretation (Cohen 1988):
#   η² = 0.01 → small effect
#   η² = 0.06 → medium effect
#   η² = 0.14 → large effect
#   η² > 0.14 → very large effect

cat("\n=== EFFECT SIZES (eta squared) ===\n\n")

# Function to calculate eta squared manually
calc_eta2 <- function(data, dv) {
  vals <- data[[dv]]                    # get all values for this variable
  grand_mean <- mean(vals, na.rm = TRUE)  # overall mean across all rooms
  
  # SS_total = sum of (each value - grand mean)²
  ss_total <- sum((vals - grand_mean)^2, na.rm = TRUE)
  
  # SS_between = sum of n × (room mean - grand mean)² for each room
  group_means <- tapply(vals, data$room, mean, na.rm = TRUE)  # mean per room
  group_ns    <- tapply(vals, data$room, length)               # n per room
  ss_between  <- sum(group_ns * (group_means - grand_mean)^2)
  
  # eta squared = proportion of total variation explained by room
  eta2 <- ss_between / ss_total
  return(round(eta2, 3))
}

cat("Perceptual dimension: eta2 =", calc_eta2(all_data, "perceptual"), "\n")
cat("Affective dimension:  eta2 =", calc_eta2(all_data, "affective"), "\n")
cat("Scale rating:         eta2 =", calc_eta2(all_data, "scale"), "\n")

# ============================================================
# 5. POST-HOC BONFERRONI TESTS
# ============================================================
# ANOVA tells us "differences exist" but not WHICH rooms differ
# Post-hoc paired t-tests compare specific room pairs
#
# We only compare anchor rooms (Square, Extreme) against all others
# Total comparisons: 9
#
# Bonferroni correction: multiply p by number of comparisons (9)
# This prevents false positives from doing many comparisons
# If p_bonf < 0.05 → statistically significant difference between that pair

cat("\n=== POST-HOC BONFERRONI — AFFECTIVE DIMENSION ===\n\n")
cat(sprintf("%-35s %-22s | %6s | %7s | %7s | %s\n", 
            "Room 1", "Room 2", "t", "p", "p_bonf", "sig"))
cat(strrep("-", 85), "\n")

# Define which pairs to compare
# We compare each anchor room against all classical proportion rooms
pairs <- list(
  c("Kvadrats_1x1", "Zelta_1x1618"),   # Square vs Golden Ratio
  c("Kvadrats_1x1", "Palladio_3x4"),   # Square vs Palladian 3:4
  c("Kvadrats_1x1", "Palladio_2x3"),   # Square vs Palladian 2:3
  c("Kvadrats_1x1", "Palladio_1x2"),   # Square vs Palladian 1:2
  c("Kvadrats_1x1", "Ekstrama_1x3"),   # Square vs Extreme (H3)
  c("Ekstrama_1x3", "Zelta_1x1618"),   # Extreme vs Golden Ratio
  c("Ekstrama_1x3", "Palladio_3x4"),   # Extreme vs Palladian 3:4
  c("Ekstrama_1x3", "Palladio_2x3"),   # Extreme vs Palladian 2:3
  c("Ekstrama_1x3", "Palladio_1x2")    # Extreme vs Palladian 1:2
)

n_comparisons <- length(pairs)  # = 9, used for Bonferroni correction

for (pair in pairs) {
  r1 <- pair[1]  # first room in comparison
  r2 <- pair[2]  # second room in comparison
  
  # Extract affective scores for each room
  vals1 <- all_data$affective[all_data$room == r1]
  vals2 <- all_data$affective[all_data$room == r2]
  
  # Paired t-test: paired = TRUE because same participants rated both rooms
  t_result <- t.test(vals1, vals2, paired = TRUE)
  
  # Apply Bonferroni correction: multiply p by number of comparisons
  # min(..., 1.0) ensures p never exceeds 1.0
  p_bonf <- min(t_result$p.value * n_comparisons, 1.0)
  
  # Mark as significant if p_bonf < 0.05
  sig <- ifelse(p_bonf < 0.05, "*", "n.s.")
  
  cat(sprintf("%-35s %-22s | %6.3f | %7.4f | %7.4f | %s\n",
              r1, r2, t_result$statistic, t_result$p.value, p_bonf, sig))
}

cat("\n=== POST-HOC BONFERRONI — SCALE RATING ===\n\n")
cat(sprintf("%-35s %-22s | %6s | %7s | %7s | %s\n", 
            "Room 1", "Room 2", "t", "p", "p_bonf", "sig"))
cat(strrep("-", 85), "\n")

# Same pairs, but now for scale rating
for (pair in pairs) {
  r1 <- pair[1]
  r2 <- pair[2]
  
  # Extract scale ratings for each room
  vals1 <- all_data$scale[all_data$room == r1]
  vals2 <- all_data$scale[all_data$room == r2]
  
  # Paired t-test with Bonferroni correction
  t_result <- t.test(vals1, vals2, paired = TRUE)
  p_bonf <- min(t_result$p.value * n_comparisons, 1.0)
  sig <- ifelse(p_bonf < 0.05, "*", "n.s.")
  
  cat(sprintf("%-35s %-22s | %6.3f | %7.4f | %7.4f | %s\n",
              r1, r2, t_result$statistic, t_result$p.value, p_bonf, sig))
}

cat("\n* = statistically significant (p_bonf < 0.05)\n")
cat("n.s. = not significant\n")
cat("\n=== ANALYSIS COMPLETE ===\n")
