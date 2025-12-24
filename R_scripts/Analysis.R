# ---------------------------------------------
# Module: 7COM1079
# Project: Amusement Park Injury Analysis
# Velan Sivasankaran
#
# Research Question:
#   Is there a difference in the proportion of injured
#   body parts between male and female riders?
# ---------------------------------------------------------

# 0. Use the imported dataset -----------------------------

# This assumes you already imported the Excel/CSV file into RStudio
# and it appears in the Environment as:
#   Amusement_Park_Injuries_xlsxCleaned

df <- Amusement_Park_Injuries_xlsxCleaned

# Check the column names once (optional, for your understanding)
print(names(df))


# 1. Clean Gender and Body Part ----------------------------

# Create a cleaned Gender column: "Female" / "Male"
df$Gender_clean <- NA
df$Gender_clean[df$Gender == "F"] <- "Female"
df$Gender_clean[df$Gender %in% c("M", "m")] <- "Male"

# Get the Body Part column (it might be called "Body.Part" or "Body Part")
if ("Body.Part" %in% names(df)) {
  body_raw <- df$Body.Part
} else if ("Body Part" %in% names(df)) {
  body_raw <- df[["Body Part"]]
} else {
  stop("Cannot find a 'Body Part' or 'Body.Part' column. Check names(df).")
}

# Remove leading/trailing spaces
body_trim <- trimws(body_raw)

# Standardise case: First letter uppercase, rest lowercase
body_clean <- ifelse(
  is.na(body_trim),
  NA,
  paste0(
    toupper(substr(body_trim, 1, 1)),
    tolower(substr(body_trim, 2, nchar(body_trim)))
  )
)

df$BodyPart_clean <- body_clean


# 2. Keep only rows with Male/Female and valid BodyPart ----

valid_rows <- !is.na(df$Gender_clean) &
  !is.na(df$BodyPart_clean) &
  df$Gender_clean %in% c("Male", "Female")

df2 <- df[valid_rows, ]

# Quick sanity checks (optional)
cat("Gender counts:\n")
print(table(df2$Gender_clean))

cat("\nTop 10 Body Parts:\n")
print(head(sort(table(df2$BodyPart_clean), decreasing = TRUE), 10))


# 3. Contingency table (Gender x Body Part) ----------------

contingency <- table(df2$Gender_clean, df2$BodyPart_clean)

cat("\nContingency table (first 10 body parts):\n")
print(contingency[, 1:10])


# 4. Chi-square test of independence -----------------------

chi_result <- chisq.test(contingency)

cat("\nChi-square test result:\n")
print(chi_result)

cat("\nChi-square statistic:", chi_result$statistic,
    "\nDegrees of freedom:", chi_result$parameter,
    "\nP-value:", chi_result$p.value, "\n")


# 5. Focus on Top 5 most frequent body parts ---------------

# Total frequency of each body part
body_counts <- sort(colSums(contingency), decreasing = TRUE)

# Take the top 5
top5 <- names(body_counts)[1:5]

cat("\nTop 5 body parts:\n")
print(top5)

# Subset the contingency table to only these 5 body parts
cont_top5 <- contingency[, top5]

cat("\nContingency table for top 5 body parts:\n")
print(cont_top5)


# 6. Show the stacked bar chart in RStudio -----------------

# This will display the graph in the Plots pane
barplot(
  cont_top5,
  beside = FALSE,                       # stacked bars
  col = c("lightblue", "pink"),         # colours for Female/Male
  main = "Top 5 Injured Body Parts by Gender",
  xlab = "Body Part",
  ylab = "Number of Injuries",
  legend.text = rownames(cont_top5),
  args.legend = list(x = "topright")
)


# 7. Save the same plot as a PNG file ----------------------

# Check where the file will be saved:
cat("\nCurrent working directory is:\n")
print(getwd())

# Save the plot to a PNG file in the working directory
# Optional: Full body-part bar plot used in report
png("BodyPart_by_Gender.png", width = 1200, height = 800)

barplot(
  table(df2$Gender_clean, df2$BodyPart_clean),
  beside = TRUE,
  main = "Distribution of Injured Body Parts by Gender",
  xlab = "Body Part Category",
  ylab = "Number of Injuries",
  legend.text = TRUE
)

dev.off()


cat("\nSaved plot as 'top5_bodyparts_by_gender.png' in the folder:\n")
print(getwd())

