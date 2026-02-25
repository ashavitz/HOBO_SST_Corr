# ---- Minimal workflow test script ----

# Always compute month folder like Data/NECOFS/2026-02
month_folder <- format(Sys.Date(), "%Y-%m")
out_dir <- file.path("Data", "NECOFS", month_folder)

# Create directory if it doesn't exist
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Simple data frame
df <- data.frame(
  message = "workflow test",
  date = Sys.Date(),
  time = Sys.time()
)

# Write CSV with daily filename
day_stamp <- format(Sys.Date(), "%Y-%m-%d")
out_csv <- file.path(out_dir, paste0("TEST_", day_stamp, ".csv"))

write.csv(df, out_csv, row.names = FALSE)

cat("Test CSV written to:", out_csv, "\n")






# # Folder like Daily_CSV/2026-02
# month_folder <- format(Sys.Date(), "%Y-%m")
# out_dir <- file.path("Data/NECOFS", month_folder)
# dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
# 
# # Optional: name outputs with the day so each day is a new file
# day_stamp <- format(Sys.Date(), "%Y-%m-%d")
# # Example:
# # out_csv <- file.path(out_dir, paste0("NECOFS_", day_stamp, ".csv"))
# # write.csv(df, out_csv, row.names = FALSE)