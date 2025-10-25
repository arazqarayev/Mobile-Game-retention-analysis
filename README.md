# Mobile-Game-retention-analysis
🎮 Mobile Game Retention Analysis — On  R 

# ======================================================
# 🎮 Mobile Game Retention Analysis — One-Click R Script
# - Reads mobilegame.csv
# - Computes simple KPIs, version summary, segmentation
# - Saves figures + CSV outputs


need <- c("tidyverse")
has  <- need %in% rownames(installed.packages())
if (any(!has)) install.packages(need[!has], repos = "https://cloud.r-project.org")
library(tidyverse)

# 1) Load data 

df <- read.csv("mobilegame.csv")

# Basic required columns
req_cols <- c("userid","version","sum_gamerounds","retention_1","retention_7")
miss <- setdiff(req_cols, names(df))
if (length(miss) > 0) stop(paste0("❌ Missing columns: ", paste(miss, collapse=", ")))

# Ensure logicals (TRUE/FALSE)
if (!is.logical(df$retention_1)) df$retention_1 <- df$retention_1 %in% c(TRUE,"TRUE","1",1)
if (!is.logical(df$retention_7)) df$retention_7 <- df$retention_7 %in% c(TRUE,"TRUE","1",1)

# 2) Quick health checks ------------------------------
cat("Rows:", nrow(df), "Cols:", ncol(df), "\n")
print(colSums(is.na(df)))


kpi <- df %>%
  summarise(
    players        = n_distinct(userid),
    avg_rounds     = mean(sum_gamerounds, na.rm = TRUE),
    median_rounds  = median(sum_gamerounds, na.rm = TRUE),
    max_rounds     = max(sum_gamerounds, na.rm = TRUE),
    day1_retention = mean(retention_1, na.rm = TRUE),
    day7_retention = mean(retention_7, na.rm = TRUE)
  )
print(kpi)

# 2) Version summary 
ver_sum <- df %>%
  group_by(version) %>%
  summarise(
    players         = n(),
    avg_rounds      = mean(sum_gamerounds, na.rm = TRUE),
    retention1_rate = mean(retention_1, na.rm = TRUE),
    retention7_rate = mean(retention_7, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(desc(retention7_rate))
print(head(ver_sum))

# 3) Segmentation (New/Regular/Heavy) -----------------
seg <- df %>%
  mutate(segment = case_when(
    sum_gamerounds < 10   ~ "New",
    sum_gamerounds < 100  ~ "Regular",
    TRUE                  ~ "Heavy"
  )) %>%
  group_by(segment) %>%
  summarise(
    players          = n(),
    avg_rounds       = mean(sum_gamerounds, na.rm = TRUE),
    retention1_rate  = mean(retention_1, na.rm = TRUE),
    retention7_rate  = mean(retention_7, na.rm = TRUE),
    .groups = "drop"
  ) %>% arrange(match(segment, c("New","Regular","Heavy")))
print(seg)


if (!dir.exists("figures")) dir.create("figures")
if (!dir.exists("outputs")) dir.create("outputs")


p_ver <- ggplot(ver_sum, aes(x = fct_reorder(version, retention7_rate),
                             y = retention7_rate)) +
  geom_col() +
  coord_flip() +
  labs(title = "7-Day Retention by Version",
       x = "Version", y = "Retention 7 Rate") +
  theme_minimal()
ggsave("figures/retention_by_version.png", p_ver, width = 7, height = 4.5, dpi = 120)


p_seg <- ggplot(seg, aes(x = segment, y = retention7_rate)) +
  geom_col() +
  labs(title = "7-Day Retention by Segment",
       x = "Segment", y = "Retention 7 Rate") +
  theme_minimal()
ggsave("figures/retention_by_segment.png", p_seg, width = 6, height = 4.5, dpi = 120)


p_hist <- ggplot(df, aes(x = sum_gamerounds, fill = retention_7)) +
  geom_histogram(bins = 60, alpha = 0.85, position = "identity") +
  labs(title = "Game Rounds Distribution by 7-day Retention",
       x = "Total Game Rounds", y = "Count") +
  theme_minimal()
ggsave("figures/rounds_hist_by_ret7.png", p_hist, width = 7, height = 4.5, dpi = 120)


write.csv(kpi,     "outputs/global_kpis.csv",       row.names = FALSE)
write.csv(ver_sum, "outputs/version_summary.csv",    row.names = FALSE)
write.csv(seg,     "outputs/segment_summary.csv",    row.names = FALSE)


readme_text <- c(
"# 🎮 Mobile Game Retention Analysis (R)",
"",
"This project analyzes player retention and engagement using **mobilegame.csv**.",
"",
"## 📊 Dataset",
"- Required columns: `userid`, `version`, `sum_gamerounds`, `retention_1`, `retention_7`",
"",
"## 🚀 How to Run",
"```r",
"source(\"run_mobilegame_project.R\")",
"```",
"Outputs will be saved to **figures/** and **outputs/**.",
"",
"## 📈 Key Metrics",
"- **Day 1 / Day 7 Retention**",
"- **Average Rounds per User**",
"- **Version Summary** (players, avg rounds, retention rates)",
"- **Segments**: New (<10), Regular (10–99), Heavy (100+)",
"",
"## 🖼️ Figures",
"- figures/retention_by_version.png",
"- figures/retention_by_segment.png",
"- figures/rounds_hist_by_ret7.png",
"",

writeLines(readme_text, "README.md")

cat("✅ Done.\nSaved figures & CSVs, and wrote README.md\n")
# ======================================================
