# Step 1: Load the data
df <- read_csv(
  "844stage.csv",
  col_types = cols(
    timestamp = col_datetime(format = "%Y-%m-%d %H:%M"),
    qlevel = col_number(),
    qflag = col_character(),
    stage = col_number()
  )
)

# Step 2: Apply linear interpolation for small gaps (<12)
df <- df %>%
  arrange(timestamp) %>%
  mutate(
    stage_filled = zoo::na.approx(stage, x = timestamp, maxgap = 12, na.rm = FALSE),
    qflag = case_when(
      is.na(stage) & !is.na(stage_filled) ~ "EV_LINEAR",  # Linear fill
      TRUE ~ qflag
    )
  )

# Step 3: Identify NA groups and calculate gap lengths
df <- df %>%
  mutate(is_na = is.na(stage)) %>%
  mutate(group_id = cumsum(!is_na))  # Create NA group IDs

# Summarize NA groups
na_groups <- df %>%
  filter(is_na) %>%
  group_by(group_id) %>%
  summarise(
    start_time = min(timestamp),
    end_time = max(timestamp),
    .groups = "drop"
  ) %>%
  mutate(gap_minutes = as.numeric(difftime(end_time, start_time, units = "mins")))

# Step 4: Identify gaps for spline (60 < gap ≤ 380 minutes)
df <- df %>%
  left_join(na_groups %>% select(group_id, gap_minutes), by = "group_id") %>%
  mutate(
    is_large_gap = gap_minutes > 60 & gap_minutes <= 380,
    is_very_large_gap = gap_minutes > 380,
    qflag = case_when(
      is.na(stage_filled) & is_very_large_gap ~ "MV",  # Too large to fill
      TRUE ~ qflag
    )
  )

# Step 5: Full-series spline interpolation
spline_interp <- as.data.frame(
  spline(
    x = as.numeric(df$timestamp),
    y = df$stage,
    xout = as.numeric(df$timestamp)
  )
)
colnames(spline_interp) <- c("timestamp_numeric", "spline_stage")
spline_interp$timestamp <- as.POSIXct(spline_interp$timestamp_numeric, origin = "1970-01-01", tz = "UTC")

# Join spline to main df
df <- df %>%
  left_join(spline_interp %>% select(timestamp, spline_stage), by = "timestamp")

# Step 6: Fill large gaps with spline
df <- df %>%
  mutate(
    stage_filled = if_else(
      is.na(stage_filled) & is_large_gap & !is.na(spline_stage),
      spline_stage,
      stage_filled
    ),
    qflag = case_when(
      is.na(stage) & is_large_gap & !is.na(stage_filled) ~ "EV_SPLINE",
      TRUE ~ qflag
    )
  )

# Step 7: Apply manual update to a subset range
subset_df <- df %>%
  filter(timestamp >= as.POSIXct("2025-05-06 07:00:00") & timestamp <= as.POSIXct("2025-05-09 00:00:00"))

# Step 7.2: Override flags only for the subset
subset_df <- subset_df %>%
  mutate(
    stage_filled = if_else(
      !is.na(spline_stage), spline_stage, stage_filled
    ),
    qflag = case_when(
      !is.na(spline_stage) ~ "EV_SPLINE",
      is.na(spline_stage) & !is.na(stage_filled) ~ "AV",
      TRUE ~ qflag
    )
  )

# Step 7.3: Merge updated subset back into full dataset
final_df <- df %>%
  left_join(subset_df %>% select(timestamp, qflag, stage_filled), by = "timestamp", suffix = c("", "_updated")) %>%
  mutate(
    stage_filled = coalesce(stage_filled_updated, stage_filled),
    qflag = coalesce(qflag_updated, qflag)
  ) %>%
  select(-ends_with("_updated"))

# Step 8: Apply final qflag and qlevel logic 
final_df <- final_df %>%
  mutate(
    qflag = case_when(
      is.na(qflag) & !is.na(stage_filled) ~ "AV",
      TRUE ~ qflag
    ),
    qlevel = case_when(
      qflag == "AV" ~ 2,
      qflag %in% c("EV_LINEAR", "EV_SPLINE") ~ 3,
      qflag == "MV" ~ 2,
      TRUE ~ qlevel
    )
  )

# Step 9: Add year column for plotting
final_df <- final_df %>%
  mutate(year = format(timestamp, "%Y"))

# Step 10: Plot the results
ggplot(final_df, aes(x = timestamp)) +
  geom_line(aes(y = stage_filled), color = "black", size = 0.5, alpha = 0.8) +
  geom_point(aes(y = stage_filled, color = qflag, shape = qflag), size = 3, alpha = 0.8) +
  scale_color_manual(values = c(
    "EV_SPLINE" = "purple",
    "EV_LINEAR" = "blue",
    "AV" = "grey70",
    "MV" = "red"
  )) +
  scale_shape_manual(values = c(
    "EV_SPLINE" = 2,
    "EV_LINEAR" = 1,
    "AV" = 16,
    "MV" = 17
  )) +
  scale_x_datetime(date_labels = "%b %d", date_breaks = "2 days") +
  scale_y_continuous(name = "Water Level (m)") +
  labs(
    title = "Water Level QC Overview",
    subtitle = "Black = Final Line, Purple = Spline Fill, Blue = Linear Fill, Grey = AV, Red = MV (Long Gap)",
    x = "Date", y = "Water Level"
  ) +
  theme_minimal(base_size = 12) +
  facet_wrap(~year, scales = "free_x", ncol = 1)

# Create output df with only the required columns
output_df <- final_df %>%
  select(timestamp, qlevel, qflag, stage_filled)

