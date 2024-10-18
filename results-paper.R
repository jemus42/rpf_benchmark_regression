library(ggplot2)
library(ragg)
library(patchwork)
library(data.table)
library(dplyr)
library(kableExtra)
conf <- config::get(config = "production_v1")
set.seed(3) # Only for jitterdodge consistency in plots

output_path <- here::here("paper-results")
if (!fs::dir_exists(output_path)) fs::dir_create(output_path)

# Load task metadata
task_meta <- readRDS("task_meta.rds")

task_meta <- task_meta |>
  arrange(dim_rank) |>
  mutate(
    task_label = glue::glue("{task_id} ({n} x {p})"),
    task_label = factor(task_label, levels = task_label, ordered = TRUE),
    task_id = factor(task_id, levels = task_id, ordered = TRUE)
  )

# benchmark results
aggr   <- readRDS(fs::path(conf$result_path, "aggr",   ext = "rds"))
scores <- readRDS(fs::path(conf$result_path, "scores", ext = "rds"))
aggr   <- task_meta[aggr, on = c("task_id")]
scores <- task_meta[scores, on = c("task_id")]

# Define learner colors for somewhat identifiable plots
learner_cols <- c(
  "rpf" = "#F73098",
  "xgb" = "#3BA99C",
  "rpf_fixdepth" = "#CA1694",
  "xgb_fixdepth" = "#256A62",
  "ranger" = "#2171B5",
  "featureless" = "#484848"
)
learner_order <- names(learner_cols)
learner_label <- function(x) {
  x <- as.character(x)
  ret <- c("rpf" = "rpf", "xgb" = "xgboost", "rpf_fixdepth" = "rpf (2)", "xgb_fixdepth" = "xgboost (2)", "ranger" = "rf", "featureless" = "average")[x]
  unname(ret)
}
learner_order_labelled <- learner_label(learner_order)

aggr[, learner_label := learner_label(learner_id)]
scores[, learner_label := learner_label(learner_id)]
aggr[, learner_id := factor(learner_id, levels = rev(learner_order))]
scores[, learner_id := factor(learner_id, levels = rev(learner_order))]

# Subset to tasks actually used
tasks_keep <- as.character(task_meta[dim_rank <= 12, task_id])
task_meta <- task_meta[task_id %in% tasks_keep, ]
scores <- scores[task_id %in% tasks_keep, ]
aggr <- aggr[task_id %in% tasks_keep, ]

# Table: Tasks --------------------------------------------------------------------------------

task_meta |>
  arrange(dim) |>
  select(task_id, n, p) |>
  kbl(
    caption = "Selected OpenML CTR-23 tasks, arranged by dimensionality",
    col.names = c("Task", "n", "p"),
    booktabs = TRUE, format = "latex"
  ) |>
  kable_styling() |>
  writeLines(con = fs::path(output_path, "tasks.tex"))


# Plot: Aggregated ----------------------------------------------------------------------------

tmp_aggr <- aggr |>
  tidyr::pivot_longer(
    cols = any_of(c("rmse", "mae", "rrse")),
    names_to = "measure",
    values_to = "score",
    names_transform = toupper
  ) |>
  filter(!(measure == "RRSE" & learner_id == "featureless"))

p_aggr_rmse <- tmp_aggr |>
  filter(measure == "RMSE") |>
  ggplot(aes(y = learner_id, x = score, fill = learner_id)) +
  geom_boxplot(alpha = .5) +
  geom_point(
    position = position_jitterdodge(dodge.width = .5),
    shape = 21, stroke = .1, size = 2
  ) +
  scale_x_log10(guide = guide_axis(position = "top"), labels = scales::label_comma(accuracy = 0.01)) +
  scale_y_discrete(labels = learner_label) +
  scale_fill_manual(values = learner_cols, guide = "none") +
  labs(
    #title = "Aggregated scores over all tasks",
    x = "RMSE (log10)", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot"
  )

p_aggr_mae <- tmp_aggr |>
  filter(measure == "MAE") |>
  ggplot(aes(y = learner_id, x = score, fill = learner_id)) +
  geom_boxplot(alpha = .5) +
  geom_point(
    position = position_jitterdodge(dodge.width = .5),
    shape = 21, stroke = .1, size = 2
  ) +
  scale_x_log10(guide = guide_axis(position = "top"), labels = scales::label_comma(accuracy = 0.01)) +
  scale_y_discrete(labels = learner_label) +
  scale_fill_manual(values = learner_cols, guide = "none") +
  labs(
    #title = "Aggregated scores over all tasks",
    x = "MAE (log10)", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot"
  )


p_aggr_rrse <- tmp_aggr |>
  filter(measure == "RRSE") |>
  ggplot(aes(y = learner_id, x = score, fill = learner_id)) +
  geom_boxplot(alpha = .5) +
  geom_point(
    position = position_jitterdodge(dodge.width = .5),
    shape = 21, stroke = .1, size = 2
  ) +
  scale_x_continuous(
    guide = guide_axis(position = "top"),
    labels = scales::label_percent(accuracy = 1),
    limits = c(0, 1)
  ) +
  scale_y_discrete(labels = learner_label) +
  scale_fill_manual(values = learner_cols, guide = "none") +
  labs(
    #title = "Aggregated scores over all tasks",
    x = "RRSE (%)", y = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title.position = "plot"
  )

ggsave(
  filename = fs::path(output_path, "aggregated-hybrid-rmse-mae-1.pdf"),
  plot = p_aggr_rmse / p_aggr_mae,
  width = 7, height = 4
)

ggsave(
  filename = fs::path(output_path, "aggregated-rrse-1.pdf"),
  plot = p_aggr_rrse,
  width = 7, height = 1.75
)

# Table: Aggregated ---------------------------------------------------------------------------

aggr_tab <- aggr |>
  mutate(
    # By construction RRSE is 100 for featureless
    # correcting numeric inaccuracy
    rrse = if_else(learner_id == "featureless", 100, 100 * rrse)
  ) |>
  tidyr::pivot_longer(
    cols = any_of(c("rmse", "mae", "rrse")),
    names_to = "measure", names_transform = toupper,
    values_to = "score"
  ) |>
  group_by(learner_id, measure) |>
  summarize(
    median = median(score, na.rm = TRUE),
    q25 = quantile(score, .25, na.rm = TRUE),
    q75 = quantile(score, .75, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 2))) |>
  mutate(
    median = glue::glue("{median} [{q25}; {q75}]")
  ) |>
  select(-q25, -q75) |>
  tidyr::pivot_wider(names_from = measure, names_sep = "_", values_from = median)

# Table with all 3 measures
aggr_tab |>
  select(learner_id, contains(toupper(c("rmse", "mae")))) |>
  arrange(desc(learner_id)) |>
  mutate(learner_id = learner_label(learner_id)) |>
  kbl(
    col.names = c("Learner", "RMSE", "MAE"),
    caption = "Median [q25, q75] of scores over all datasets and outer resampling iterations",
    booktabs = TRUE,
    format = "latex"
  ) |>
  kable_styling(latex_options = c("hold_position")) |>
  writeLines(con = fs::path(output_path, "aggr-rmse-mae.tex"))

# Table with RRSE only, omitting featureless learner
aggr_tab |>
  select(learner_id, RRSE) |>
  filter(learner_id != "featureless") |>
  arrange(desc(learner_id)) |>
  mutate(learner_id = learner_label(learner_id)) |>
  kbl(
    col.names = c("Learner", "RRSE (%)"),
    caption = "Median [q25, q75] of RRSE over all datasets and outer resampling iterations",
    booktabs = TRUE,
    format = "latex"
  ) |>
  kable_styling(latex_options = c("hold_position")) |>
  writeLines(con = fs::path(output_path, "aggr-rrse.tex"))


# Per-task boxplots ---------------------------------------------------------------------------

#' Plot scores per task, consistently
#' @param xdf Data frame `scores`, with columns `task_id`, `learner_id`, `rmse`, `mae`, `rrse` and multiple iterations
#' @param measure One of `"rmse"`, `"mae"`, `"rrse"`.
#' @param include_featless Include featureless learner in plot.
#' @param logscale Use log10 scale for x-axis.
#' @param ncol Number of columns in `ggplot2::facet_wrap`.
plot_scores <- function(xdf, measure = "rmse", include_featless = TRUE, logscale = FALSE, ncol = 3) {
  xdf <- xdf |>
    tidyr::pivot_longer(
      cols = any_of(c("rmse", "mae", "rrse")),
      names_to = "measure", names_transform = toupper,
      values_to = "score"
    ) |>
    filter(.data$measure == toupper(.env$measure))

  if (!include_featless) {
    xdf <- xdf |>
      filter(learner_id != "featureless")
  }

  if (measure == "rrse") {
    xdf <- xdf |>
      filter(learner_id != "featureless") |>
      mutate(score = 100 * score)

    measure <- "RRSE (%)"
  }

  p <- xdf |>
    mutate(
      task_label = stringr::str_replace(task_label, "\\s\\(", "\\\n("),
      task_label = reorder(task_label, n*p)
      #task_label = stringr::str_wrap(task_label, width = 20, whitespace_only = FALSE)
    ) |>
    ggplot(aes(y = learner_id, x = score, fill = learner_id, color = learner_id)) +
    facet_wrap(vars(task_label), ncol = ncol, scales = "free") +
    geom_boxplot(alpha = .5) +
    geom_jitter(shape = 21, stroke = .5, alpha = .15, size = 1) +
    scale_fill_manual(values = learner_cols, guide = "none", aesthetics = c("color", "fill")) +
    scale_y_discrete(labels = learner_label) +
    labs(
      # title = "Scores per task",
      # subtitle = "Tasks ordered by n * p, decreasing",
      y = NULL, x = toupper(measure)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      panel.spacing.x = unit(1, "cm"),
      plot.title.position = "plot"
    )

  if (logscale) {
    p + scale_x_log10(labels = scales::label_comma(accuracy = 0.01))
  } else {
    p + scale_x_continuous(labels = scales::label_comma())
  }
}


p_rmse <- plot_scores(scores, "rmse", include_featless = TRUE, logscale = FALSE, ncol = 4)
p_mae <- plot_scores(scores, "mae", include_featless = TRUE, logscale = FALSE, ncol = 4)
p_rrse <- plot_scores(scores, "rrse", include_featless = FALSE, logscale = FALSE, ncol = 4)

ggsave(
  filename = fs::path(output_path, "per-task-rmse-1.pdf"),
  plot = p_rmse,
  width = 13, height = 6
)

ggsave(
  filename = fs::path(output_path, "per-task-mae-1.pdf"),
  plot = p_mae,
  width = 13, height = 6
)

ggsave(
  filename = fs::path(output_path, "per-task-rrse-no-featless-1.pdf"),
  plot = p_rrse,
  width = 13, height = 6
)
