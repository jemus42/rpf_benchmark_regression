if (FALSE) {
  # Packages required but not automatically picked up by renv
  requireNamespace("xgboost")
  requireNamespace("randomPlantedForest")
  requireNamespace("ranger")
  requireNamespace("callr") # For encapsulation
}

if (conf$tuning$tuner == "mbo") {
  library(mlr3mbo)

  if (FALSE) {
    requireNamespace("DiceKriging")
    requireNamespace("rgenoud")
  }
}

wrap_autotuner <- function(learner_id, ..., search_space, .encode = FALSE) {
  paradox::assert_param_set(search_space)

  base_learner <- lrn(learner_id, ...)

  if (.encode) {
    base_learner <- po("encode", method = "treatment") %>>%
      po("removeconstants") %>>%
      base_learner |>
      as_learner()
  }

  # Used for XGBoost learners to enable internal tuning / early stopping using test set
  # Disabled due to bug for now
  # if ("validation" %in% base_learner$properties) {
  #   cli::cli_alert_info("Setting validation for {.val {learner_id}}")
  #   set_validate(base_learner, "test")
  # }


  if (conf$fallback$inner) {
    if (packageVersion("mlr3") >= "0.21.0") {
      base_learner$encapsulate("evaluate", lrn("regr.featureless"))
    } else {
      base_learner$fallback = lrn("regr.featureless")
      base_learner$encapsulate = c(train = "evaluate", predict = "evaluate")
    }
  }

  base_learner$timeout = c(
    train   = conf$timeout$base$train  * 3600,
    predict = conf$timeout$base$predict * 3600
  )

  at <- auto_tuner(
    learner = base_learner,
    resampling = switch(
      conf$resampling$inner$strategy,
      "holdout"     = rsmp("holdout"),
      "cv"          = rsmp("cv", folds = conf$resampling$inner$folds),
      "repeated_cv" = rsmp("repeated_cv",
                           folds = conf$resampling$inner$folds,
                           repeats = conf$resampling$inner$repeats),
    ),
    measure = msr("regr.mse"),
    search_space = search_space,
    # Terminate after evals XOR time limit
    terminator = trm("combo", list(
      trm("run_time", secs = conf$tuning$runtime),
      trm("evals", n_evals = conf$tuning$evals, k = conf$tuning$multiplier)
    ), any = TRUE),
    tuner = tnr(conf$tuning$tuner),
    store_tuning_instance = TRUE,
    store_benchmark_result = FALSE,
    store_models = FALSE
  )

  if (conf$fallback$outer) {
    if (packageVersion("mlr3") >= "0.21.0") {
      at$encapsulate("evaluate", lrn("regr.featureless"))
    } else {
      at$fallback = lrn("regr.featureless")
      at$encapsulate = c(train = "evaluate", predict = "evaluate")
    }
  }

  # Kill switch for long-running jobs
  at$timeout = c(
    train   = conf$timeout$autotuner$train  * 3600,
    predict = conf$timeout$autotuner$predict * 3600
  )

  at

}

learners <- list(

  rpf = wrap_autotuner(
    learner_id = "regr.rpf",
    nthreads = conf$learner_threads,
    ntrees = 50,
    max_interaction_limit = 20,
    search_space = ps(
      max_interaction_ratio = p_dbl(0, 1),
      splits    = p_int(10, 200),
      split_try = p_int(1, 20),
      t_try     = p_dbl(0.1, 1)
    )
  )

  ,

  rpf_fixdepth = wrap_autotuner(
    learner_id = "regr.rpf",
    nthreads = conf$learner_threads,
    ntrees = 50,
    max_interaction = 2,
    search_space = ps(
      splits    = p_int(10, 200),
      split_try = p_int(1, 20),
      t_try     = p_dbl(0.1, 1)
    )
  )

  ,

  xgb = wrap_autotuner(
    learner_id = "regr.xgboost",
    nthread = conf$learner_threads,
    # early_stopping_rounds = 50,
    # eval_metric = "rmse",
    .encode = TRUE,
    search_space = ps(
      regr.xgboost.max_depth        = p_int(1, 20),
      regr.xgboost.subsample        = p_dbl(0.1, 1),
      regr.xgboost.colsample_bytree = p_dbl(0.1, 1),
      regr.xgboost.eta              = p_dbl(1e-4, 1, logscale = TRUE),
      regr.xgboost.nrounds          = p_int(lower = 10, upper = 5000)
      # regr.xgboost.nrounds          = p_int(upper = 5000, tags = "internal_tuning",
      #                                       aggr = function(x) as.integer(mean(unlist(x))))
    )
  )

  ,

  xgb_fixdepth = wrap_autotuner(
    learner_id = "regr.xgboost",
    nthread = conf$learner_threads,
    max_depth = 2,
    # early_stopping_rounds = 50,
    # eval_metric = "rmse",
    .encode = TRUE,
    search_space = ps(
      regr.xgboost.subsample        = p_dbl(0.1, 1),
      regr.xgboost.colsample_bytree = p_dbl(0.1, 1),
      regr.xgboost.eta              = p_dbl(1e-4, 1, logscale = TRUE),
      regr.xgboost.nrounds          = p_int(lower = 10, upper = 5000)
      # regr.xgboost.nrounds          = p_int(upper = 5000, tags = "internal_tuning",
      #                                       aggr = function(x) as.integer(mean(unlist(x))))
    )
  )

  ,

  ranger = wrap_autotuner(
    learner_id = "regr.ranger",
    num.threads = conf$learner_threads,
    num.trees = 500,
    search_space = ps(
      mtry.ratio      = p_dbl(0.1, 1),
      min.node.size   = p_int(1, 50),
      sample.fraction = p_dbl(0.1, 1)
      # replace = p_lgl()
    )
  )

  ,

  featureless = lrn("regr.featureless")
)

# Use list names for learner ids for convenience downstream
mlr3misc::imap(learners, function(l, id) l$id = id)

