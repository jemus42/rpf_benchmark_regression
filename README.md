# rpf_benchmark_regression

The empirical benchmark for `randomPlantedForest`.

To reproduce results, first start an R session in the project directory and run

```r
renv::restore()
```

To ensure all required R packages are available and using the expected versions.

## Structure

This project uses `batchtools` and the `mlr3` ecosystem to run the benchmark.
The following files are required to produce results:

- `config.yml`: Configuration file for the benchmark. By default, the `default` profile is loaded via `.Rprofile` automatically and the resulting `conf` list object is expected to be present in subsequent scripts to defined benchmark parameters.
  - For the final benchmark run, the `<config-profile>` is set to `production` via `./.Renviron`.
- `benchmark.R`: The main benchmark script, sourcing other scripts as needed.
  - This script does not yet submit any jobs, as this step is highly dependent on the available hardware and computing environment, e.g. whether jobs are run locally or on a HPC cluster.
  - The `batchtools` registry is created at `registries/<config-profile>`
- `learners.R`: Sets up `mlr3` learners including tuning spaces.
- `tasks.R`: Retrieves the datasets from OpenML and creates `mlr3` tasks
  - Writes `./task_meta.rds`, which is used in subsequent scripts to amend task metadata to other tables.
- `process-results.R`: Processes the `batchtools` registry and writes intermediate result files to `results/<config-profile>`
- `results-paper.R`: Produces plots and LaTeX tables and writes them to `./paper-results`

