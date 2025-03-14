##### internal meta-analysis ######

models = c(
  "model1US", "model2US", "model3US",
  "model1JA", "model2JA", "model3JA",
  "model1SG", "model2SG", "model3SG"
)

sesPathsAll = lapply(
  # get all models
  setNames(mget(models), models),
  # get std values
  function(X) lavaan::standardizedSolution(X) %>%
    dplyr::filter(op == "~" & grepl("ses", rhs)) %>%
    dplyr::select(lhs, rhs, est.std, se)
) %>%
  # compile
  dplyr::bind_rows(.id = "source") %>%
  # label appropriately
  dplyr::mutate(
    model = substr(source, 6, 6),
    sample = substr(source, 7, 8),
    source = NULL)

sesPathCombis =
  # create df that has all possible combinations
  expand.grid(
    model = c(1, 2, 3),
    lhs = c("stressPos", "stressNeg"),
    rhs = c("ses_lad", "ses_inc", "ses_edu"),
    stringsAsFactors = FALSE
  ) %>%
  # remove non-ladder from model 1
  dplyr::filter(!(model == 1 & rhs != "ses_lad"))

sesPathsMeta = lapply(
  1:nrow(sesPathCombis),
  function(X) {
    currCombi = sesPathCombis[X, ]
    metaResult = metafor::rma(
    yi = est.std,
    sei = se,
    data = sesPathsAll,
    subset = (
      lhs == currCombi$lhs &
        rhs == currCombi$rhs &
        model == currCombi$model),
    slab = sample
  )
    metaResult$combi = currCombi
    return(metaResult)
  }
)

sesPathsMetaSummary = lapply(sesPathsMeta, function(res) {
  # Combine `b`, `ci.lb`, `ci.ub`, and the columns of `combi`
  data.frame(
    b = res$b,
    ci.lb = res$ci.lb,
    ci.ub = res$ci.ub,
    # Convert the combi data frame to a list of columns
    as.list(res$combi),
    stringsAsFactors = FALSE
  )
}) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(
    b = round(b, 3),
    ci = paste0("[", round(ci.lb, 3), ", ", round(ci.ub, 3), "]"),
    .keep = "unused"
    ) %>%
  as.data.frame(row.names = 1:nrow(.)) %>%
  dplyr::select(lhs, rhs, model, b, ci)

sesPathsMetaSummary %>%
  # get only model 3
  # for SEM plotting purposes
  dplyr::filter(model == 3)
