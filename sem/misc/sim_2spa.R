# WIP
# adapted from https://osf.io/dxtjb "2spa_with_mirt_and_OpenMx.R"

library(lavaan)
library(mirt)
library(OpenMx)

Sys.setenv(OMP_NUM_THREADS = parallel::detectCores())
mxOption(key = 'Number of Threads', value = parallel::detectCores())

# Simulate data
test_data = lavaan::simulateData(
  model = "
  factorScore1 =~ x1 + 0.9*x2 + 1.1*x3
  factorScore2 =~ x4 + 0.8*x5 + 1.2*x6
  factorScore3 =~ y1 + 0.7*y2 + 1.3*y3",
  sample.nobs = 1000
)

# Factor score estimation for factorScore1
factorScore1_prior = 'F = 1-3
             PRIOR = (1-3, a1, norm, 0, 5)
             START = (1-3, a1, 1)'
factorScore1_irtfit = mirt(test_data[, c("x1", "x2", "x3")],
                   model = factorScore1_prior,  # with prior
                   itemtype = "graded",  # graded response model
                   technical = list(NCYCLES = 1e5),  # increase no. of iterations
                   TOL = 1e-5)  # reduce tolerance
factorScore1_fs = fscores(factorScore1_irtfit, 
                  full.scores.SE = TRUE)  # EAP scores with standard error

# Factor score estimation for factorScore2
factorScore2_prior = 'F = 1-3
             PRIOR = (1-3, a1, norm, 0, 5)
             START = (1-3, a1, 1)'
factorScore2_irtfit = mirt(test_data[, c("x4", "x5", "x6")],
                   model = factorScore2_prior,  # with prior
                   itemtype = "graded",  # graded response model
                   technical = list(NCYCLES = 1e5),  # increase no. of iterations
                   TOL = 1e-5)  # reduce tolerance
factorScore2_fs = fscores(factorScore2_irtfit, 
                  full.scores.SE = TRUE)  # EAP scores with standard error

# Factor score estimation for factorScore3
factorScore3_prior = 'F = 1-3
             PRIOR = (1-3, a1, norm, 0, 5)
             START = (1-3, a1, 1)'
factorScore3_irtfit = mirt(test_data[, c("y1", "y2", "y3")],
                   model = factorScore3_prior,  # with prior
                   itemtype = "graded",  # graded response model
                   technical = list(NCYCLES = 1e5),  # increase no. of iterations
                   TOL = 1e-5)  # reduce tolerance
factorScore3_fs = fscores(factorScore3_irtfit, 
                  full.scores.SE = TRUE)  # EAP scores with standard error

# Combine the factor scores
test_data[, c("factorScore1", "factorScore1_SE")] = factorScore1_fs
test_data[, c("factorScore2", "factorScore2_SE")] = factorScore2_fs
test_data[, c("factorScore3", "factorScore3_SE")] = factorScore3_fs
# Compute the error variance and loading constraints
test_data = within(test_data, {
  errVar_factorScore1 = factorScore1_SE^2 * (1 - factorScore1_SE^2)
  loadConstr_factorScore1 = 1 - factorScore1_SE^2
  errVar_factorScore2 = factorScore2_SE^2 * (1 - factorScore2_SE^2)
  loadConstr_factorScore2 = 1 - factorScore2_SE^2
  errVar_factorScore3 = factorScore3_SE^2 * (1 - factorScore3_SE^2)
  loadConstr_factorScore3 = 1 - factorScore3_SE^2
})

# Step 2: OpenMx with definition variable
fsreg_mx = mxModel(
  "Test Model", 
  type = "RAM",
  mxData(observed = test_data, type = "raw"), 
  manifestVars = c("factorScore1", "factorScore2", "factorScore3"),
  latentVars = c("eta1", "eta2", "eta3"), 
  mxPath(from = "eta1", to = "factorScore1", 
         free = FALSE, labels = c("data.loadConstr_factorScore1")), 
  mxPath(from = "eta2", to = "factorScore2", 
         free = FALSE, labels = c("data.loadConstr_factorScore2")), 
  mxPath(from = "eta3", to = "factorScore3", 
         free = FALSE, labels = c("data.loadConstr_factorScore3")), 
  mxPath(from = c("eta1", "eta2"), to = "eta3", 
         values = c(.5, .5), labels = c("b1", "b2")),
  mxPath(from = c("eta3", "eta2", "eta1"), arrows = 2, 
         free = c(TRUE, FALSE, FALSE), 
         values = c(0.75, 1, 1), 
         labels = c("errVar_eta3", "Var_eta2", "Var_eta1")),
  mxPath(from = c("factorScore3", "factorScore2", "factorScore1"), 
         arrows = 2, values = 1, 
         free = FALSE, 
         labels = c("data.errVar_factorScore3", 
                    "data.errVar_factorScore2", 
                    "data.errVar_factorScore1")), 
  mxPath(from = "one", to = c("factorScore1", "factorScore2", "factorScore3"), 
         values = 0, labels = c("int1", "int2", "int3"))
  )
# Run OpenMx
test_fit = mxRun(fsreg_mx, intervals = TRUE)
# Summarize the results
summary(test_fit)
