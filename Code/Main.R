# Install packages
# install.packages(c("semPLS", "DiagrammeR"))

# Import libraries
library(semPLS)
library(DiagrammeR)

# Import European Satisfaction Index (ECSI) and
# the measurement instrument for the mobile phone industry
help("ECSImobi")

# From-to-matrix: first column contains the tail and the second row is the head
# Structural model
data("ECSIsm")
View(ECSIsm)
# Measurement model: reflective (default)
data("ECSImm")
View(ECSImm)

# Dataset of the MVs
data("mobi")
View(mobi)

# Create a plsm object
ECSI <- plsm(data = mobi, strucmod = ECSIsm, measuremod = ECSImm)
help("plsm")

# mvplot(model = ECSI, data = mobi, LV = "Expectation")
mvpairs(model = ECSI, data = mobi, LV = "Image")

# sem pls
ecsi <- sempls(model = ECSI, data = mobi, wscheme = "centroid")
ecsi
names(ecsi)

# Create the diagram
# Structural model only
pathDiagram(ecsi, file = "ecsi-structure", full = FALSE, edge.labels = "both",
            output.type = "graphics", digits = 2, graphics.fmt = "pdf")
grViz("ecsi-structure.dot")

# Full model (structural and measurement)
pathDiagram(ecsi, file = "ecsi-fullstructure", full = TRUE, edge.labels = "both",
            output.type = "graphics", digits = 2, graphics.fmt = "pdf")
grViz("ecsi-fullstructure.dot")

# Model evaluation
# A. Measurement model
# 1. convergent validity
ecsi$outer_loadings
# 2. Indicator reliability
communality(ecsi)
# 3. Composite reliability
dgrho(ecsi)
# 4. Discriminant validity
plsLoadings(ecsi)

# B. Structural Model
# 1. Coefficient of determination
rSquared(ecsi)
# 2. Redundancy
redundancy(ecsi)
# 3. Goodness of fit
gof(ecsi)

# Path coefficients and total effects are extracted by pathCoeff and totalEffects
pC <- pathCoeff(ecsi)
print(pC, abbreviate = T, minlength = 3)

# Total effect
tE <- totalEffects(ecsi)
print(tE, abbreviate = T, minlength = 3)

# Outer weight
plsWeights(ecsi)

# Plot of the evolution of outer weights until convergence for all blocks of MVs
plot(ecsi)

# Kernel density estimates can provide hints on the adequacy of the model
densityplot(ecsi, use = "residuals")

# BOOTSTRAPPED SEMPLS
set.seed(123)
ecsiBoot <- bootsempls(ecsi, nboot = 500, start = "ones", verbose = F)
ecsiBoot

ecsiBootsummary <- summary(ecsiBoot, type = "bca", level = 0.9)
ecsiBootsummary

densityplot(ecsiBoot, patern = "beta")
parallelplot(ecsiBoot, pattern = "beta", reflinesAt = c(0, 0.5),
             alpha = 0.3, type = "bca", main = "Path Coefficients\nof 500 bootstrap samples")
