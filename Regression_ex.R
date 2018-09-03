# prepare the R environment
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,            # Data munging functions
  zoo,              # Feature engineering rolling aggregates
  data.table,       # Feature engineering
  ggplot2,          # Graphics
  scales,           # Time formatted axis
  readr,            # Reading input files
  stringr,          # String functions
  reshape2,         # restructure and aggregate data 
  randomForest,     # Random forests
  corrplot,         # correlation plots
  Metrics,          # Eval metrics for ML
  vcd               # Visualizing discrete distributions
)

# set options for plots
options(repr.plot.width=6, repr.plot.height=6)
# Load the matches data
googFile <-"C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\goog.csv"

if(!file.exists(googFile)){tryCatch(googFile)}
if(file.exists(googFile)) goog_original <- read.csv(googFile)
head(goog_original)
summary(goog_original)
googTable <-read.table(googFile,header = TRUE, sep ="," )[,c("Date","Adj.Close")]
head(googTable)
# generate an id column for future use (joins etc)
goog_original$goog_id = seq.int(nrow(goog_original))
head(goog_original)
summary(goog_original)

