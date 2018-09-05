# prepare the R environment
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  dplyr,         # Data munging functions
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
xomFile <- 'C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\xom.csv'
snpFile <-'C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\snp.csv'
xom <- preProcess(xomFile,snpFile)
names(xom)[2:3] <- c("xom.returns","snp.returns" )
xom_SM <- lm(xom$xom.returns~xom$snp.returns)
summary(xom_SM)

uso <- read.table('C:\\Users\\Mirela\\RStudioProjects\\Regression_file\\Regression_Data\\uso.csv',header = TRUE, sep =",")[,c("Date","Adj.Close")]
names(uso)[2]<-"uso.returns"
uso[,c("Date")] <- as.Date(uso[,c("Date")])
uso <- uso[order(uso$Date, decreasing = TRUE),]
uso[-nrow(uso),-1] <- uso[-nrow(uso),-1]/uso[-1,-1]-1
xom <- merge(xom, uso, by = "Date")

xom_MLR <- lm(xom$xom.returns~xom$snp.returns + xom$uso.returns)
summary(xom_MLR)
