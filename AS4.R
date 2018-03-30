library(stringr)

# read data
energy <- read.csv("Desktop/AS4/DATA/calData.csv", header = T)

# create subset by state
cal.indices <- which(energy$State == "California")
cal.data <- energy[cal.indices,]
mic.indices <- which(energy$State == "Michigan")
mic.data <- energy[mic.indices,]
flo.indices <- which(energy$State == "Florida")
flo.data <- energy[flo.indices,]

# fix colnames
colnames <- colnames(cal.data)
colnames <- str_replace_all(colnames, "[.]", "")
colnames(cal.data) <- colnames
colnames(mic.data) <- colnames
colnames(flo.data) <- colnames

# merge datasets
cal.data <- rbind(cal.data, mic.data, flo.data)

# write csv
write.csv(cal.data, "triData.csv")

# Get correlation plot
install.packages("ggcorrplot")
library(ggcorrplot)
cal.data2 <- cal.data[,-c(84, 85)]
corr <- data.frame(cor(cal.data2))
