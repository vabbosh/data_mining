#### Read the data into a dataframe ####

BCancer_Wis <- read.table("./Data/breast-cancer-wisconsin.data", sep=",")

colnames(BCancer_Wis) <- c("Sample.code.number",
                           "Clump.Thickness",
                           "Uniformity.of.Cell.Size",
                           "Uniformity.of.Cell.Shape",
                           "Marginal.Adhesion",
                           "Single.Epithelial.Cell.Size",
                           "Bare.Nuclei",
                           "Bland.Chromatin",
                           "Normal.Nucleoli",
                           "Mitoses",
                           "Class")

#### Pre-process data #####

#find which col are factors
factors = names(Filter(is.factor, BCancer_Wis))
#change type to int. This will change any "?" value to NA
BCancer_Wis[,factors] <- as.integer(as.character(BCancer_Wis[,factors]))
# Remove incomplete rows - rows with NA fields
BCancer_Wis <- BCancer_Wis[complete.cases(BCancer_Wis),]

# drop Sample.code.number
BCancer_Wis <- subset(BCancer_Wis, select = -c(Sample.code.number))

#change Class to factor type
BCancer_Wis$Class <- as.factor(BCancer_Wis$Class)

#save
saveRDS(BCancer_Wis, "./Data/bcw_processed.Rda")
