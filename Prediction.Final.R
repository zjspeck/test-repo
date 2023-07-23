##############################################################################

# first lets find out how many different models we can build
colnames(PitchSals)
possible.predictors <- colnames(PitchSals)[c(9,13,18,21)] # possible predictors

# generate all possible combinations of predictors
df.models <- NULL # here we store all possible models
model.count <- 1 # counter for model 

for(nr.predictors in 1:4){ # change number of predictors
  
  predictors <- combn(x = possible.predictors, m = nr.predictors) # generate all possible combinations of predictors
  
  for(combination in 1:ncol(predictors)){ # loop over every combination of predictors
    
    predictors.list <- paste0(predictors[,combination], collapse = "|") # all predictors
    formula <- paste0("Salarie_2015 ~ ",paste0(predictors[,combination], collapse = "+")) # model formula
    
    df.models <- rbind(df.models, c(model.count, nr.predictors, predictors.list, formula))
    model.count <- model.count + 1 # increase model count
  }
}

colnames(df.models) <- c("id", "nr. predictors", "predictors", "formula") # column names


#convert data in data frame and cread a numeric id column. 
df.models <- df.models %>% 
  as.data.frame() %>% # convert to data frame
  mutate(id = as.numeric(as.character(id))) # convert to numeric

# Generate all possible models (with function)
#PitchSals.Model <- generate.models(predictors.vars = possible.predictors, outcome.var = "Salarie_2015")

# Split data frame train ~ test
set.seed(123)
id.rows <- 1:nrow(PitchSals) # all rows ids
id.train <- sample(x = id.rows, size = round(0.8 * nrow(PitchSals)), replace = F) # train rows
id.test <- setdiff(id.rows, id.train) # test rows- removes the rows from the id.train

# write back to data frame; add a new col name sample
PitchSals[id.train,"sample"] <- "train"
PitchSals[id.test,"sample"] <- "test"
#check the test and train size
PitchSals%>% count(sample)
# Split sample (with function)
set.seed(123)
#diamonds.big <- split.sample(diamonds.big)
PitchSals.train <- PitchSals %>% filter(sample == "train")
PitchSals.test <- PitchSals %>% filter(sample == "test")

# Train models (train dataset), predict price (test dataset), calculate RMSE (test dataset)

df.models <- df.models %>% # add RMSE column
  mutate(RMSE = NA)

for(id.model in 1:nrow(df.models)){ # loop over each model
  
  # train model
  formula <- df.models[id.model,"formula"]
  lm.model <- lm(formula = formula, data = PitchSals.train)
  
  # predict price (test dataset)
  PitchSals.test <- PitchSals.test %>% 
    mutate(`Salarie_2015 Predicted` = predict(lm.model, .)) 
  
  # calculate RMSE (predicted price VS actual price)
  SSE <- (PitchSals.test$`price predicted` - PitchSals.test$price)^2 # sum of squared errors
  RMSE <- sqrt(mean(SSE))
  
  # write RMSE back to table
  df.models[id.model,"RMSE"] <- RMSE
}
#Small RMSE indicates a better prediction
# draw model performance
predictors.levels <- df.models %>% arrange(id) %>% pull(predictors) # levels for predictors factor variable

df.models %>% 
  mutate(`nr. predictors` = as.factor(as.character(`nr. predictors`)),
         predictors = factor(predictors, levels = predictors.levels)) %>% 
  ggplot(aes(x = predictors, y = RMSE, fill = `nr. predictors`)) +
  geom_bar(stat = "identity", color = "black") +
  scale_y_continuous(breaks = seq(0, 5000, 500)) +
  scale_fill_viridis_d() +
  xlab("Predictors") +
  ylab("RMSE") +
  ggtitle("Price prediction model performance") +
  theme(axis.text = element_text(size = 12),
        axis.text.x = element_text(angle = 90, hjust = 1),
        axis.title = element_text(size = 16),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5))

