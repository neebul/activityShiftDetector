library(tidyverse)
library(doParallel)
library(randomForest)
library(ggthemes)
library(data.table)

setwd('your_working_directory')


# Load dataset
df <- fread("file_name.csv") %>% 
  dplyr::select(-axiv_index_b, -axiv_index_t)

df$transition_noTransition <- as.factor(df$transition_noTransition)


# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

res <- list()


# Fitting a random forest model (with a leave-one-out)
for (i in unique(df$subject)) {
  
  cat('Testing on:', i, " ")
  t <- proc.time()
  
  
  dat <- rbind(df %>% dplyr::filter(subject != i) %>% dplyr::filter(transition_noTransition == 1) %>% 
                 dplyr::select(-subject, -video_time, -X),
               ((df %>% dplyr::filter(subject != i) %>% dplyr::filter(transition_noTransition == 0) %>% 
                 dplyr::select(-subject, -video_time, -X)) [sample(nrow(df %>% 
                 dplyr::filter(subject != i) %>% dplyr::filter(transition_noTransition == 0)), 30000), ]))
                # Here we take 30000 values for the noTransition condition, considering that generally there are
                # much more moments of noTransition than transition. This value of 30000 can be changed.
  
  
  
  model.rf <- foreach(ntree = rep(29, 7), .combine = combine, .multicombine = TRUE, 
                      .packages="randomForest") %dopar% 
    randomForest(transition_noTransition ~ .,
                 data = dat,
                 ntree = ntree,
                 mtry = 2,
                 importance = TRUE,
                 trim = TRUE,
                 returnData = FALSE,
    )
  
  pred <- dplyr::filter(df, subject == i)
  res[[i]] <- data.frame(subject = i, X = pred$X, obs = pred$transition_noTransition, pred = predict(model.rf, pred), 
                         predict(model.rf, dplyr::filter(df, subject == i), type = "prob"))
  
  cat('(finished in: ', round((proc.time() - t)[[3]]/60, 2), " min)\n", sep = "")
}

stopCluster(cl)


# Joining all together
res <- bind_rows(res)


# Observing the results in a confusion matrix
caret::confusionMatrix(res$obs, res$pred)
