library(tidyverse)
library(doParallel)
library(randomForest)
library(ggthemes)
library(data.table)

setwd('C:/Users/cpicard/Documents/double_step_model/lab_data/7_transitions_datasetCreation')


# Load the data
#df <- read.csv(file = 'lab_data_transitions_0_2.csv', header = TRUE) %>% 
#  dplyr::select(-axiv_index_b, -axiv_index_t, -X.1)

df <- fread("lab_data_transitions_0_2.csv") %>% 
  dplyr::select(-axiv_index_b, -axiv_index_t, -V1)

df$lim_activ_0_2 <- as.factor(df$lim_activ_0_2)


# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

res <- list()


# Fitting a model (removing 1 person each time)
for (i in unique(df$subject)) {
  
  cat('Testing on:', i, " ")
  t <- proc.time()
  
  
  dat <- rbind(df %>% dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_0_2 == 1) %>% 
                 dplyr::select(-subject, -video_time, -X),
               ((df %>% dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_0_2 == 0) %>% 
                 dplyr::select(-subject, -video_time, -X)) [sample(nrow(df %>% 
                 dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_0_2 == 0)), 30000), ]))
  
  
  
  model.rf <- foreach(ntree = rep(29, 7), .combine = combine, .multicombine = TRUE, 
                      .packages="randomForest") %dopar% 
    randomForest(lim_activ_0_2 ~ .,
                 data = dat,
                 ntree = ntree,
                 mtry = 2,
                 importance = TRUE,
                 trim = TRUE,
                 returnData = FALSE,
    )
  
  pred <- dplyr::filter(df, subject == i)
  res[[i]] <- data.frame(subject = i, X = pred$X, obs = pred$lim_activ_0_2, pred = predict(model.rf, pred), 
                         predict(model.rf, dplyr::filter(df, subject == i), type = "prob"))
  
  cat('(finished in: ', round((proc.time() - t)[[3]]/60, 2), " min)\n", sep = "")
}

stopCluster(cl)


# Joining all together
res <- bind_rows(res)
#caret::confusionMatrix(res$obs, res$pred)

setwd('C:/Users/cpicard/Documents/double_step_model/lab_data/8_prediction_transitions')

write.csv(x = res, file = 'pred_transitions_0_2.csv', row.names = TRUE)







rm(list=ls())







setwd('C:/Users/cpicard/Documents/double_step_model/lab_data/7_transitions_datasetCreation')


# Load the data
#df <- read.csv(file = 'lab_data_transitions_2_5.csv', header = TRUE) %>% 
#  dplyr::select(-axiv_index_b, -axiv_index_t, -X.1)

df <- fread("lab_data_transitions_2_5.csv") %>% 
  dplyr::select(-axiv_index_b, -axiv_index_t, -V1)

df$lim_activ_2_5 <- as.factor(df$lim_activ_2_5)


# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

res <- list()


# Fitting a model (removing 1 person each time)
for (i in unique(df$subject)) {
  
  cat('Testing on:', i, " ")
  t <- proc.time()
  
  dat <- rbind(df %>% dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_2_5 == 1) %>% 
                 dplyr::select(-subject, -video_time, -X),
               ((df %>% dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_2_5 == 0) %>% 
                   dplyr::select(-subject, -video_time, -X)) [sample(nrow(df %>% 
                   dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_2_5 == 0)), 30000), ]))
  
  
  
  model.rf <- foreach(ntree = rep(29, 7), .combine = combine, .multicombine = TRUE, 
                      .packages="randomForest") %dopar% 
    randomForest(lim_activ_2_5 ~ .,
                 data = dat,
                 ntree = ntree,
                 mtry = 2,
                 importance = TRUE,
                 trim = TRUE,
                 returnData = FALSE,
    )
  
  pred <- dplyr::filter(df, subject == i)
  res[[i]] <- data.frame(subject = i, X = pred$X, obs = pred$lim_activ_2_5, pred = predict(model.rf, pred),
                         predict(model.rf, dplyr::filter(df, subject == i), type = "prob"))
  
  cat('(finished in: ', round((proc.time() - t)[[3]]/60, 2), " min)\n", sep = "")
}

stopCluster(cl)


# Joining all together
res <- bind_rows(res)
#caret::confusionMatrix(res$obs, res$pred)

setwd('C:/Users/cpicard/Documents/double_step_model/lab_data/8_prediction_transitions')

write.csv(x = res, file = 'pred_transitions_2_5.csv', row.names = TRUE)








rm(list=ls())








setwd('C:/Users/cpicard/Documents/double_step_model/lab_data/7_transitions_datasetCreation')


# Load the data
#df <- read.csv(file = 'lab_data_transitions_5_15.csv', header = TRUE) %>% 
#  dplyr::select(-axiv_index_b, -axiv_index_t, -X.1)

df <- fread("lab_data_transitions_5_15.csv") %>% 
  dplyr::select(-axiv_index_b, -axiv_index_t, -V1)

df$lim_activ_5_15 <- as.factor(df$lim_activ_5_15)


# Set up parallel processing
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

res <- list()


# Fitting a model (removing 1 person each time)
for (i in unique(df$subject)) {
  
  cat('Testing on:', i, " ")
  t <- proc.time()
  
  dat <- rbind(df %>% dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_5_15 == 1) %>% 
                 dplyr::select(-subject, -video_time, -X),
               ((df %>% dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_5_15 == 0) %>% 
                   dplyr::select(-subject, -video_time, -X)) [sample(nrow(df %>% 
                   dplyr::filter(subject != i) %>% dplyr::filter(lim_activ_5_15 == 0)), 30000), ]))
  
  
  
  model.rf <- foreach(ntree = rep(29, 7), .combine = combine, .multicombine = TRUE, 
                      .packages="randomForest") %dopar% 
    randomForest(lim_activ_5_15 ~ .,
                 data = dat,
                 ntree = ntree,
                 mtry = 2,
                 importance = TRUE,
                 trim = TRUE,
                 returnData = FALSE,
    )
  
  pred <- dplyr::filter(df, subject == i)
  res[[i]] <- data.frame(subject = i, X = pred$X, obs = pred$lim_activ_5_15, pred = predict(model.rf, pred),
                         predict(model.rf, dplyr::filter(df, subject == i), type = "prob"))
  
  cat('(finished in: ', round((proc.time() - t)[[3]]/60, 2), " min)\n", sep = "")
}

stopCluster(cl)


# Joining all together
res <- bind_rows(res)
#caret::confusionMatrix(res$obs, res$pred)

setwd('C:/Users/cpicard/Documents/double_step_model/lab_data/8_prediction_transitions')

write.csv(x = res, file = 'pred_transitions_5_15.csv', row.names = TRUE)