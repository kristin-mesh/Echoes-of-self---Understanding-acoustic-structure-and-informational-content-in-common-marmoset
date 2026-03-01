# LIBRARIES + IMPORTING DATA----------------------------------------------------

library(emmeans)
library(cowplot)
library(ggplot2)
library(alookr)
library(dplyr)
library(randomForest)
library(caret)
library(caTools)
library(lmerTest)
library(lme4)
library(rstudioapi)
library(readxl)
library(ggResidpanel)

# directory of the currently running script

script_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

# relative path to the data file
file_path <- file.path(script_dir, "raw_data.xlsx")

# read .xlsx file in the project folder
raw_data <- read_excel(file_path)

# converting relevant columns to factors
rds_data <- raw_data %>%
  mutate(across(c(stage, file_type, ID, sex, element, el_type, el_no, date, file_number, linear, 
                  curve, shape, call_ref), as.factor))

# relative path to the data file + import into RStudio

rds_file_path <- file.path(script_dir, "rds_data.rds")
saveRDS(rds_data, rds_file_path)
bandpassed <- readRDS(rds_file_path)

# first filtering out all of Olympia's data from the table
# dropping unused levels, and in this case the "Olympia" level from "ID" factor 
# because of low sample size

bandpassed <- bandpassed %>% filter(!grepl("Olympia", ID)) 
bandpassed <- bandpassed %>% droplevels(bandpassed$ID)

# ID ~ EVERYTHING---------------------------------------------------------------  

# splitting the data set into train and test sets according to the ID variable

# configuring how the sample data will be split for the training and testing sets

sb <- sample.split(bandpassed$ID, SplitRatio = 0.7) 
train = bandpassed[sb, ]
test = bandpassed[!sb, ]
train$ID <- as.factor(train$ID)
test$ID <- as.factor(test$ID)

set.seed(70)

# configuring a grid of mtry values that will be tested during a test training
# ntree set to default 500 value for all models

mtryGrid <- expand.grid(mtry = c(1:15))
model_mtry <- train(ID ~ sex + avg_entropy + bw_90 + dur_90 + freq_5 + 
                      freq_95 + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                      `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                      `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                      `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                      `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                      `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                      `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                      `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                      `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                      `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                      `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                      `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                      `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                      `131` + `132` + `133`, data=train, 
                    method="rf", tuneGrid = mtryGrid, ntree = 500)

bestmtry <- model_mtry$bestTune$mtry # storing the best mtry value in a variable

rf_model <- randomForest(train[,c(9,13:19,22:155)], train$ID, method="rf", 
                         mtry = bestmtry, ntree=500, importance = TRUE)
print(rf_model) # to see the OOB error rate 

pred_train <- predict(rf_model, test)
confusionMatrix(pred_train, test$ID)

varImpPlot(rf_model) # to see most important variable

# COMPARING Bs - STRUCTURE------------------------------------------------------

tidy_Bs <- bandpassed %>% filter(el_type == "B") #creating data set with only Bs
tidy_Bs <- tidy_Bs %>% droplevels()
set.seed(2)
sb_B <- sample.split(tidy_Bs$el_no, SplitRatio = 0.7)
train_B = tidy_Bs[sb_B, ]
test_B = tidy_Bs[!sb_B, ]
test_B <- test_B %>% filter(!grepl("-1:0",shape))
# removing level as only 1 value present
train_B$ID <- as.factor(train_B$ID)
test_B$ID <- as.factor(test_B$ID)
train_B$el_no <- as.factor(train_B$el_no)
test_B$el_no <- as.factor(test_B$el_no)

model_mtry_B <- train(el_no ~ ID + sex + avg_entropy + bw_90 + dur_90 + freq_5 + 
                        freq_95 + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                        `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                        `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                        `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                        `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                        `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                        `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                        `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                        `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                        `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                        `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                        `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                        `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                        `131` + `132` + `133`, data=train_B, 
                      method="rf", tuneGrid = mtryGrid, ntree = 500)

bestmtry_B <- model_mtry_B$bestTune$mtry

set.seed(2)
rf_model_B <- randomForest(train_B[,c(8,9, 13:19,22:155)],train_B$el_no,method="rf", 
                           mtry = bestmtry_B, ntree=500, importance = TRUE)
print(rf_model_B)
 
test_B <- rbind(train_B[1,], test_B)
test_B <- test_B[-1,]

pred_train_B <- predict(rf_model_B, test_B)
confusionMatrix(pred_train_B, test_B$el_no)

varImpPlot(rf_model_B)

# COMPARING Cs - STRUCTURE ------------------------------------------------------ 

tidy_Cs <- bandpassed %>% filter(el_type == "C") #creating data set with only Cs
set.seed(3)
sb_C <- sample.split(tidy_Cs$el_no, SplitRatio = 0.7)
train_C = tidy_Cs[sb_C, ]
test_C = tidy_Cs[!sb_C, ]
train_C$el_no <- as.factor(train_C$el_no)
test_C$el_no <- as.factor(test_C$el_no)

model_mtry_C <- train(el_no ~ ID + sex + avg_entropy + bw_90 + dur_90 + freq_5 + 
                        freq_95 + freq_center + time_center + shape + 
                        `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                        `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                        `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                        `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                        `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                        `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                        `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                        `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                        `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                        `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                        `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                        `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                        `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                        `131` + `132` + `133`, 
                      data=train_C, method="rf", tuneGrid=mtryGrid, ntree = 500)

bestmtry_C <- model_mtry_C$bestTune$mtry

rf_model_C <- randomForest(train_C[,c(8,9,13:19,22:155)], train_C$el_no, method="rf", 
                           mtry = bestmtry_C, ntree=500, importance = TRUE)
print(rf_model_C)

pred_train_C <- predict(rf_model_C, test_C)
confusionMatrix(pred_train_C, test_C$el_no)

varImpPlot(rf_model_C)


# COMPARING FIRST PHEES (1s)----------------------------------------------------

tidy_1 <- bandpassed %>% filter(el_no == "1") #creating data set with only 1s
tidy_1 <- tidy_1 %>% droplevels()
set.seed(5)
sb_1 <- sample.split(tidy_1$el_type, SplitRatio = 0.7) 
train_1 = tidy_1[sb_1, ]
test_1 = tidy_1[!sb_1, ]
train_1$el_type <- as.factor(train_1$el_type)
test_1$el_type <- as.factor(test_1$el_type)

model_mtry_1 <- train(el_type ~ ID + sex + avg_entropy + bw_90 + dur_90 + freq_5+ 
                        freq_95 + freq_center + time_center + shape + 
                        `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                        `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                        `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                        `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                        `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                        `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                        `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                        `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                        `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                        `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                        `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                        `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                        `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                        `131` + `132` + `133`, 
                      data=train_1, method="rf", tuneGrid=mtryGrid, ntree = 500)

bestmtry_1 <- model_mtry_1$bestTune$mtry

rf_model_1 <- randomForest(train_1[,c(8,9,13:19,22:155)],train_1$el_type,method="rf", 
                           mtry = bestmtry_1, ntree=500, importance = TRUE)
print(rf_model_1)

pred_train_1 <- predict(rf_model_1, test_1)
confusionMatrix(pred_train_1, test_1$el_type)

varImpPlot(rf_model_1)


# COMPARING SECOND PHEES (2S)---------------------------------------------------

tidy_2 <- bandpassed %>% filter(el_no == "2") #creating data set with only 2s
tidy_2 <- tidy_2 %>% droplevels()
set.seed(6)
sb_2 <- sample.split(tidy_2$el_type, SplitRatio = 0.7) 
train_2 = tidy_2[sb_2, ]
test_2 = tidy_2[!sb_2, ]
train_2$el_type <- as.factor(train_2$el_type)
test_2$el_type <- as.factor(test_2$el_type)

model_mtry_2 <- train(el_type ~ ID + sex + avg_entropy + bw_90 + dur_90 + freq_5+ 
                        freq_95 + freq_center + time_center + shape + 
                        `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                        `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                        `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                        `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                        `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                        `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                        `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                        `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                        `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                        `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                        `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                        `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                        `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                        `131` + `132` + `133`, data=train_2, 
                      method="rf", tuneGrid=mtryGrid, ntree = 500)

bestmtry_2 <- model_mtry_2$bestTune$mtry

rf_model_2 <- randomForest(train_2[,c(8,9,13:19,22:155)],train_2$el_type,method="rf", 
                           mtry = bestmtry_2, ntree=500, importance = TRUE)
print(rf_model_2)

pred_train_2 <- predict(rf_model_2, test_2)
confusionMatrix(pred_train_2, test_2$el_type)

varImpPlot(rf_model_2)


# PREDICTING ID INFO CONTENT IN A1s---------------------------------------------

tidy_As <- bandpassed %>% filter(el_type == "A") #creating data set with only As
tidy_As <- tidy_As %>% droplevels()

set.seed(18)
sb_A <- sample.split(tidy_As$ID, SplitRatio = 0.7)
train_A = tidy_As[sb_A, ]
test_A = tidy_As[!sb_A, ]
train_A$ID <- as.factor(train_A$ID)
test_A$ID <- as.factor(test_A$ID)

model_mtry_A <- train(ID ~ sex + avg_entropy + bw_90 + dur_90 + freq_5 + freq_95
                      + freq_center + time_center + shape + 
                        `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                        `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                        `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                        `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                        `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                        `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                        `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                        `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                        `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                        `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                        `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                        `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                        `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                        `131` + `132` + `133`, data=train_A, 
                      method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_A <- model_mtry_A$bestTune$mtry

rf_model_A <- randomForest(train_A[,c(9,13:19,22:155)],train_A$ID,method="rf", 
                           mtry = bestmtry_A, ntree=500, importance = TRUE)
print(rf_model_A)

pred_train_A <- predict(rf_model_A, test_A)
confusionMatrix(pred_train_A, test_A$ID)

varImpPlot(rf_model_A)

# PREDICTING ID INFO CONTENT IN Bs----------------------------------------------

tidy_B_1 <- tidy_Bs %>% filter(el_no == "1") #creating data set with only 1s
# from B sequences 
tidy_B_1 <- tidy_B_1 %>% droplevels()
set.seed(7)
sb_B_1 <- sample.split(tidy_B_1$ID, SplitRatio = 0.7) 
train_B_1 = tidy_B_1[sb_B_1, ]
test_B_1 = tidy_B_1[!sb_B_1, ]
train_B_1$ID <- as.factor(train_B_1$ID)
test_B_1$ID <- as.factor(test_B_1$ID)

model_mtry_B_1 <- train(ID ~ sex + avg_entropy + bw_90 + dur_90 + freq_5+freq_95
                        + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                          `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                          `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                          `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                          `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                          `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                          `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                          `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                          `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                          `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                          `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                          `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                          `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                          `131`+ `132` + `133`, data=train_B_1, 
                        method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_B_1 <- model_mtry_B_1$bestTune$mtry

rf_model_B_1 <- randomForest(train_B_1[,c(9,13:19,22:155)],train_B_1$ID,method="rf", 
                             mtry = bestmtry_B_1, ntree=500, importance = TRUE)
print(rf_model_B_1)

pred_train_B_1 <- predict(rf_model_B_1, test_B_1)
confusionMatrix(pred_train_B_1, test_B_1$ID)

varImpPlot(rf_model_B_1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tidy_B_2 <- tidy_Bs %>% filter(el_no == "2") #creating data set with only 2s
# from B sequences 
tidy_B_2 <- tidy_B_2 %>% droplevels()
set.seed(8)
sb_B_2 <- sample.split(tidy_B_2$ID, SplitRatio = 0.7) 
train_B_2 = tidy_B_2[sb_B_2, ]
test_B_2 = tidy_B_2[!sb_B_2, ]
train_B_2$ID <- as.factor(train_B_2$ID)
test_B_2$ID <- as.factor(test_B_2$ID)

model_mtry_B_2 <- train(ID ~ sex + avg_entropy + bw_90 + dur_90 + freq_5+freq_95
                        + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                          `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                          `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                          `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                          `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                          `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                          `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                          `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                          `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                          `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                          `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                          `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                          `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                          `131`+ `132` + `133`, data=train_B_2, 
                        method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_B_2 <- model_mtry_B_2$bestTune$mtry

rf_model_B_2 <- randomForest(train_B_2[,c(9,13:19,22:155)],train_B_2$ID,method="rf", 
                             mtry = bestmtry_B_2, ntree=500, importance = TRUE)
print(rf_model_B_2)

pred_train_B_2 <- predict(rf_model_B_2, test_B_2)
confusionMatrix(pred_train_B_2, test_B_2$ID)

varImpPlot(rf_model_B_2)

# PREDICTING ID INFO CONTENT IN Cs----------------------------------------------

tidy_C_1 <- tidy_Cs %>% filter(el_no == "1") #creating data set with only 1s
# from C sequences 
tidy_C_1 <- tidy_C_1 %>% droplevels()
set.seed(9)
sb_C_1 <- sample.split(tidy_C_1$ID, SplitRatio = 0.7) 
train_C_1 = tidy_C_1[sb_C_1, ]
test_C_1 = tidy_C_1[!sb_C_1, ]
train_C_1$ID <- as.factor(train_C_1$ID)
test_C_1$ID <- as.factor(test_C_1$ID)

model_mtry_C_1 <- train(ID ~ sex + avg_entropy + bw_90 + dur_90 + freq_5+freq_95
                        + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                          `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                          `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                          `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                          `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                          `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                          `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                          `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                          `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                          `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                          `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                          `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                          `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                          `131`+ `132` + `133`, data=train_C_1, 
                        method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_C_1 <- model_mtry_C_1$bestTune$mtry

rf_model_C_1 <- randomForest(train_C_1[,c(9,13:19,22:155)],train_C_1$ID,method="rf", 
                             mtry = bestmtry_C_1, ntree=500, importance = TRUE)

print(rf_model_C_1)

pred_train_C_1 <- predict(rf_model_C_1, test_C_1)
confusionMatrix(pred_train_C_1, test_C_1$ID)

varImpPlot(rf_model_C_1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tidy_C_2 <- tidy_Cs %>% filter(el_no == "2") #creating data set with only 2s
# from C sequences 
tidy_C_2$element <- droplevels(tidy_C_2$element)
tidy_C_2$el_type <- droplevels(tidy_C_2$el_type)
tidy_C_2$el_no <- droplevels(tidy_C_2$el_no)
tidy_C_2$element <- droplevels(tidy_C_2$element)
tidy_C_2$el_type <- droplevels(tidy_C_2$el_type)
tidy_C_2$el_no <- droplevels(tidy_C_2$el_no)

set.seed(10)
sb_C_2 <- sample.split(tidy_C_2$ID, SplitRatio = 0.7) 
train_C_2 = tidy_C_2[sb_C_2, ]
test_C_2 = tidy_C_2[!sb_C_2, ]
train_C_2$ID <- as.factor(train_C_2$ID)
test_C_2$ID <- as.factor(test_C_2$ID)

model_mtry_C_2 <- train(ID ~ sex + avg_entropy + bw_90 + dur_90 + freq_5+freq_95
                        + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                          `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                          `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                          `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                          `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                          `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                          `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                          `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                          `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                          `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                          `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                          `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                          `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                          `131`+ `132` + `133`, data=train_C_2, 
                        method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_C_2 <- model_mtry_C_2$bestTune$mtry

rf_model_C_2 <- randomForest(train_C_2[,c(9,13:19,22:155)],train_C_2$ID,method="rf", 
                             mtry = bestmtry_C_2, ntree=500, importance = TRUE)

print(rf_model_C_2)

pred_train_C_2 <- predict(rf_model_C_2, test_C_2)
confusionMatrix(pred_train_C_2, test_C_2$ID)

varImpPlot(rf_model_C_2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tidy_C_3 <- tidy_Cs %>% filter(el_no == "3") #creating data set with only 3s
# from C sequences 
tidy_C_3 <- tidy_C_3 %>% droplevels()

set.seed(11)
sb_C_3 <- sample.split(tidy_C_3$ID, SplitRatio = 0.7) 
train_C_3 = tidy_C_3[sb_C_3, ]
test_C_3 = tidy_C_3[!sb_C_3, ]
train_C_3$ID <- as.factor(train_C_3$ID)
test_C_3$ID <- as.factor(test_C_3$ID)

model_mtry_C_3 <- train(ID ~ sex + avg_entropy + bw_90 + dur_90 + freq_5+freq_95
                        + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                          `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                          `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                          `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                          `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                          `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                          `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                          `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                          `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                          `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                          `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                          `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                          `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                          `131`+ `132` + `133`, data=train_C_3, 
                        method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_C_3 <- model_mtry_C_3$bestTune$mtry

rf_model_C_3 <- randomForest(train_C_3[,c(9,13:19,22:155)],train_C_3$ID,method="rf", 
                             mtry=bestmtry_C_3, ntree=500, importance = TRUE)

print(rf_model_C_3)

set.seed(11)
pred_train_C_3 <- predict(rf_model_C_3, test_C_3)
confusionMatrix(pred_train_C_3, test_C_3$ID)

varImpPlot(rf_model_C_3)

# SEX ~ EVERYTHING--------------------------------------------------------------

# balancing the samples, based on lowest sample size per ID
tidy_s_by_ID <- bandpassed %>% group_by(ID) %>% summarise(Count=n())
View(tidy_s_by_ID)
# in this case, Wuschel has the lowest sample size (n = 446)
# randomly drawing 446 samples for each individual and creating a new data set
set.seed(12)
tidy_s <- bandpassed %>% group_by(ID) %>% slice_sample(n=446)

sb_s <- sample.split(tidy_s$sex, SplitRatio = 0.7)
train_s = tidy_s[sb_s, ]
test_s = tidy_s[!sb_s, ]
train_s$sex <- as.factor(train_s$sex)
test_s$sex <- as.factor(test_s$sex)

model_mtry_s <- train(sex ~ avg_entropy + bw_90 + dur_90 + freq_5 + freq_95 + 
                        freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                        `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                        `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                        `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                        `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                        `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                        `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                        `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                        `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                        `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                        `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                        `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                        `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                        `131`+ `132` + `133`, data=train_s, 
                      method="rf", tuneGrid = mtryGrid, ntree = 500)

bestmtry_s <- model_mtry_s$bestTune$mtry

rf_model_s <- randomForest(train_s[,c(13:19,22:155)], train_s$sex, method="rf", 
                           mtry=bestmtry_s, ntree=500, importance = TRUE)
print(rf_model_s)

pred_train_s <- predict(rf_model_s, test_s)
confusionMatrix(pred_train_s, test_s$sex)

varImpPlot(rf_model_s) 

# PREDICTING SEX INFO CONTENT IN A1s--------------------------------------------

tidy_A_by_ID <- tidy_As %>% group_by(ID, element) %>% summarise(Count=n())
View(tidy_A_by_ID)
set.seed(19)
tidy_s_A <- tidy_As %>% group_by(ID) %>% slice_sample(n=14)

sb_s_A <- sample.split(tidy_s_A$sex, SplitRatio = 0.7)
train_s_A = tidy_s_A[sb_s_A, ]
test_s_A = tidy_s_A[!sb_s_A, ]
train_s_A$sex <- as.factor(train_s_A$sex)
test_s_A$sex <- as.factor(test_s_A$sex)

model_mtry_s_A <- train(sex ~ avg_entropy + bw_90 + dur_90 + freq_5 + freq_95
                        + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                          `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                          `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                          `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                          `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                          `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                          `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                          `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                          `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                          `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                          `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                          `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                          `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                          `131`+ `132` + `133`, data=train_s_A, 
                        method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_s_A <- model_mtry_s_A$bestTune$mtry

rf_model_s_A <- randomForest(train_s_A[,c(13:19,22:155)],train_s_A$sex,
                             method="rf", mtry=bestmtry_s_A, ntree=500, importance = TRUE)
print(rf_model_s_A)

pred_train_s_A <- predict(rf_model_s_A, test_s_A)
confusionMatrix(pred_train_s_A, test_s_A$sex)

varImpPlot(rf_model_s_A)

# PREDICTING SEX INFO CONTENT IN Bs---------------------------------------------

tidy_B_1_by_ID <- tidy_B_1 %>% group_by(ID, element) %>% summarise(Count=n())
View(tidy_B_1_by_ID)
set.seed(13)
tidy_s_B_1 <- tidy_B_1 %>% group_by(ID) %>% slice_sample(n=63)

sb_s_B_1 <- sample.split(tidy_s_B_1$sex, SplitRatio = 0.7)
train_s_B_1 = tidy_s_B_1[sb_s_B_1, ]
test_s_B_1 = tidy_s_B_1[!sb_s_B_1, ]
train_s_B_1$sex <- as.factor(train_s_B_1$sex)
test_s_B_1$sex <- as.factor(test_s_B_1$sex)

model_mtry_s_B_1 <- train(sex ~ avg_entropy + bw_90 + dur_90 + freq_5 + freq_95
                          + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                            `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                            `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                            `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                            `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                            `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                            `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                            `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                            `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                            `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                            `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                            `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                            `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                            `131`+ `132` + `133`, data=train_s_B_1, 
                          method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_s_B_1 <- model_mtry_s_B_1$bestTune$mtry

rf_model_s_B_1<-randomForest(train_s_B_1[,c(13:19,22:155)],train_s_B_1$sex,
                             method="rf", mtry=bestmtry_s_B_1, ntree=500, importance = TRUE)
print(rf_model_s_B_1)

pred_train_s_B_1 <- predict(rf_model_s_B_1, test_s_B_1)
confusionMatrix(pred_train_s_B_1, test_s_B_1$sex)

varImpPlot(rf_model_s_B_1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tidy_B_2_by_ID <- tidy_B_2 %>% group_by(ID, element) %>% summarise(Count=n())
View(tidy_B_2_by_ID)
set.seed(14)
tidy_s_B_2 <- tidy_B_2 %>% group_by(ID) %>% slice_sample(n=63)

sb_s_B_2 <- sample.split(tidy_s_B_2$sex, SplitRatio = 0.7)
train_s_B_2 = tidy_s_B_2[sb_s_B_2, ]
test_s_B_2 = tidy_s_B_2[!sb_s_B_2, ]
train_s_B_2$sex <- as.factor(train_s_B_2$sex)
test_s_B_2$sex <- as.factor(test_s_B_2$sex)

model_mtry_s_B_2 <- train(sex ~ avg_entropy + bw_90 + dur_90 + freq_5 + freq_95
                          + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                            `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                            `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                            `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                            `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                            `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                            `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                            `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                            `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                            `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                            `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                            `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                            `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                            `131`+ `132` + `133`, data=train_s_B_2, 
                          method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_s_B_2 <- model_mtry_s_B_2$bestTune$mtry

rf_model_s_B_2<-randomForest(train_s_B_2[,c(13:19,22:155)],train_s_B_2$sex,method="rf", 
                             mtry=bestmtry_s_B_2, ntree=500, importance = TRUE)
print(rf_model_s_B_2)

pred_train_s_B_2 <- predict(rf_model_s_B_2, test_s_B_2)
confusionMatrix(pred_train_s_B_2, test_s_B_2$sex)

varImpPlot(rf_model_s_B_2)

# PREDICTING SEX INFO CONTENT IN Cs---------------------------------------------

tidy_C_1_by_ID <- tidy_C_1 %>% group_by(ID, element) %>% summarise(Count=n())
View(tidy_C_1_by_ID)
set.seed(15)
tidy_s_C_1 <- tidy_C_1 %>% group_by(ID) %>% slice_sample(n=33)

sb_s_C_1 <- sample.split(tidy_s_C_1$sex, SplitRatio = 0.7)
train_s_C_1 = tidy_s_C_1[sb_s_C_1, ]
test_s_C_1 = tidy_s_C_1[!sb_s_C_1, ]
train_s_C_1$sex <- as.factor(train_s_C_1$sex)
test_s_C_1$sex <- as.factor(test_s_C_1$sex)

model_mtry_s_C_1 <- train(sex ~ avg_entropy + bw_90 + dur_90 + freq_5 + freq_95
                          + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                            `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                            `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                            `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                            `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                            `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                            `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                            `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                            `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                            `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                            `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                            `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                            `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                            `131`+ `132` + `133`, data=train_s_C_1, 
                          method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_s_C_1 <- model_mtry_s_C_1$bestTune$mtry

rf_model_s_C_1<-randomForest(train_s_C_1[,c(13:19,22:155)],train_s_C_1$sex,method="rf", 
                             mtry=bestmtry_s_C_1, ntree=500, importance = TRUE)
print(rf_model_s_C_1)

pred_train_s_C_1 <- predict(rf_model_s_C_1, test_s_C_1)
confusionMatrix(pred_train_s_C_1, test_s_C_1$sex)

varImpPlot(rf_model_s_C_1)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tidy_C_2_by_ID <- tidy_C_2 %>% group_by(ID, element) %>% summarise(Count=n())
View(tidy_C_2_by_ID)
set.seed(16)
tidy_s_C_2 <- tidy_C_2 %>% group_by(ID) %>% slice_sample(n=36)

sb_s_C_2 <- sample.split(tidy_s_C_2$sex, SplitRatio = 0.7)
train_s_C_2 = tidy_s_C_2[sb_s_C_2, ]
test_s_C_2 = tidy_s_C_2[!sb_s_C_2, ]
train_s_C_2$sex <- as.factor(train_s_C_2$sex)
test_s_C_2$sex <- as.factor(test_s_C_2$sex)

model_mtry_s_C_2 <- train(sex ~ avg_entropy + bw_90 + dur_90 + freq_5 + freq_95
                          + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                            `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                            `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                            `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                            `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                            `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                            `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                            `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                            `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                            `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                            `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                            `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                            `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                            `131`+ `132` + `133`, data=train_s_C_2, 
                          method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_s_C_2 <- model_mtry_s_C_2$bestTune$mtry

rf_model_s_C_2<-randomForest(train_s_C_2[,c(13:19,22:155)],train_s_C_2$sex,method="rf", 
                             mtry=bestmtry_s_C_2, ntree=500, importance = TRUE)
print(rf_model_s_C_2)

pred_train_s_C_2 <- predict(rf_model_s_C_2, test_s_C_2)
confusionMatrix(pred_train_s_C_2, test_s_C_2$sex) 

varImpPlot(rf_model_s_C_2)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tidy_C_3_by_ID <- tidy_C_3 %>% group_by(ID, element) %>% summarise(Count=n())
View(tidy_C_3_by_ID)
set.seed(17)
tidy_s_C_3 <- tidy_C_3 %>% group_by(ID) %>% slice_sample(n=36)

sb_s_C_3 <- sample.split(tidy_s_C_3$sex, SplitRatio = 0.7)
train_s_C_3 = tidy_s_C_3[sb_s_C_3, ]
test_s_C_3 = tidy_s_C_3[!sb_s_C_3, ]
train_s_C_3$sex <- as.factor(train_s_C_3$sex)
test_s_C_3$sex <- as.factor(test_s_C_3$sex)

model_mtry_s_C_3 <- train(sex ~ avg_entropy + bw_90 + dur_90 + freq_5 + freq_95
                          + freq_center + time_center + shape + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + 
                            `11` + `12` + `13` + `14` + `15` + `16` + `17` + `18` + `19` + `20` + 
                            `21` + `22` + `23` + `24` + `25` + `26` + `27` + `28` + `29` + `30` + 
                            `31` + `32` + `33` + `34` + `35` + `36` + `37` + `38` + `39` + `40` + 
                            `41` + `42` + `43` + `44` + `45` + `46` + `47` + `48` + `49` + `50` + 
                            `51` + `52` + `53` + `54` + `55` + `56` + `57` + `58` + `59` + `60` + 
                            `61` + `62` + `63` + `64` + `65` + `66` + `67` + `68` + `69` + `70` + 
                            `71` + `72` + `73` + `74` + `75` + `76` + `77` + `78` + `79` + `80` + 
                            `81` + `82` + `83` + `84` + `85` + `86` + `87` + `88` + `89` + `90` + 
                            `91` + `92` + `93` + `94` + `95` + `96` + `97` + `98` + `99` + `100` + 
                            `101` + `102` + `103` + `104` + `105` + `106` + `107` + `108` + `109` + `110` + 
                            `111` + `112` + `113` + `114` + `115` + `116` + `117` + `118` + `119` + `120` + 
                            `121` + `122` + `123` + `124` + `125` + `126` + `127` + `128` + `129` + `130` + 
                            `131`+ `132` + `133`, data=train_s_C_3, 
                          method="rf", tuneGrid=mtryGrid, ntree=500)

bestmtry_s_C_3 <- model_mtry_s_C_3$bestTune$mtry

rf_model_s_C_3<-randomForest(train_s_C_3[,c(13:19,22:155)],train_s_C_3$sex,method="rf", 
                             mtry=bestmtry_s_C_3, ntree=500, importance = TRUE)
print(rf_model_s_C_3)

pred_train_s_C_3 <- predict(rf_model_s_C_3, test_s_C_3)
confusionMatrix(pred_train_s_C_3, test_s_C_3$sex)

varImpPlot(rf_model_s_C_3)

# LINEAR MODELS-----------------------------------------------------------------

# seeing how B type sequence elements in different positions differ according to
# their most important predictor: Center Time

mod0 <- lmer(time_center ~ el_no + (1|ID), data=tidy_Bs)
anova(mod0)
summary(mod0)
resid_panel(mod0) # checking residual plots

# creating plot

plot1 <- ggplot(tidy_Bs, aes(el_no,predict(mod0)))+
  geom_boxplot(aes(fill=el_no))+
  xlab("Relative position within sequence")+
  ylab("Center Time (s)")+
  theme_light()+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=13.5))+
  theme(axis.title.y = element_text(margin = ggplot2::margin(r = 10))) + 
  scale_fill_manual(values=c("gray0", "gray99"))+
  theme(legend.position= "none") 
plot1

# seeing how C type sequence elements in different positions differ according to
# their most important predictor: Center Time (using the square root value)

mod <- lmer(sqrt(time_center) ~ el_no + (1|ID), data=tidy_Cs)
anova(mod)
summary(mod)
resid_panel(mod) # checking residual plots

# pairwise comparison of the values

time.emm.s <- emmeans(mod, "el_no")
pairs(time.emm.s)

# creating plot (+ backtransforming model results)

plot2 <- ggplot(tidy_Cs, aes(el_no, predict(mod)^2))+
  geom_boxplot(aes(fill=el_no))+
  xlab("Relative position within sequence")+
  ylab("Center Time (s)")+
  theme_light()+
  theme(axis.text=element_text(size=12), axis.title=element_text(size=13.5))+
  theme(axis.title.y = element_text(margin = ggplot2::margin(r = 10))) + 
  scale_fill_manual(values=c("gray0", "gray99", "gray50"))+
  theme(legend.position= "none")
plot2 

# creating a grid so the two plots are in one figure

plot_grid(plot1, 
          plot2 +
            theme(axis.title.y = element_blank()), labels = "AUTO", ncol=2)

