##Load data sets

phone_usage <- read.csv("phone_use_data.csv", header = TRUE)
mood_sampling <- read.csv("mood_sampling_data.csv", header = TRUE)

##Load needed packages
library("ggplot2")
library("reshape2") #in thesis
library("dplyr") # in thesis
library("chron") # in thesis
library("reshape")
library("tidyverse") # in thesis
library("hash")
##library("proxy") 
library("factoextra") #in thesis

##Clean mood sampling (stress and calm: day, part, hour)
mood_sampling_clean <- subset(mood_sampling, stressed >= 0 & stressed <= 5 & calm >= 0 & calm <= 5,
                              select= c(user_id, response_time, stressed, calm))

mood_sampling_clean$stressed <- mood_sampling_clean$stressed + 1
mood_sampling_clean$calm <- mood_sampling_clean$calm + 1

mood_sampling_clean$response_splitted <- substr(mood_sampling_clean$response_time, 0, 10)
mood_sampling_clean$response_time_splitted <- substr(mood_sampling_clean$response_time, 12, 19)
mood_sampling_clean$response_time_splitted <- times(paste0(mood_sampling_clean$response_time_splitted))
mood_sampling_clean$response_hour <- substr(mood_sampling_clean$response_time_splitted, 0, 2)

breaks <- c(0, 6, 12, 18, 24) / 24 # times are internally fractions of a day
labels <- c("night", "morning", "daylight", "evening")
mood_sampling_clean$response_part_day <- cut(mood_sampling_clean$response_time_splitted, breaks, labels, 
                                             include.lowest = TRUE)

##clean phone usage data set (nothing is done with endtime, so also not described in thesis)
starttime_splitted <- colsplit(phone_usage$startTime, "T", c("start_date", "start_time"))
phone_usage_clean <- phone_usage %>%
  cbind(starttime_splitted)
phone_usage_clean$starttime_hour <- substr(phone_usage_clean$start_time, 0, 2)

phone_usage_clean$start_time <- times(paste0(phone_usage_clean$start_time))

#remove T
phone_usage_clean$startTime <- sub("T", " ", phone_usage_clean$startTime)
phone_usage_clean$endTime <- sub("T", " ", phone_usage_clean$endTime)

phone_usage_clean$start_part_day <- cut(phone_usage_clean$start_time, breaks, labels, include.lowest = TRUE)

phone_usage_clean$duration_mins <- difftime(phone_usage_clean$endTime, phone_usage_clean$startTime, unit = "mins")

matched_data_day <- left_join(phone_usage_clean, mood_sampling_clean, 
                              by = c("user_id" = "user_id", "start_date" = "response_splitted"))

matched_data_part <- left_join(phone_usage_clean, mood_sampling_clean, 
                               by = c("user_id" = "user_id", "start_date" = "response_splitted", 
                                      "start_part_day" = "response_part_day"))

matched_data_hour <- left_join(phone_usage_clean, mood_sampling_clean, 
                               by = c("user_id" = "user_id", "start_date" = "response_splitted",
                                      "starttime_hour" = "response_hour"))

##create extra datasets: duration and times a day of taking your phone
aggregate_duration_day <- matched_data_day %>% 
  group_by(user_id, start_date) %>%
  summarize(duration_day = sum(duration_mins))

aggregate_times_day <- matched_data_day %>% 
  group_by(user_id, start_date) %>%
  summarize(times_day = sum(n_distinct(session)))

##create extra datasets: duration and times per part of taking your phone
aggregate_duration_part <- matched_data_part %>% 
  group_by(user_id, start_date, start_part_day) %>%
  summarize(duration_part = sum(duration_mins))

aggregate_times_part <- matched_data_part %>% 
  group_by(user_id, start_date, start_part_day) %>%
  summarize(times_part = sum(n_distinct(session)))

##create extra datasets: duration and times per hour of taking your phone

aggregate_duration_hour <- matched_data_hour %>% 
  group_by(user_id, start_date, starttime_hour) %>%
  summarize(duration_hour = sum(duration_mins))

aggregate_times_hour <- matched_data_hour %>% 
  group_by(user_id, start_date, starttime_hour) %>%
  summarize(times_hour = sum(n_distinct(session)))

##Add duration & times per day 

matched_data_day <- left_join(matched_data_day, aggregate_duration_day, 
                              by = c("user_id", "start_date"))
matched_data_day <- left_join(matched_data_day, aggregate_times_day, 
                              by = c("user_id", "start_date"))

##Add duration & times per part day 
matched_data_part <- left_join(matched_data_part, aggregate_duration_part, 
                               by = c("user_id", "start_date", "start_part_day"))
matched_data_part <- left_join(matched_data_part, aggregate_times_part, 
                               by = c("user_id", "start_date", "start_part_day"))

##Add duration & times per hour
matched_data_hour <- left_join(matched_data_hour, aggregate_duration_hour, 
                               by = c("user_id", "start_date", "starttime_hour"))
matched_data_hour <- left_join(matched_data_hour, aggregate_times_hour, 
                               by = c("user_id", "start_date", "starttime_hour"))

matched_data_part$start_date_part <- paste(matched_data_part$start_date, matched_data_part$start_part_day, sep= " ")
matched_data_hour$start_date_hour <- paste(matched_data_hour$start_date, matched_data_hour$starttime_hour, sep= " ")

##remove approximately 20.000 duplicated rows!
matched_data_day <- matched_data_day %>% distinct()
matched_data_part <- matched_data_part %>% distinct()
matched_data_hour <- matched_data_hour%>% distinct()

matched_data_day$duration_day <- ifelse(matched_data_day$duration_day > 1440, 1440, matched_data_day$duration_day)
matched_data_part$duration_part <- ifelse(matched_data_part$duration_part > 360, 360, matched_data_part$duration_part)
matched_data_hour$duration_hour <- ifelse(matched_data_hour$duration_hour > 60, 60, matched_data_hour$duration_hour)

##without_na <- subset(matched_data_day, is.na(stressed & calm))
##unique(without_na$user_id)

##ggplot(matched_data_day, aes(x = stressed)) + geom_bar()
##ggplot(matched_data_day, aes(x = calm)) + geom_bar()

##To calculate the mean level of stress & calm: ignore NA values!
##Stress, calm, usage and duration per day 

day_stress_transposed <- matched_data_day %>%
  select("user_id", "start_date", "stressed") %>%
  cast(user_id ~ start_date, mean, value = "stressed", na.rm = TRUE)
day_stress_transposed <- round(day_stress_transposed, 0)

day_calm_transposed <- matched_data_day %>%
  select("user_id", "start_date", "calm") %>%
  cast(user_id ~ start_date, mean, value = "calm", na.rm = TRUE)
day_calm_transposed <- round(day_calm_transposed, 0)

day_usage_transposed <- matched_data_day %>%
  select("user_id", "start_date", "duration_day") %>%
  cast(user_id ~ start_date, mean, value = "duration_day")

day_times_transposed <- matched_data_day %>%
  select("user_id", "start_date", "times_day") %>%
  cast(user_id ~ start_date, mean, value = "times_day")

##Stress, calm, usage and duration per PART of day 

part_usage_transposed <- matched_data_part %>%
  select("user_id", "start_date_part", "duration_part") %>%
  cast(user_id ~ start_date_part, mean, value = "duration_part")

part_times_transposed <- matched_data_part %>%
  select("user_id", "start_date_part", "times_part") %>%
  cast(user_id ~ start_date_part, mean, value = "times_part")

##Stress, calm, usage and duration per HOUR

hour_usage_transposed <- matched_data_hour %>%
  select("user_id", "start_date_hour", "duration_hour") %>%
  cast(user_id ~ start_date_hour, mean, value = "duration_hour")

hour_times_transposed <- matched_data_hour %>%
  select("user_id", "start_date_hour", "times_hour") %>%
  cast(user_id ~ start_date_hour, mean, value = "times_hour")

saveRDS(day_stress_transposed, "day_stress_transposed")
saveRDS(day_calm_transposed, "day_calm_transposed")
saveRDS(day_usage_transposed, "day_usage_transposed")
saveRDS(day_times_transposed, "day_times_transposed")

saveRDS(part_stress_transposed, "part_stress_transposed")
saveRDS(part_calm_transposed, "part_calm_transposed")
saveRDS(part_usage_transposed, "part_usage_transposed")
saveRDS(part_times_transposed, "part_times_transposed")

saveRDS(hour_stress_transposed, "hour_stress_transposed")
saveRDS(hour_calm_transposed, "hour_calm_transposed")
saveRDS(hour_usage_transposed, "hour_usage_transposed")
saveRDS(hour_times_transposed, "hour_times_transposed")

##----------------------------------------------------------- Python 

S_euclidean_day <- as.matrix(read.csv("S_euclidean_day", header = FALSE))
S_cosine_day <- as.matrix(read.csv("S_cosine_day", header = FALSE))
C_euclidean_day <- as.matrix(read.csv("C_euclidean_day", header = FALSE))
C_cosine_day <- as.matrix(read.csv("C_cosine_day", header = FALSE))
U_euclidean_day <- as.matrix(read.csv("U_euclidean_day", header = FALSE))
U_cosine_day <- as.matrix(read.csv("U_cosine_day", header = FALSE))
T_euclidean_day <- as.matrix(read.csv("T_euclidean_day", header = FALSE))
T_cosine_day <- as.matrix(read.csv("T_cosine_day", header = FALSE))

U_euclidean_part <- as.matrix(read.csv("U_euclidean_part", header = FALSE))
U_cosine_part <- as.matrix(read.csv("U_cosine_part", header = FALSE))
T_euclidean_part <- as.matrix(read.csv("T_euclidean_part", header = FALSE))
T_cosine_part <- as.matrix(read.csv("T_cosine_part", header = FALSE))

U_euclidean_hour <- as.matrix(read.csv("U_euclidean_hour", header = FALSE))
U_cosine_hour <- as.matrix(read.csv("U_cosine_hour", header = FALSE))
T_euclidean_hour <- as.matrix(read.csv("T_euclidean_hour", header = FALSE))
T_cosine_hour <- as.matrix(read.csv("T_cosine_hour", header = FALSE))

##S_cosine_day2 <- as.matrix(read.csv("S_cosine_day2", header = FALSE))
##colnames(S_cosine_day2) <- NULL
##Sd_cosine_day2 <- as.dist(1 - S_cosine_day2)

colnames(S_euclidean_day) <- NULL
colnames(S_cosine_day) <- NULL
colnames(C_euclidean_day) <- NULL
colnames(C_cosine_day) <- NULL
colnames(U_euclidean_day) <- NULL
colnames(U_cosine_day) <- NULL
colnames(T_euclidean_day) <- NULL
colnames(T_cosine_day) <- NULL

colnames(U_euclidean_part) <- NULL
colnames(U_cosine_part) <- NULL
colnames(T_euclidean_part) <- NULL
colnames(T_cosine_part) <- NULL

colnames(U_euclidean_hour) <- NULL
colnames(U_cosine_hour) <- NULL
colnames(T_euclidean_hour) <- NULL
colnames(T_cosine_hour) <- NULL

##Hierarchical clustering
Sd_euclidean_day <- as.dist(S_euclidean_day)

##S_cosine_day_copy <- 1 - as.vector(S_cosine_day)

Sd_cosine_day <- as.dist(1 - S_cosine_day)
Cd_euclidean_day <- as.dist(C_euclidean_day)
Cd_cosine_day <- as.dist(1 - C_cosine_day)
Ud_euclidean_day <- as.dist(U_euclidean_day)
Ud_cosine_day <- (as.dist(1 - U_cosine_day))
Td_euclidean_day <- as.dist(T_euclidean_day)
Td_cosine_day <- as.dist(1 - T_cosine_day)

Ud_euclidean_part <- as.dist(U_euclidean_part)
Ud_cosine_part <- as.dist(1 - U_cosine_part)
Td_euclidean_part <- as.dist(T_euclidean_part)
Td_cosine_part <- as.dist(1 - T_cosine_part)

Ud_euclidean_hour <- as.dist(U_euclidean_hour)
Ud_cosine_hour <- as.dist(1 - U_cosine_hour)
Td_euclidean_hour <- as.dist(T_euclidean_hour)
Td_cosine_hour <- as.dist(1 - T_cosine_hour)

## Correlations
cor.test(as.vector(Sd_euclidean_day), as.vector((Ud_euclidean_day)))
cor.test(as.vector(Sd_euclidean_day), as.vector((Td_euclidean_day)))
cor.test(as.vector(Sd_cosine_day), as.vector((Ud_cosine_day)))
cor.test(as.vector(Sd_cosine_day), as.vector((Td_cosine_day)))

cor.test(as.vector(Sd_euclidean_day), as.vector((Ud_euclidean_part)))
cor.test(as.vector(Sd_euclidean_day), as.vector((Td_euclidean_part)))
cor.test(as.vector(Sd_cosine_day), as.vector((Ud_cosine_part)))
cor.test(as.vector(Sd_cosine_day), as.vector((Td_cosine_part)))

cor.test(as.vector(Sd_euclidean_day), as.vector((Ud_euclidean_hour)))
cor.test(as.vector(Sd_euclidean_day), as.vector((Td_euclidean_hour)))
cor.test(as.vector(Sd_cosine_day), as.vector((Ud_cosine_hour)))
cor.test(as.vector(Sd_cosine_day), as.vector((Td_cosine_hour)))

cor.test(as.vector(Cd_euclidean_day), as.vector((Ud_euclidean_day)))
cor.test(as.vector(Cd_euclidean_day), as.vector((Td_euclidean_day)))
cor.test(as.vector(Cd_cosine_day), as.vector((Ud_cosine_day)))
cor.test(as.vector(Cd_cosine_day), as.vector((Td_cosine_day)))

cor.test(as.vector(Cd_euclidean_day), as.vector((Ud_euclidean_part)))
cor.test(as.vector(Cd_euclidean_day), as.vector((Td_euclidean_part)))
cor.test(as.vector(Cd_cosine_day), as.vector((Ud_cosine_part)))
cor.test(as.vector(Cd_cosine_day), as.vector((Td_cosine_part)))

cor.test(as.vector(Cd_euclidean_day), as.vector((Ud_euclidean_hour)))
cor.test(as.vector(Cd_euclidean_day), as.vector((Td_euclidean_hour)))
cor.test(as.vector(Cd_cosine_day), as.vector((Ud_cosine_hour)))
cor.test(as.vector(Cd_cosine_day), as.vector((Td_cosine_hour)))

plot(Sd_cosine_day, Td_cosine_hour)
plot(Sd_cosine_day, Ud_cosine_hour)

plot(Sd_euclidean_day, Td_euclidean_hour)
plot(Sd_euclidean_day, Ud_euclidean_hour)

plot(Cd_cosine_day, Td_cosine_hour)
plot(Cd_cosine_day, Ud_cosine_hour)

Ue_day_hc <- hclust(Ud_euclidean_day, method = "ward.D2")
plot(Ue_day_hc)
Te_day_hc <- hclust(Td_euclidean_day, method = "ward.D2")
plot(Te_day_hc)

Uc_day_hc <- hclust(Ud_cosine_day, method = "ward.D")
plot(Uc_day_hc)
Tc_day_hc <- hclust(Td_cosine_day, method = "ward.D")
plot(Tc_day_hc)


Ue_part_hc <- hclust(Ud_euclidean_part, method = "ward.D2")
plot(Ue_part_hc)
Te_part_hc <- hclust(Td_euclidean_part, method = "ward.D2")
plot(Te_part_hc)

Uc_part_hc <- hclust(Ud_cosine_part, method = "ward.D")
plot(Uc_part_hc)
Tc_part_hc <- hclust(Td_cosine_part, method = "ward.D")
plot(Tc_part_hc)


Ue_hour_hc <- hclust(Ud_euclidean_hour, method = "ward.D2")
plot(Ue_hour_hc)
Te_hour_hc <- hclust(Td_euclidean_hour, method = "ward.D2")
plot(Te_hour_hc)

Uc_hour_hc <- hclust(Ud_cosine_hour, method = "ward.D")
plot(Uc_hour_hc)
Tc_hour_hc <- hclust(Td_cosine_hour, method = "ward.D")
plot(Tc_hour_hc)

##Optimal split
fviz_nbclust(U_euclidean_day, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(U_euclidean_day, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(U_cosine_day, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(U_cosine_day, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(T_euclidean_day, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(T_euclidean_day, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(T_cosine_day, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(T_cosine_day, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(U_euclidean_part, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(U_euclidean_part, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(U_cosine_part, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(U_cosine_part, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(T_euclidean_part, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(T_euclidean_part, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(T_cosine_part, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(T_cosine_part, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(U_euclidean_hour, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(U_euclidean_hour, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(U_cosine_hour, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(U_cosine_hour, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(T_euclidean_hour, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(T_euclidean_hour, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

fviz_nbclust(T_cosine_hour, hcut, method = "silhouette", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: silhoutte")
fviz_nbclust(T_cosine_hour, hcut, method = "wss", k.max = 50) + 
  theme_minimal() + ggtitle("the Elbow Method: wss")

#Cut the tree
subgroup_usage_day_euclidean <- cutree(Ue_day_hc, k = 4)
subgroup_times_day_euclidean <- cutree(Te_day_hc, k = 5)

subgroup_usage_day_cosine <- cutree(Uc_day_hc, k = 4)
subgroup_times_day_cosine <- cutree(Tc_day_hc, k = 4)

subgroup_usage_part_euclidean <- cutree(Ue_part_hc, k = 3)
subgroup_times_part_euclidean <- cutree(Te_part_hc, k = 4)

subgroup_usage_part_cosine <- cutree(Uc_part_hc, k = 5)
subgroup_times_part_cosine <- cutree(Tc_part_hc, k = 4)

subgroup_usage_hour_euclidean <- cutree(Ue_hour_hc, k = 4)
subgroup_times_hour_euclidean <- cutree(Te_hour_hc, k = 4)

subgroup_usage_hour_cosine <- cutree(Uc_hour_hc, k = 6)
subgroup_times_hour_cosine <- cutree(Tc_hour_hc, k = 4)

## 1.Create dictionary usage hour euclidean (stress)
time_dictionary <- hash() 

for (i in seq_along(subgroup_usage_hour_euclidean)) {
  .set(time_dictionary, i, subgroup_usage_hour_euclidean[i])
}

time_dictionary <- (sort(values(time_dictionary)))
time_list <- as.list(time_dictionary)
keys_time <- as.numeric(names(time_list))

Sd_euclidean_matrix <- as.matrix(Sd_euclidean_day)
Sd2_euclidean_matrix <- Sd_euclidean_matrix[(keys_time),(keys_time)]

melted_Sd2_euclidean <- melt(Sd2_euclidean_matrix, na.rm = TRUE)

ggplot(data = melted_Sd2_euclidean, aes(x=X2, y=X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab", 
                       name="Euclidean distance / stress - usage hour") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 10, vjust = 1, 
                                   size = 7, hjust = 1))+
  coord_fixed()

## 2. Create dictionary usage hour cosine stress
time_dictionary <- hash() 

for (i in seq_along(subgroup_usage_hour_cosine)) {
  .set(time_dictionary, i, subgroup_usage_hour_cosine[i])
}

time_dictionary <- (sort(values(time_dictionary)))
time_list <- as.list(time_dictionary)
keys_time <- as.numeric(names(time_list))

Sd_cosine_matrix <- as.matrix(Sd_cosine_day)
Sd2_cosine_matrix <- Sd_cosine_matrix[(keys_time),(keys_time)]

melted_Sd2_cosine <- melt(Sd2_cosine_matrix, na.rm = TRUE)

ggplot(data = melted_Sd2_cosine, aes(x=X2, y=X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab", 
                       name="Cosine distance / stress - usage hour") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 10, vjust = 1, 
                                   size = 7, hjust = 1))+
  coord_fixed()

## 3. Create dictionary usage day cosine calm
time_dictionary <- hash() 

for (i in seq_along(subgroup_usage_hour_cosine)) {
  .set(time_dictionary, i, subgroup_usage_hour_cosine[i])
}

time_dictionary <- (sort(values(time_dictionary)))
time_list <- as.list(time_dictionary)
keys_time <- as.numeric(names(time_list))

Cd_cosine_matrix <- as.matrix(Cd_cosine_day)
Cd2_cosine_matrix <- Cd_cosine_matrix[(keys_time),(keys_time)]

melted_Cd2_cosine <- melt(Cd2_cosine_matrix, na.rm = TRUE)

ggplot(data = melted_Cd2_cosine, aes(x=X2, y=X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab", 
                       name="Cosine distance / calm - usage hour") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 30, vjust = 1, 
                                   size = 6, hjust = 1))+
  coord_fixed()

## 4.Create dictionary times hour euclidean (stress)
time_dictionary <- hash() 

for (i in seq_along(subgroup_times_hour_euclidean)) {
  .set(time_dictionary, i, subgroup_times_hour_euclidean[i])
}

time_dictionary <- (sort(values(time_dictionary)))
time_list <- as.list(time_dictionary)
keys_time <- as.numeric(names(time_list))

Sd_euclidean_matrix <- as.matrix(Sd_euclidean_day)
Sd2_euclidean_matrix <- Sd_euclidean_matrix[(keys_time),(keys_time)]

melted_Sd2_euclidean <- melt(Sd2_euclidean_matrix, na.rm = TRUE)

ggplot(data = melted_Sd2_euclidean, aes(x=X2, y=X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab", 
                       name="Euclidean distance / stress - times hour") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 10, vjust = 1, 
                                   size = 6, hjust = 1))+
  coord_fixed()

## 5. Create dictionary times hour cosine stress
time_dictionary <- hash() 

for (i in seq_along(subgroup_times_hour_cosine)) {
  .set(time_dictionary, i, subgroup_times_hour_cosine[i])
}

time_dictionary <- (sort(values(time_dictionary)))
time_list <- as.list(time_dictionary)
keys_time <- as.numeric(names(time_list))

Sd_cosine_matrix <- as.matrix(Sd_cosine_day)
Sd2_cosine_matrix <- Sd_cosine_matrix[(keys_time),(keys_time)]

melted_Sd2_cosine <- melt(Sd2_cosine_matrix, na.rm = TRUE)

ggplot(data = melted_Sd2_cosine, aes(x=X2, y=X1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab", 
                       name="Cosine distance / stress - times hour") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 8, vjust = 1, 
                                   size = 6, hjust = 1))+
  coord_fixed()


## 6. Create dictionary times hour cosine calm
time_dictionary <- hash() 

for (i in seq_along(subgroup_times_hour_cosine)) {
  .set(time_dictionary, i, subgroup_times_hour_cosine[i])
}

time_dictionary <- (sort(values(time_dictionary)))
time_list <- as.list(time_dictionary)
keys_time <- as.numeric(names(time_list))

Cd_cosine_matrix <- as.matrix(Cd_cosine_day)
Cd2_cosine_matrix <- Cd_cosine_matrix[(keys_time),(keys_time)]

melted_Cd2_cosine <- melt(Cd2_cosine_matrix)

ggplot(data = melted_Cd2_cosine, aes(x=X1, y=X2, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", space = "Lab", 
                       name="Cosine distance / calm - times hour") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 20, vjust = 1, 
                                   size = 6, hjust = 1))+
  coord_fixed()

