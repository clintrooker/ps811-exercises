#1 downloaded data and codebook
here()
#2
food_data <- food_coded
#3
food_first_95 <- food_data[1:95, ]

#4
food_first_95[, c("GPA","calories_chicken", "drink", "fav_cuisine", "father_profession",
                  "mother_profession" )]

food_first_95[, c(1, 4, 16, 25, 26, 45)]

#5 
food_first_95$health <- food_first_95$healthy_feeling*10

#6
female_GPA <- subset(food_first_95, Gender==1 & GPA > 3.0)

#7 
ordered_by_food <- food_first_95[order(food_first_95$fav_cuisine), ]


#8
data_means <- data.frame(chicken_mean <- mean(food_first_95$calories_chicken, na.rm = TRUE),
           chicken_sd <- sd(food_first_95$calories_chicken, na.rm = TRUE),
           waffle_mean <- mean(food_first_95$waffle_calories, na.rm = TRUE),
           waffle_sd <- sd(food_first_95$waffle_calories, na.rm = TRUE),
           turkey_mean <- mean(food_first_95$turkey_calories, na.rm = TRUE),
           turkey_sd <- sd(food_first_95$turkey_calories, na.rm = TRUE),
           tortilla_mean <- mean(food_first_95$tortilla_calories, na.rm = TRUE),
           tortilla_sd <- sd(food_first_95$tortilla_calories, na.rm = TRUE)
           )

#9
aggregate(formula = cbind(Gender, cuisine) ~ GPA + weight, 
          data = food_first_95, 
          FUN = function(x){
            c(mean = mean(x), sd = sd(x))
          })

#Tidy 

#1 and 2 just uploading data 
#3
bottom_500 <- top_n(facebook_fact_check, -500)

#4 
select(bottom_500, "post_id", "Page", "Date Published", "Rating", "share_count", "comment_count")


#5 
bottom_500 <- mutate(bottom_500, post_type_coded =
                       case_when(`Post Type`=="link"~"1",
                                 `Post Type`=="photo"~"2",
                                 `Post Type`=="text"~"3",
                                 `Post Type`=="video"~"4"
                       ))

#6 
arrange(bottom_500, desc(Page))

#7 
summarise(facebook_fact_check,
          share_count_mean <- mean(share_count, na.rm = TRUE),
          share_count_sd <- sd(share_count, na.rm = TRUE),
          reaction_count_mean <- mean(reaction_count, na.rm = TRUE),
          reaction_count_sd <- sd(reaction_count, na.rm = TRUE),
          comment_count_mean <- mean(comment_count, na.rm = TRUE),
          comment_count_sd <- sd(comment_count, na.rm = TRUE))

#8 
facebook_fact_check %>% 
  group_by("mainstream", Category) %>% 
  summarise(share_count_mean = mean(share_count, na.rm = TRUE),
            share_count_sd = sd(share_count, na.rm = TRUE),
            reaction_count_mean = mean(reaction_count, na.rm = TRUE),
            reaction_count_sd = sd(reaction_count, na.rm = TRUE),
            comment_count_mean = mean(comment_count, na.rm = TRUE),
            comment_count_sd = sd(comment_count, na.rm = TRUE)) %>% 
  ungroup()
