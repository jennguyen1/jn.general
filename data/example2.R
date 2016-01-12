
###################################
# format and prepare survey files #
###################################

# define a function that opens and cleans survey files 
open_survey <- function(x){
  
  # clean file
  file <- x %>% 
    # selects teacher id & survey answers
    dplyr::select(teacherid_da, starts_with("m_")) %>% 
    # make survey responses numeric
    mutate_each(funs(as.numeric), -teacherid_da) 
  
  # return file
  return(file)
}

# for upper elementary
survey_data_upper_elem_2015 <- open_survey(raw_student_survey_upper_elem)

# ditto for secondary school
survey_data_secondary_2015 <- open_survey(raw_student_survey_secondary)

##########################################
# edit survey files: question polarities #
##########################################

# reverse polarities
neg_questions <- subset(survey_code, polarities == "Neg")$var

# function to reverse the polarity of survey answers
reverse_polarity <- function(dat){
  
  # split data into questions to edit and other
  neg_ques <- dplyr::select(dat, one_of(neg_questions))
  pos_ques <- dplyr::select(dat, -one_of(neg_questions))
  
  # loop through columns of neg_ques and reverse the polarity
  neg_ques <- neg_ques %>% mutate_each(funs(mapvalues(., c(NA, 1:5), c(NA, 5:1))))
  
  # combine the results and return
  ret_data <- cbind(pos_ques, neg_ques)
  return(ret_data)
}

# run reverse polarity on the two data sets
survey_data_upper_elem_2015 <- reverse_polarity(survey_data_upper_elem_2015)

####################################################
# edit survey files: teacher, ques type aggregates #
####################################################

# combine two files
survey_2015 <- rbindlist(list(survey_data_upper_elem_2015, survey_data_secondary_2015), fill = TRUE)

# melt data and add the survey code vars
melted_survey_2015 <- melt(survey_2015, id.vars = "teacherid_da")
setnames(melted_survey_2015, c("teacherid_da", "variable"), c("teacher_id", "var"))

# add survey code variables
merged_survey_2015 <- merge(melted_survey_2015, survey_code, "var")

# remove the question types we don't care about
merged_survey_2015 <- merged_survey_2015 %>% 
  # remove the category 1s and demographics and empty
  subset(!(question_type %in% c("", "Category 1", "Demographic")))

# compute aggregated scores
agg_survey_scores <- merged_survey_2015 %>% 
  # group by teacher id and question type
  group_by(teacher_id, question_type) %>%
  # aggregated survey score: mean of individual questions
  summarise(score = mean(value, na.rm = TRUE)) %>% 
  # convert all NAN (means of NA) into NA
  mutate(score = ifelse(is.nan(score), NA, score)) %>% 
  # cast to make the category wide
  dcast.data.table(teacher_id ~ question_type, value.var = "score")


##########################################################
# edit survey files: teacher aggregate for each question #
##########################################################
# define a function that computes average scores for survey data
avr_scores <- function(x){
  
  # group by teacher id and find average of survey questions
  agg <- x %>% 
    
    # group by teacher
    group_by(teacherid_da) %>%
    
    # average the survey question answers
    summarise_each(funs(mean(., na.rm = TRUE)))
  
  return(agg)
  
}

# average scores for surveys
agg_survey_ue_2015 <- avr_scores(survey_data_upper_elem_2015)
agg_survey_s_2015 <- avr_scores(survey_data_secondary_2015)

# combine survey results for upper elem & secondary schools
agg_survey_2015 <- rbindlist(list(agg_survey_s_2015, agg_survey_ue_2015), fill=TRUE)
setnames(agg_survey_2015, "teacherid_da","teacher_id")
