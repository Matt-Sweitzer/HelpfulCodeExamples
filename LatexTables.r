library(readxl)
library(mice)
library(miceadds)
library(multiwayvcov)
library(xtable)
options(scipen = 3)
#options(scipen=999)



postdict <- read_excel('/Users/Matthew/Box/Ballot initiatives project/Data/Eye Tracking Data/Postdiction/postdict_all_data_8_18_19.xlsx')
postdict <-as.data.frame(postdict)

predict <- read_excel('/Users/Matthew/Box/Ballot initiatives project/Data/Eye Tracking Data/Prediction/predict_all_data_8_18_19.xlsx')
predict <-as.data.frame(predict)

#Subsets data to only include eye movements-related trials that could be corrected
postdict_good_trials <- postdict[postdict$corrected_trial == '0'| postdict$corrected_trial =='1',]
predict_good_trials <- predict[predict$corrected_trial == '0'| predict$corrected_trial =='1',]

##further subsets predict and postdict data to only trials in which lab participants rendered a directional vote
postdict_good_trials_only_votes <- postdict_good_trials[postdict_good_trials$abstain == '0',]
predict_good_trials_only_votes <- predict_good_trials[predict_good_trials$abstain == '0',]



## i==starting model number; j==variable name
estfn<-function(i, j){
  mat1<-get(paste("rmat", i, sep=""))
  mat2<-get(paste("rmat", i+1, sep=""))
  mat3<-get(paste("rmat", i+2, sep=""))
  mat4<-get(paste("rmat", i+3, sep=""))
  mat5<-get(paste("rmat", i+4, sep=""))
  mat6<-get(paste("rmat", i+5, sep=""))
  est1<-as.character(formatC(mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],1], format="fg", flag="#", digits=2))
  est2<-as.character(formatC(mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],1], format="fg", flag="#", digits=2))
  est3<-as.character(formatC(mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],1], format="fg", flag="#", digits=2))
  est4<-as.character(formatC(mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],1], format="fg", flag="#", digits=2))
  est5<-as.character(formatC(mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],1], format="fg", flag="#", digits=2))
  est6<-as.character(formatC(mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],1], format="fg", flag="#", digits=2))
  if(mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]>0.1){
    est1<-paste("$", est1, "$", sep="")
  }
  if(mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]<=0.1&mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]>0.05){
    est1<-paste("$", est1, "^{+}$", sep="")
  }
  if(mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]<=0.05&mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]>0.01){
    est1<-paste("$", est1, "^{*}$", sep="")
  }
  if(mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]<=0.01&mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]>0.001){
    est1<-paste("$", est1, "^{**}$", sep="")
  }
  if(mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],4]<=0.001){
    est1<-paste("$", est1, "^{***}$", sep="")
  }
  if(mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]>0.1){
    est2<-paste("$", est2, "$", sep="")
  }
  if(mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]<=0.1&mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]>0.05){
    est2<-paste("$", est2, "^{+}$", sep="")
  }
  if(mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]<=0.05&mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]>0.01){
    est2<-paste("$", est2, "^{*}$", sep="")
  }
  if(mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]<=0.01&mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]>0.001){
    est2<-paste("$", est2, "^{**}$", sep="")
  }
  if(mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],4]<=0.001){
    est2<-paste("$", est2, "^{***}$", sep="")
  }
  if(mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]>0.1){
    est3<-paste("$", est3, "$", sep="")
  }
  if(mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]<=0.1&mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]>0.05){
    est3<-paste("$", est3, "^{+}$", sep="")
  }
  if(mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]<=0.05&mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]>0.01){
    est3<-paste("$", est3, "^{*}$", sep="")
  }
  if(mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]<=0.01&mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]>0.001){
    est3<-paste("$", est3, "^{**}$", sep="")
  }
  if(mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],4]<=0.001){
    est3<-paste("$", est3, "^{***}$", sep="")
  }
  if(mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]>0.1){
    est4<-paste("$", est4, "$", sep="")
  }
  if(mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]<=0.1&mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]>0.05){
    est4<-paste("$", est4, "^{+}$", sep="")
  }
  if(mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]<=0.05&mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]>0.01){
    est4<-paste("$", est4, "^{*}$", sep="")
  }
  if(mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]<=0.01&mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]>0.001){
    est4<-paste("$", est4, "^{**}$", sep="")
  }
  if(mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],4]<=0.001){
    est4<-paste("$", est4, "^{***}$", sep="")
  }
  if(mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]>0.1){
    est5<-paste("$", est5, "$", sep="")
  }
  if(mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]<=0.1&mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]>0.05){
    est5<-paste("$", est5, "^{+}$", sep="")
  }
  if(mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]<=0.05&mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]>0.01){
    est5<-paste("$", est5, "^{*}$", sep="")
  }
  if(mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]<=0.01&mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]>0.001){
    est5<-paste("$", est5, "^{**}$", sep="")
  }
  if(mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],4]<=0.001){
    est5<-paste("$", est5, "^{***}$", sep="")
  }
  if(mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]>0.1){
    est6<-paste("$", est6, "$", sep="")
  }
  if(mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]<=0.1&mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]>0.05){
    est6<-paste("$", est6, "^{+}$", sep="")
  }
  if(mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]<=0.05&mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]>0.01){
    est6<-paste("$", est6, "^{*}$", sep="")
  }
  if(mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]<=0.01&mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]>0.001){
    est6<-paste("$", est6, "^{**}$", sep="")
  }
  if(mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],4]<=0.001){
    est6<-paste("$", est6, "^{***}$", sep="")
  }
  return(c(est1, est2, est3, est4, est5, est6))
}



sefn<-function(i, j){
  mat1<-get(paste("rmat", i, sep=""))
  mat2<-get(paste("rmat", i+1, sep=""))
  mat3<-get(paste("rmat", i+2, sep=""))
  mat4<-get(paste("rmat", i+3, sep=""))
  mat5<-get(paste("rmat", i+4, sep=""))
  mat6<-get(paste("rmat", i+5, sep=""))
  se1<-as.character(formatC(mat1[(1:length(rownames(mat1)))[rownames(mat1)==j],2], format="fg", flag="#", digits=2))
  se2<-as.character(formatC(mat2[(1:length(rownames(mat2)))[rownames(mat2)==j],2], format="fg", flag="#", digits=2))
  se3<-as.character(formatC(mat3[(1:length(rownames(mat3)))[rownames(mat3)==j],2], format="fg", flag="#", digits=2))
  se4<-as.character(formatC(mat4[(1:length(rownames(mat4)))[rownames(mat4)==j],2], format="fg", flag="#", digits=2))
  se5<-as.character(formatC(mat5[(1:length(rownames(mat5)))[rownames(mat5)==j],2], format="fg", flag="#", digits=2))
  se6<-as.character(formatC(mat6[(1:length(rownames(mat6)))[rownames(mat6)==j],2], format="fg", flag="#", digits=2))
  se1<-paste("$(", se1, ")$", sep="")
  se2<-paste("$(", se2, ")$", sep="")
  se3<-paste("$(", se3, ")$", sep="")
  se4<-paste("$(", se4, ")$", sep="")
  se5<-paste("$(", se5, ")$", sep="")
  se6<-paste("$(", se6, ")$", sep="")
  return(c(se1, se2, se3, se4, se5, se6))
}



###[SET 1] Real-world postdiction abstention analyses with preregistered variables####

model1 <- lm.cluster(data=postdict_good_trials, formula = ln_perc_abstensions ~ body_average_first_fix_duration + modified_body_word_count  + position_within_other_proposals + clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
rmat1<-summary(model1)

model2 <- lm.cluster(data=postdict_good_trials, formula = ln_perc_abstensions ~ body_average_first_pass_fix + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
rmat2<-summary(model2)

model3 <- lm.cluster(data=postdict_good_trials, formula = ln_perc_abstensions ~ body_average_first_pass_duration  + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
rmat3<-summary(model3)

model4 <- lm.cluster(data=postdict_good_trials, formula = ln_perc_abstensions ~ body_average_regression + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
rmat4<-summary(model4)

model5 <- lm.cluster(data=postdict_good_trials, formula = ln_perc_abstensions ~ body_average_fix + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
rmat5<-summary(model5)

model6 <- lm.cluster(data=postdict_good_trials, formula = ln_perc_abstensions ~ body_average_duration + modified_body_word_count + position_within_other_proposals +  clauses_per_sentence_body  + income  + education  + political_knowledge  + age  + female  + norming_salience  + norming_importance  + norming_interest, cluster= "subj_id")
rmat6<-summary(model6)



###TABLE S1

tableS1<-cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{Eye Movement Measures}", "", "", "", "", "", "")
tableS1<-rbind(tableS1, cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. first fixation duration}}", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("&", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. first pass fixations}}", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("&", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. first pass fixation duration}}", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("&", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. regression fixations}}", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("&", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. total fixations}}", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("&", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. total fixation duration}}", "--", "--", "--", "--", "--", "--"))
tableS1<-rbind(tableS1, cbind("&", "--", "--", "--", "--", "--", "--"))

est<-as.character(formatC(rmat1[2,1], format="fg", flag="#", digits=2))
if(rmat1[2,4]>.1){est<-paste("$", est, "$", sep="")}
if(rmat1[2,4]<=.1 & rmat1[2,4]>.05){est<-paste("$", est, "^{+}$", sep="")}
if(rmat1[2,4]<=.05 & rmat1[2,4]>.01){est<-paste("$", est, "^{*}$", sep="")}
if(rmat1[2,4]<=.01 & rmat1[2,4]>.001){est<-paste("$", est, "^{**}$", sep="")}
if(rmat1[2,4]<=.001){est<-paste("$", est, "^{***}$", sep="")}
tableS1[2,2]<-est
tableS1[3,2]<-paste("(", as.character(formatC(rmat1[2,2], format="fg", flag="#", digits=2)), ")", sep="")

est<-as.character(formatC(rmat2[2,1], format="fg", flag="#", digits=2))
if(rmat2[2,4]>.1){est<-paste("$", est, "$", sep="")}
if(rmat2[2,4]<=.1 & rmat2[2,4]>.05){est<-paste("$", est, "^{+}$", sep="")}
if(rmat2[2,4]<=.05 & rmat2[2,4]>.01){est<-paste("$", est, "^{*}$", sep="")}
if(rmat2[2,4]<=.01 & rmat2[2,4]>.001){est<-paste("$", est, "^{**}$", sep="")}
if(rmat2[2,4]<=.001){est<-paste("$", est, "^{***}$", sep="")}
tableS1[4,3]<-est
tableS1[5,3]<-paste("(", as.character(formatC(rmat2[2,2], format="fg", flag="#", digits=2)), ")", sep="")

est<-as.character(formatC(rmat3[2,1], format="fg", flag="#", digits=2))
if(rmat3[2,4]>.1){est<-paste("$", est, "$", sep="")}
if(rmat3[2,4]<=.1 & rmat3[2,4]>.05){est<-paste("$", est, "^{+}$", sep="")}
if(rmat3[2,4]<=.05 & rmat3[2,4]>.01){est<-paste("$", est, "^{*}$", sep="")}
if(rmat3[2,4]<=.01 & rmat3[2,4]>.001){est<-paste("$", est, "^{**}$", sep="")}
if(rmat3[2,4]<=.001){est<-paste("$", est, "^{***}$", sep="")}
tableS1[6,4]<-est
tableS1[7,4]<-paste("(", as.character(formatC(rmat3[2,2], format="fg", flag="#", digits=2)), ")", sep="")

est<-as.character(formatC(rmat4[2,1], format="fg", flag="#", digits=2))
if(rmat4[2,4]>.1){est<-paste("$", est, "$", sep="")}
if(rmat4[2,4]<=.1 & rmat4[2,4]>.05){est<-paste("$", est, "^{+}$", sep="")}
if(rmat4[2,4]<=.05 & rmat4[2,4]>.01){est<-paste("$", est, "^{*}$", sep="")}
if(rmat4[2,4]<=.01 & rmat4[2,4]>.001){est<-paste("$", est, "^{**}$", sep="")}
if(rmat4[2,4]<=.001){est<-paste("$", est, "^{***}$", sep="")}
tableS1[8,5]<-est
tableS1[9,5]<-paste("(", as.character(formatC(rmat4[2,2], format="fg", flag="#", digits=2)), ")", sep="")

est<-as.character(formatC(rmat5[2,1], format="fg", flag="#", digits=2))
if(rmat5[2,4]>.1){est<-paste("$", est, "$", sep="")}
if(rmat5[2,4]<=.1 & rmat5[2,4]>.05){est<-paste("$", est, "^{+}$", sep="")}
if(rmat5[2,4]<=.05 & rmat5[2,4]>.01){est<-paste("$", est, "^{*}$", sep="")}
if(rmat5[2,4]<=.01 & rmat5[2,4]>.001){est<-paste("$", est, "^{**}$", sep="")}
if(rmat5[2,4]<=.001){est<-paste("$", est, "^{***}$", sep="")}
tableS1[10,6]<-est
tableS1[11,6]<-paste("(", as.character(formatC(rmat5[2,2], format="fg", flag="#", digits=2)), ")", sep="")

est<-as.character(formatC(rmat6[2,1], format="fg", flag="#", digits=2))
if(rmat6[2,4]>.1){est<-paste("$", est, "$", sep="")}
if(rmat6[2,4]<=.1 & rmat6[2,4]>.05){est<-paste("$", est, "^{+}$", sep="")}
if(rmat6[2,4]<=.05 & rmat6[2,4]>.01){est<-paste("$", est, "^{*}$", sep="")}
if(rmat6[2,4]<=.01 & rmat6[2,4]>.001){est<-paste("$", est, "^{**}$", sep="")}
if(rmat6[2,4]<=.001){est<-paste("$", est, "^{***}$", sep="")}
tableS1[12,7]<-est
tableS1[13,7]<-paste("(", as.character(formatC(rmat6[2,2], format="fg", flag="#", digits=2)), ")", sep="")

#hline

tableS1<-rbind(tableS1, cbind("\\midrule\\addlinespace[1ex]\\multicolumn{2}{l}{Ballot Characteristic Covariates}", "", "", "", "", "", ""))

tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Word count}}", estfn(1, "modified_body_word_count"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "modified_body_word_count"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\rule{4pt}{0pt}Position within other proposals}", estfn(1, "position_within_other_proposals"))))
tableS1<-rbind(tableS1, rbind(c("\\multicolumn{2}{l}{\\rule{8pt}{0pt}in real-world ballot}", sefn(1, "position_within_other_proposals"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Number of clauses per sentence}}", estfn(1, "clauses_per_sentence_body"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "clauses_per_sentence_body"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. norming familiarity rating}}", estfn(1, "norming_salience"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "norming_salience"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. norming importance rating}}", estfn(1, "norming_importance"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "norming_importance"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Avg. norming interest rating}}", estfn(1, "norming_interest"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "norming_interest"))))

#hline

tableS1<-rbind(tableS1, cbind("\\midrule\\addlinespace[1ex]\\multicolumn{2}{l}{Demographic Covariates}", "", "", "", "", "", ""))

tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Participant's income}}", estfn(1, "income"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "income"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Participant's level of education}}", estfn(1, "education"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "education"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Participant's political knowledge score}}", estfn(1, "political_knowledge"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "political_knowledge"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Participant's age}}", estfn(1, "age"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "age"))))
tableS1<-rbind(tableS1, rbind(c("\\addlinespace[1ex]\\multicolumn{2}{l}{\\multirow{2}{*}{\\rule{4pt}{0pt}Participant's sex}}", estfn(1, "female"))))
tableS1<-rbind(tableS1, rbind(c("&", sefn(1, "female"))))

#hline

tableS1<-rbind(tableS1, cbind("\\midrule\\addlinespace[1ex]\\multicolumn{2}{l}{$N$}", length(model1$lm_res$residuals), length(model2$lm_res$residuals), length(model3$lm_res$residuals), length(model4$lm_res$residuals), length(model5$lm_res$residuals), length(model6$lm_res$residuals)))
tableS1<-rbind(tableS1, cbind("\\addlinespace[1ex]\\multicolumn{2}{l}{$R^{2}_{adj}$}", formatC(summary(model1$lm_res)$adj.r.squared, format="fg", flag="#", digits=2), formatC(summary(model2$lm_res)$adj.r.squared, format="fg", flag="#", digits=2), formatC(summary(model3$lm_res)$adj.r.squared, format="fg", flag="#", digits=2), formatC(summary(model4$lm_res)$adj.r.squared, format="fg", flag="#", digits=2), formatC(summary(model5$lm_res)$adj.r.squared, format="fg", flag="#", digits=2), formatC(summary(model6$lm_res)$adj.r.squared, format="fg", flag="#", digits=2)))



colnames(tableS1)<-c("&", "\\textbf{Model 1}", "\\textbf{Model 2}", "\\textbf{Model 3}", "\\textbf{Model 4}", "\\textbf{Model 5}", "\\textbf{Model 6}")

tabS1 <- xtable(tableS1)
align(tabS1) <- "lccccccc"
caption(tabS1) <- "Study 1 Abstention Analyses with Preregistered Covariates"
print(tabS1,
  tabular.environment = "tabularx",
  width = "\\linewidth",
  include.rownames = FALSE,
  sanitize.text.function = function(x) {x},
  caption.placement = "top",
  booktabs = TRUE,
  size="\\fontsize{8pt}{10pt}\\selectfont"
)
