#Julian De Freitas, 2019
#Analysis script for De Freitas, Rips, & Alvarez - The limit of personal identity
#Experiment 17

## clear workspace
rm(list = ls())  

## necessary libraries
if (!require(rjson)) {install.packages("rjson"); require(rjson)}
if (!require(ggplot2)) {install.packages("ggplot2"); require(ggplot2)}
if (!require(ltm)) {install.packages("ltm"); require(ltm)} 
if (!require(heplots)) {install.packages("heplots"); require(heplots)} 
if (!require(lmtest)) {install.packages("lmtest"); require(lmtest)} 
if (!require(compute.es)) {install.packages("compute.es"); require(compute.es)}
if (!require(lsr)) {install.packages("lsr"); require(lsr)} 
if (!require(lsmeans)) {install.packages("lsmean"); require(lsmeans)}
if (!require(nnet)) {install.packages("nnet"); require(nnet)}
if (!require(mlogit)) {install.packages("mlogit"); require(mlogit)}
library(ggrepel)

##================ import data ================================================================================================

## set directory to data folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory to current directory
setwd("../data/")

files <- list.files(pattern=('*txt'))
myJSON <- lapply(files, function(x) fromJSON(txt=x)) #join into one single JSON file
(data <- lapply(myJSON, function(myJSON) { as.data.frame(myJSON) }))
data <- (data <- do.call(rbind, data))

dim(data) 

if(FALSE) {
  filenames_old <- list.files(pattern=('*txt'))
  
  # Rename all files in the files list, and rename them by renaming the part before the first "_" with the integer in order
  # Also, save the part that we renamed as well as the integers we assigned them to in a csv file, so that it's easy to see which file corresponds to which participant
  for(i in 1:length(files)) {
    # Print "Renaming [worker_id] to [num]"
    print(paste0("Renaming ", files[i], " to ", i))
    
    #file.rename(files[i], paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i]))))
    #files[i] <- paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
    # If there are files with the same worker_id, rename them to the same integer
    
    if(i > 1) {
      print(paste(substr(filenames_old[i], 1, regexpr("_", filenames_old[i])-1), substr(filenames_old[i-1], 1, regexpr("_", filenames_old[i-1])-1)))
      if(substr(filenames_old[i], 1, regexpr("_", filenames_old[i])-1) == substr(filenames_old[i-1], 1, regexpr("_", filenames_old[i-1])-1)) {
        file.rename(files[i], paste0(i-1, substr(files[i], regexpr("_", files[i]), nchar(files[i]))))
        files[i] <- paste0(i-1, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
      }
      else {
        file.rename(filenames_old[i], paste0(i, substr(filenames_old[i], regexpr("_", filenames_old[i]), nchar(filenames_old[i]))))
        files[i] <- paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
      }
    }
    else {
      file.rename(filenames_old[i], paste0(i, substr(filenames_old[i], regexpr("_", filenames_old[i]), nchar(filenames_old[i]))))
      files[i] <- paste0(i, substr(files[i], regexpr("_", files[i]), nchar(files[i])))
    }
  }
  
  # Also anonymize the 'workerID' field in each file to the integer we assigned it to, and save the file
  for(i in 1:length(files)) {
    myJSON <- fromJSON(files[i])
    
    # We should save the myJSON$workerID as the integer before the first "_"
    w_num <- substr(files[i], 1, regexpr("_", files[i])-1)
    print(paste0(files[i], " -- ", w_num))
    myJSON$workerID <- w_num
    
    # Write the text into files[i]
    write(toJSON(myJSON, na = "string"), files[i])
  }
  
  worker_ids <- c()
  nums <- c()
  for(i in 1:length(filenames_old)) {
    worker_ids <- c(worker_ids, substr(filenames_old[i], 1, regexpr("_", filenames_old[i]) - 1))
    nums <- c(nums, substr(files[i], 1, regexpr("_", files[i]) - 1))
  }
  
  filenames <- as.data.frame(cbind(worker_ids, nums))
  colnames(filenames) <- c('worker_id', 'integer')
  # Remove NA column
  filenames <- filenames[filenames$worker_id != 'NA',]
  filenames$`NA`<- NULL
  write.csv(filenames, 'filenames_e6.csv')  
}


##======================== counts and exclusions =============================================================================

#exclude those who failed attention check
data <- subset(data, (data$trialStruct.attention==0) &
                 (data$trialStruct.comp_imagine==2) &
                 (data$trialStruct.comp_num_versions==3))

dim(data)

age <- data$trialStruct.age[data$trialStruct.age %in% 1:100] #ignore folks who said they're 0 yrs old
mean(age,na.rm = TRUE) 
gender <- as.factor(data$trialStruct.sex); table(gender)[2]/sum(table(gender)) 

##======================== prep data for analysis ================================================================================

q1 <- as.numeric(data$trialStruct.q1_duplicate)
q2 <- as.numeric(data$trialStruct.q2_futures)
q3 <- as.numeric(data$trialStruct.q3_20young_20old)
q4 <- as.numeric(data$trialStruct.q4_true_surface)
q5 <- as.numeric(data$trialStruct.q5_1myoung_1mold)
q6 <- as.numeric(data$trialStruct.q6_18young_60old)
q7 <- as.numeric(data$trialStruct.q7_futures_2)
q8 <- as.numeric(data$trialStruct.q8_healthy_unhealthy)

similarities <- c(mean(q2), 
                  mean(q3),
                  mean(q4),
                  mean(q5),
                  mean(q6),
                  mean(q7),
                  mean(q8))

experiments <- c('future-you1 vs future-you2',
                 '20-years-younger vs 20-years-older',
                 'true vs surface',
                 '1-month-younger vs 1-month-older',
                 '18-year-old vs 60-year-old',
                 'future-you1 vs future-you2',
                 'healthy vs unhealthy')

#performance differences two selves - one                 
e9_alternatives <- -0.06407179 ##
e10_20yr_youngOld <- 0.1994021 ##
e11_true_surface <- 0.0412656 ##
e12_1m_youngOld <- 0.1162411 ##
e13_1860_youngOld <- 0.2832143 ##
e14_futures_2 <- -0.09078714 ##
e15_healthy_unhealthy <- 0.1069605 ##

effects <- c(e9_alternatives,
             e10_20yr_youngOld,
             e11_true_surface,
             e12_1m_youngOld, 
             e13_1860_youngOld,
             e14_futures_2,
             e15_healthy_unhealthy)

##========================================== analysis ======================================================================

cor.test(similarities, effects)

plot(similarities, effects)
abline(effects ~ similarities)

df <- data.frame(matrix(ncol = 2, nrow = 7))
colnames(df) <-  c("similarities", "effects")
df$similarities <- similarities
df$effects <- effects

##========================================== plot ======================================================================

quartz()
pc <- predict(prcomp(~effects+similarities),df)[,1]

p1 <- ggplot(df,aes(similarities, effects, color=pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE) +
  #geom_text(aes(label=experiments),hjust=1, vjust=0)+
  geom_smooth(method=lm , color="black", se=TRUE) + # Add linear regression line 
  theme_minimal() +
  scale_color_gradient(low = "#f0650e", high = "#0091ff")+
  scale_alpha(range = c(.25, .6))+
  xlab("Self Distinctiveness Rating")+ylab("Memory Accuracy for 2 Selves - 1 Self")+
      ggtitle("Memory Performance by Self Similarity") +
  theme(axis.title.x = element_text(color='black', margin=margin(20,0,0,0)), 
        axis.title.y = element_text(margin=margin(0,0,0,0)),
        plot.title = element_text(size=30, face='bold'),
        text = element_text(size=30))+
        theme(legend.position = "none")
  coord_cartesian(ylim=c(-0.5, 0.5)) 

### geom_label_repel
p1 + 
  geom_label_repel(aes(label = experiments),
                   size= 5.0,
                   box.padding   = 1.8, 
                   point.padding = 1.5,
                   segment.color = 'grey50') 

####======================================= end =========================================================

rm(list = ls()) 

