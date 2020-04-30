@Anustup Mukherjee
#ML model implements Random Forest Classifiers for the case classification
#ML model implemnts  Logistic Regression for the prediction of Suicide
#Accuracy 94.7 %
#R script 
#for working on this script download R -studio 


library(data.table)
library(ggplot2)
library(ggrepel)
library(plotly)
library(ROCR)
library(MASS)
library(randomForest)
df <-fread(r"C:\Users\Anustup\Downloads\foreveralone.csv")
margin.table(table(df$gender, df$sexuallity),c(1,2))
prop.table(table(df$gender, df$sexuallity),1) 
prop.table(table(df$gender, df$sexuallity),1) 
df <- df[gender == 'Transgender female', gender:='Female']
df <- df[gender == 'Transgender male', gender:='Male']

Female_Age_Mean <- mean(df[gender == 'Female',age])
Male_Age_Mean <- mean(df[gender == 'Male',age])

ggplotly(
ggplot(data = df, aes(x = age, fill = gender)) + 
  ggtitle("Distribution of respondents by age") + 
  labs(x = "Age", y = "Number of respondents") +
  geom_histogram(bins = 100
                 , alpha = 0.7
                 , binwidth = 1
                 , col = 'black') + 
  scale_fill_manual(values=c("red", "sky blue")) +
  scale_x_continuous(breaks=seq(0 , max(df[,age]), 5)) +
  scale_y_continuous(breaks=seq(0 , 50, 2 )) + 
  geom_vline(aes(xintercept = Female_Age_Mean), colour="red") +
  geom_vline(aes(xintercept = Male_Age_Mean), colour="sky blue")
)
ggplotly(
  ggplot(data = df, aes(x = age, fill = gender)) + 
    ggtitle("Density of distribution (respondents by age)") + 
    labs(x = "Age", y = "Number of respondents") +
    geom_density(alpha = 0.7
                   , binwidth = 1
                   , position="identity"
                   , col = 'black') + 
    scale_fill_manual(values=c("red", "sky blue")) +
    scale_x_continuous(breaks=seq(0 , max(df[,age]), 5))
)
race_variables <- df[,.N, by = 'race']
race_variables <- race_variables[order(-N),]
race_variables
df$race <- 
sapply(df$race, function(x){
  ifelse(x == "White non-Hispanic" |
     x == "Asian" | 
     x == "Hispanic (of any race)" |
     x == "Black", yes = x, no = "Other and mixed")
})
ggplotly(
  ggplot(data = df, aes(x = race, fill = race)) + 
    ggtitle("Respondents by Race") + 
    labs(x = "Race", y = "Number of respondents") +
    geom_bar(alpha = 0.7
                 , position="identity"
                 , col = 'black')
)
ggplotly(
  ggplot(data = df, aes(x = race, y = age, fill = race)) + 
    ggtitle("Respondents by Race and Age") + 
    labs(x = "Race", y = "Age") +
    geom_boxplot(alpha = 0.7
                 , col = 'black') + 
    scale_y_continuous(breaks=seq(0 , max(df[,age]), 5))
)
ggplotly(
  ggplot(data = df, aes(x = bodyweight, fill = bodyweight)) + 
    ggtitle("Bodyweight category by gender") + 
    labs(x = "Bodyweight category", y = "Number of respondents") +
    geom_bar(alpha = 0.7
             , position="identity"
             , col = 'black') + 
    facet_grid(.~gender)
)
friends_variables <- df[,.N, by = 'friends']
friends_variables <- friends_variables[order(-N),]
friends_variables
ggplot(data = df, aes(x = gender, y = friends)) + 
  ggtitle("Outliers by number of friends") + 
  geom_boxplot() + 
  geom_label_repel(data = df[friends > 50,]  
                   , mapping = aes(label = friends, label.size = 0, col = 'red'))
df$friends.f <- as.factor(
  sapply(df$friends, function(x){
    ifelse(x > 50, x <- "5. more then 50",
    ifelse(x > 30 & x <= 50, x <- "4. between 30 - 50",
    ifelse(x > 10 & x <= 30, x <- "3. between 10 - 30",
    ifelse(x > 0 & x <= 10, x <- "2. between 1 - 10",
    x <- "1. no friends"))))
  })
)
ggplotly(
    ggplot(data = df, aes(x = friends.f, fill = virgin)) +
    ggtitle("Virgin + Number of friends + Gender (NUMBER)") + 
    labs(x = "Number of friends", y = "Number of respondents") +
    geom_bar(alpha = 0.7
             , col = 'black') + 
    scale_fill_manual(values=c("black", "pink")) + 
    facet_grid(gender ~.)
  )
  ggplotly(
    ggplot(data = df, aes(x = friends.f, fill = virgin)) +
    ggtitle("Virgin + Number of friends + Gender (SHARE)") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position = 'fill'
             , col = 'black') + 
    scale_fill_manual(values=c("black", "pink")) + 
    facet_grid(gender ~.)
  )
  ggplotly(
  ggplot(data = df, aes(x = friends.f, fill = social_fear)) + 
    ggtitle("Number of friends + Social Fear") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red"))
)
ggplotly(
  ggplot(data = df, aes(x = friends.f, fill = depressed)) + 
    ggtitle("Number of friends + Depressed factor") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red"))
)
ggplotly(
  ggplot(data = df, aes(x = friends.f, fill = attempt_suicide)) + 
    ggtitle("Number of friends + AttemptSuicide") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red"))
)
temp_income <- data.table(unique(df$income))
temp_income[,income.f:="-"]
temp_income <- temp_income[order(V1)]
temp income
temp_income[1,2] <- paste('01',temp_income[1,1], sep = ') ')
temp_income[2,2] <- paste('02',temp_income[2,1], sep = ') ')
temp_income[3,2] <- paste('03',temp_income[3,1], sep = ') ')
temp_income[8,2] <- paste('04',temp_income[8,1], sep = ') ')
temp_income[10,2] <- paste('05',temp_income[10,1], sep = ') ')
temp_income[11,2] <- paste('06',temp_income[11,1], sep = ') ')
temp_income[12,2] <- paste('07',temp_income[12,1], sep = ') ')
temp_income[13,2] <- paste('08',temp_income[13,1], sep = ') ')
temp_income[4,2] <- paste('09',temp_income[4,1], sep = ') ')
temp_income[5,2] <- paste('10',temp_income[5,1], sep = ') ')
temp_income[6,2] <- paste('11',temp_income[6,1], sep = ') ')
temp_income[7,2] <- paste('12',temp_income[7,1], sep = ') ')
temp_income[9,2] <- paste('13',temp_income[9,1], sep = ') ')

temp_income$max_income <- sapply(temp_income$V1, function(x){
  temp <- strsplit(x, split = ' ')
  sapply(temp, function(y){y[3]
  })
})

temp_income$max_income[1] <- '$0'
temp_income$max_income[9] <- '$200,000'
temp_income$max_income <- as.integer(gsub(',','', substr(temp_income$max_income, start = 2 ,8)))
temp_income[order(income.f)]
df <- merge(x = df, y = temp_income, by.x = "income", by.y = "V1", all.x = TRUE)


ggplot(data = df, aes(x = income.f, fill = attempt_suicide)) + 
  ggtitle("Income category + AttemptSuicide") + 
  labs(x = "Number of friends", y = "Share of respondents") +
  geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
  scale_fill_manual(values=c("sky blue", "red")) + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=6))
  ggplotly(
  ggplot(data = df, aes(x = employment, fill = attempt_suicide)) + 
      ggtitle("Employment + AttemptSuicide") + 
  labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red")) + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8))
)
ggplotly(
  ggplot(data = df, aes(x = employment, y = max_income, fill = employment)) + 
    ggtitle("Income by Employment") + 
    labs(x = "employment", y = "max_income") +
    geom_boxplot(alpha = 0.7
                 , col = 'black'
                 , show.legend = F) + 
        theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=7))
)
train_data <- data.table(df, stringsAsFactors = TRUE)
prediction_model_glm <- glm(attempt_suicide ~ 
                              gender + 
                              sexuallity + 
                              age + 
                              race + 
                              bodyweight + 
                              virgin + 
                              social_fear + 
                              depressed + 
                              friends.f + 
                              income.f
                            , family = "binomial"
                            , data = train_data)
stepAIC(prediction_model_glm, direction = 'backward')
prediction_model_glm_final <- glm(formula = attempt_suicide ~ gender + sexuallity + race + 
                                    depressed, family = "binomial", data = train_data)


summary(prediction_model_glm_final)
train_data$prob_full <- predict(prediction_model_glm, train_data, type = 'response')
train_data$prob <- predict(prediction_model_glm_final, train_data, type = 'response')
pred_fit <- prediction(train_data$prob, train_data$attempt_suicide)
perf_fit <- performance(pred_fit, "tpr", "fpr")

auc  <- performance(pred_fit, measure = "auc")
auc <- auc@y.values[[1]]

plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.25))
lines(c(0,1),c(0,1))
text(0.6,0.2,paste("AUC = ", round(auc,4), sep=""), cex=1.4)
set.seed(122)
rf_model <- randomForest(attempt_suicide ~ 
                  gender + 
                  sexuallity + 
                  age + 
                  race + 
                  bodyweight + 
                  virgin + 
                  social_fear + 
                  depressed + 
                  friends.f + 
                  income.f
                , data = train_data
                , mtry = 3)
importance(rf_model)
print(rf_model)
---
title: "Suicide attempt prediction"
author: 'Anton Aksyonov'
date: '12.05.2017'
output:
  html_document:
    number_sections: true
    toc: true
    fig_width: 9
    fig_height: 5
    theme: yeti
    highlight: textmate
---

# Intro
The purpose of this study is to conduct a step-by-step analysis of the data set and identify factors that affect suicide attempts among respondents. As models predict I will be using logistic regression and random forest.

# Exploring step by step
I will conduct a study of the data set of data in succession for each field. If necessary, creating new attributes and dimensions.

## Load librarys & data
```{r load librarys & data, message=FALSE, warning=FALSE}
library(data.table)
library(ggplot2)
library(ggrepel)
library(plotly)
library(ROCR)
library(MASS)
library(randomForest)

df <- fread("../input/foreveralone.csv")
```

## GENDER, AGE AND SEXUALLITY


```{r, message=FALSE, warning=FALSE}
margin.table(table(df$gender, df$sexuallity),c(1,2))
```

```{r, message=FALSE, warning=FALSE}
prop.table(table(df$gender, df$sexuallity),1) 
```

```{r, message=FALSE, warning=FALSE}
prop.table(table(df$gender, df$sexuallity),2)
```

```{r, message=FALSE, warning=FALSE}
df <- df[gender == 'Transgender female', gender:='Female']
df <- df[gender == 'Transgender male', gender:='Male']

Female_Age_Mean <- mean(df[gender == 'Female',age])
Male_Age_Mean <- mean(df[gender == 'Male',age])

ggplotly(
ggplot(data = df, aes(x = age, fill = gender)) + 
  ggtitle("Distribution of respondents by age") + 
  labs(x = "Age", y = "Number of respondents") +
  geom_histogram(bins = 100
                 , alpha = 0.7
                 , binwidth = 1
                 , col = 'black') + 
  scale_fill_manual(values=c("red", "sky blue")) +
  scale_x_continuous(breaks=seq(0 , max(df[,age]), 5)) +
  scale_y_continuous(breaks=seq(0 , 50, 2 )) + 
  geom_vline(aes(xintercept = Female_Age_Mean), colour="red") +
  geom_vline(aes(xintercept = Male_Age_Mean), colour="sky blue")
)
```

```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = age, fill = gender)) + 
    ggtitle("Density of distribution (respondents by age)") + 
    labs(x = "Age", y = "Number of respondents") +
    geom_density(alpha = 0.7
                   , binwidth = 1
                   , position="identity"
                   , col = 'black') + 
    scale_fill_manual(values=c("red", "sky blue")) +
    scale_x_continuous(breaks=seq(0 , max(df[,age]), 5))
)
```

## RACE
```{r, message=FALSE, warning=FALSE}
race_variables <- df[,.N, by = 'race']
race_variables <- race_variables[order(-N),]
race_variables
```

```{r, message=FALSE, warning=FALSE}
df$race <- 
sapply(df$race, function(x){
  ifelse(x == "White non-Hispanic" |
     x == "Asian" | 
     x == "Hispanic (of any race)" |
     x == "Black", yes = x, no = "Other and mixed")
})
```

```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = race, fill = race)) + 
    ggtitle("Respondents by Race") + 
    labs(x = "Race", y = "Number of respondents") +
    geom_bar(alpha = 0.7
                 , position="identity"
                 , col = 'black')
)
```

```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = race, y = age, fill = race)) + 
    ggtitle("Respondents by Race and Age") + 
    labs(x = "Race", y = "Age") +
    geom_boxplot(alpha = 0.7
                 , col = 'black') + 
    scale_y_continuous(breaks=seq(0 , max(df[,age]), 5))
)
```

## BODYWEIGHT

```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = bodyweight, fill = bodyweight)) + 
    ggtitle("Bodyweight category by gender") + 
    labs(x = "Bodyweight category", y = "Number of respondents") +
    geom_bar(alpha = 0.7
             , position="identity"
             , col = 'black') + 
    facet_grid(.~gender)
)
```

## FRIEND with VIRGIN, SOCIAL FEAR, DEPRESSED, ATTEMPT SUICIDE

```{r, message=FALSE, warning=FALSE}
friends_variables <- df[,.N, by = 'friends']
friends_variables <- friends_variables[order(-N),]
friends_variables
```

```{r, message=FALSE, warning=FALSE}
ggplot(data = df, aes(x = gender, y = friends)) + 
  ggtitle("Outliers by number of friends") + 
  geom_boxplot() + 
  geom_label_repel(data = df[friends > 50,]  
                   , mapping = aes(label = friends, label.size = 0, col = 'red'))
```

```{r, message=FALSE, warning=FALSE}
df$friends.f <- as.factor(
  sapply(df$friends, function(x){
    ifelse(x > 50, x <- "5. more then 50",
    ifelse(x > 30 & x <= 50, x <- "4. between 30 - 50",
    ifelse(x > 10 & x <= 30, x <- "3. between 10 - 30",
    ifelse(x > 0 & x <= 10, x <- "2. between 1 - 10",
    x <- "1. no friends"))))
  })
)
```


```{r, message=FALSE, warning=FALSE}
ggplotly(
    ggplot(data = df, aes(x = friends.f, fill = virgin)) +
    ggtitle("Virgin + Number of friends + Gender (NUMBER)") + 
    labs(x = "Number of friends", y = "Number of respondents") +
    geom_bar(alpha = 0.7
             , col = 'black') + 
    scale_fill_manual(values=c("black", "pink")) + 
    facet_grid(gender ~.)
  )

ggplotly(
    ggplot(data = df, aes(x = friends.f, fill = virgin)) +
    ggtitle("Virgin + Number of friends + Gender (SHARE)") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position = 'fill'
             , col = 'black') + 
    scale_fill_manual(values=c("black", "pink")) + 
    facet_grid(gender ~.)
  )
```


```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = friends.f, fill = social_fear)) + 
    ggtitle("Number of friends + Social Fear") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red"))
)
```


```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = friends.f, fill = depressed)) + 
    ggtitle("Number of friends + Depressed factor") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red"))
)
```


```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = friends.f, fill = attempt_suicide)) + 
    ggtitle("Number of friends + AttemptSuicide") + 
    labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red"))
)

```

## INCOME & EMPLOYMENT

```{r, message=FALSE, warning=FALSE}
temp_income <- data.table(unique(df$income))
temp_income[,income.f:="-"]
temp_income <- temp_income[order(V1)]

temp_income
```

```{r, message=FALSE, warning=FALSE}
temp_income[1,2] <- paste('01',temp_income[1,1], sep = ') ')
temp_income[2,2] <- paste('02',temp_income[2,1], sep = ') ')
temp_income[3,2] <- paste('03',temp_income[3,1], sep = ') ')
temp_income[8,2] <- paste('04',temp_income[8,1], sep = ') ')
temp_income[10,2] <- paste('05',temp_income[10,1], sep = ') ')
temp_income[11,2] <- paste('06',temp_income[11,1], sep = ') ')
temp_income[12,2] <- paste('07',temp_income[12,1], sep = ') ')
temp_income[13,2] <- paste('08',temp_income[13,1], sep = ') ')
temp_income[4,2] <- paste('09',temp_income[4,1], sep = ') ')
temp_income[5,2] <- paste('10',temp_income[5,1], sep = ') ')
temp_income[6,2] <- paste('11',temp_income[6,1], sep = ') ')
temp_income[7,2] <- paste('12',temp_income[7,1], sep = ') ')
temp_income[9,2] <- paste('13',temp_income[9,1], sep = ') ')

temp_income$max_income <- sapply(temp_income$V1, function(x){
  temp <- strsplit(x, split = ' ')
  sapply(temp, function(y){y[3]
  })
})

temp_income$max_income[1] <- '$0'
temp_income$max_income[9] <- '$200,000'
temp_income$max_income <- as.integer(gsub(',','', substr(temp_income$max_income, start = 2 ,8)))
temp_income[order(income.f)]

```

```{r, message=FALSE, warning=FALSE}
df <- merge(x = df, y = temp_income, by.x = "income", by.y = "V1", all.x = TRUE)


ggplot(data = df, aes(x = income.f, fill = attempt_suicide)) + 
  ggtitle("Income category + AttemptSuicide") + 
  labs(x = "Number of friends", y = "Share of respondents") +
  geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
  scale_fill_manual(values=c("sky blue", "red")) + 
  theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=6))

```

```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = employment, fill = attempt_suicide)) + 
      ggtitle("Employment + AttemptSuicide") + 
  labs(x = "Number of friends", y = "Share of respondents") +
    geom_bar(alpha = 0.7
             , position="fill"
             , col = 'black') + 
    scale_fill_manual(values=c("sky blue", "red")) + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=8))
)
```

```{r, message=FALSE, warning=FALSE}
ggplotly(
  ggplot(data = df, aes(x = employment, y = max_income, fill = employment)) + 
    ggtitle("Income by Employment") + 
    labs(x = "employment", y = "max_income") +
    geom_boxplot(alpha = 0.7
                 , col = 'black'
                 , show.legend = F) + 
        theme(axis.text.x  = element_text(angle=45, vjust=0.5, size=7))
)
```


# Create prediction model for suicide attempt

```{r, message=FALSE, warning=FALSE}
train_data <- data.table(df, stringsAsFactors = TRUE)
```

## Logistic regression
```{r, message=FALSE, warning=FALSE}
prediction_model_glm <- glm(attempt_suicide ~ 
                              gender + 
                              sexuallity + 
                              age + 
                              race + 
                              bodyweight + 
                              virgin + 
                              social_fear + 
                              depressed + 
                              friends.f + 
                              income.f
                            , family = "binomial"
                            , data = train_data)
```

Now I will apply step-by-step selection of the most consistent model by the AIC parameter
```{r, message=FALSE, warning=FALSE}
stepAIC(prediction_model_glm, direction = 'backward')
```

```{r, message=FALSE, warning=FALSE}
prediction_model_glm_final <- glm(formula = attempt_suicide ~ gender + sexuallity + race + 
                                    depressed, family = "binomial", data = train_data)


summary(prediction_model_glm_final)
```

```{r, message=FALSE, warning=FALSE}
train_data$prob_full <- predict(prediction_model_glm, train_data, type = 'response')
train_data$prob <- predict(prediction_model_glm_final, train_data, type = 'response')


```

### ROCR - final glm model
```{r, message=FALSE, warning=FALSE}
pred_fit <- prediction(train_data$prob, train_data$attempt_suicide)
perf_fit <- performance(pred_fit, "tpr", "fpr")

auc  <- performance(pred_fit, measure = "auc")
auc <- auc@y.values[[1]]

plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.25))
lines(c(0,1),c(0,1))
text(0.6,0.2,paste("AUC = ", round(auc,4), sep=""), cex=1.4)
```

### ROCR - glm model with all predictors 
```{r, message=FALSE, warning=FALSE}
pred_fit <- prediction(train_data$prob_full, train_data$attempt_suicide)
perf_fit <- performance(pred_fit, "tpr", "fpr")

auc  <- performance(pred_fit, measure = "auc")
auc <- auc@y.values[[1]]

plot(perf_fit, colorize=T , print.cutoffs.at = seq(0,1,by=0.25))
lines(c(0,1),c(0,1))
text(0.6,0.2,paste("AUC = ", round(auc,4), sep=""), cex=1.4)
```


## Random Forest
```{r, message=FALSE, warning=FALSE}
set.seed(122)
rf_model <- randomForest(attempt_suicide ~ 
                  gender + 
                  sexuallity + 
                  age + 
                  race + 
                  bodyweight + 
                  virgin + 
                  social_fear + 
                  depressed + 
                  friends.f + 
                  income.f
                , data = train_data
                , mtry = 3)

```

```{r, message=FALSE, warning=FALSE}
importance(rf_model)
```

```{r}
print(rf_model)
