# R-Report-1
title: "R Report 1"
author: "Jeffrey Ugochukwu"
date: "6/25/2019"
output: github_document
---

```{r,echo=FALSE}
patients <- read.csv("C:/Users/ugoch/Downloads/patients.csv")
```

#I(Introduction)

My name is Jeffrey Ugochukwu and I’m from Vallejo, California. I am currently majoring in Statistical Data Science as my field of study. I am currently a freshman at UC Davis. I’m taking STA 32 because it is a class required for my major. My favorite hobby is being a musician since I’m currently a member of the Cal Aggie Marching Band-Uh. We perfom at many sporting events (football, basketball, etc…), do a lot of field show perfomances, parades, and we even do small gigs requested by specific groups or people whether it would be at the school or just the Davis community in general. Recently, I performed my first ever field show solo during the Picnic Day parade in which all of the band members would kneel at the soloists’ prescence while each soloist performs their solo in front of the audience. Coming into this class, I define statistics as an applied mathematical science in which you use quanitative values to measure the success or failure rate of a given problem so that a conclusion can be made on how to approach that problem directly.

#II(Data Regarding the Patients)

##a

The list of columns in the patients.csv is given below:

```{r,echo=FALSE}
patients_list_columns = names(patients)
patients_list_columns
```

##b

The number of rows in the patients.csv is given below:

```{r,echo=FALSE}
patients_rows = dim(patients)
patients_rows
```

##c

This is the summary of the patients.csv. This function treats categorical functions in a given dataset as a measure of the frequency per category and it treats the numeric functions as a calculation that measures the summary statistic of each given category.

```{r,echo=FALSE}
patients_sum = summary(patients)
patients_sum
```

##d

The mean of the age of the patients is given below:

```{r,echo=FALSE}
age_mean = mean(patients$age)
age_mean
```

##e

The mean and standard deviation of the age of each gender is given below:

```{r,echo=FALSE}
age_per_gender_mean = aggregate(age~gender, data = patients, mean)
age_per_gender_mean
```

##f

The boxplot of the age and marital status for each patient is given below. The graph displays the 5 number summaries of the age of patients that are divorced, married, never married, other, and widowed. We can see that the age of the patients that were divorced is roughly symmetric with all of the ages evenly distributed from the age of 24 years old to the age of 80 years old. This category has a 1st quartile of around 43 years old, a median of 54 years old, and a 3rd quartile of around 62 years old. The age of the patients that are married is also roughly symmetric with a even distribution from 20 to 80 years old. It has a 1st quartile of 40 years old, a median of 51 years old, and a 3rd quartile of 65 years old. The patients that never got married is actually skewed to the left with the majority of the values from the 1st quartile to the 3rd quartile ranging from 23 years old to 41 years old. This also has the smallest median out of all of the categories with a median of 30 years old. It also has outliers from that extend outside of the minimum max value of 68 years old. The category where the patients identified as “other” is skewed to the left with the majority of the values from the 1st quartile to the 3rd quartile ranging from 28 years old to 59 years old. It also has the 2nd smallest median with a median of 38 years old. The patients that are currently widowed have a distribution that is skewed to the right with majority of the values from the 1st quartile to the 3rd quartile ranging from 66 to 80 years old. This also has the largest median out of all of the categories with a median of 74 years old. There’s also a noticible trend that the youngest participants in terms of marital status tend to never have previouly married or they have a different situation applying to marriage since the quartile ranges (range from the 1st quartile to the 3rd) are before the age of 60 and the oldest group based on the quartile range tend to be widowed with a quartile range from 66 to 80 years old.

```{r,echo=FALSE}
bp_of_age_and_marital_staus_per_patient = boxplot(patients$age~patients$marriage, main = "Boxplot of the Age and Marital Status of each Patient", xlab = "Marital Status of each Patient", ylab = "Age of each Patient")
bp_of_age_and_marital_staus_per_patient
```

##g

The boxplot represents the height of the patients with correspondence to the weight range of each patient. All graphs that represnt the categories are all roughly symmetric, but we can see how the underweight group has the smallest spread with a range of 147.4 to 178.2 inches in height (excluding outliers). Majority of the categories have the same median of around 168 inches in height with the exception of the group that’s obese since they have a median height of 170 inches. Majority of the graphs have outliers that extend past the minimum max height range, but the obese group has an outlier that’s below the minimum min height for the range.

```{r,echo=FALSE}
bp_of_height_and_weight_range_per_patient = boxplot(patients$height~patients$obese, main = "Boxplot of the height and range of weight of the Patients", xlab = "Weight renage of the Patients", ylab = "Height of the Patients")
bp_of_height_and_weight_range_per_patient
```

##h

The pie chart below represents the categories of patients based on their marital status. It appears that half of the patients are already maried; the second majority of the patients never had a previous marriage; and the rest of the categories had an even distribution of the frequency of of the amount of patients per group.

```{r,echo=FALSE}
pie_marital_status =  pie(table(patients$marriage))
pie_marital_status
```

##i

This histogram shows the total amount of cholesterol of all of the patients. We can see here that the distribution is skewed to the right, meaning that the majority of patients have a very high total cholesterol level over the normal total amount, which is 200 mg/dl. This can be concerning because this put them at a much higher risk of heart disease in the future. The distribution is also unimodal with a single peak of the range of 175-200 mg/dl with a frequency of 1000 patients.

```{r,echo=FALSE}
hist_totalchol =  hist(patients$totalchol, main = "Histogram of the total cholesterol of the Patients", xlab = "Total Cholesterol", ylab = "Number of Patients")
hist_totalchol
```

#III(Functions in R)

##a

The fivenum() and quantile() functions both set a purpose on outputing the percentile of a value in the frequency of a dataset. The two differ in which fivenum() only gives the values of the minimum value, the 1st quartile, the median, the 3rd quartile, and the maximum value, whereas the quantile() function is more of an index of the set of data based on where you would imput a specific position on finding a specific value based on what percerntile (in this case it would be a decimal such as 0.25, 0.8, etc…) it is in.

##b

I have created a function in which I would calculate the standard deviation of the data in an given dataset that is stored in the variable, x. I have created the varibale y in which this would store the equation in which I take the mean of X with the mean() over the square root of the variance of x since the variance is the value of the standard deviation, but it’s squared as a conceptual measurement of how far the values of the data are from the mean.

```{r,echo=FALSE}
R_function = f = function(x)
{
  y=(x-mean(x))/sqrt(var(x))
  return(sqrt(var(y)))
}
x=c(1:100)
f(x)
```

#Appendex

```{r, ref.label=knitr::all_labels(),echo=TRUE,eval=FALSE}
```
