## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(tangram)

## ---- results="asis"-----------------------------------------------------
data = read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data",
        sep=",",header=F,col.names=c("age", "type_employer", "fnlwgt", "education", 
                "education_num","marital", "occupation", "relationship", "race","sex",
                "capital_gain", "capital_loss", "hr_per_week","country", "income"),
        fill=FALSE,strip.white=T)
html5(summary_table(sex ~ age + income + education_num, data),
      fragment=TRUE, inline="lancet.css", caption = "HTML5 Table Lancet Style", id="tbl4"
      )

