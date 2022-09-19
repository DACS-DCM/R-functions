# discretiseByColumn pco 2022
# some variables are populated with <5 persons and must therefore be discretised
# To allow for updates of the dataset I set the limit to 20 persons.
# Discretised persons are assigned the most popular value in the column.
# if 13 persons have hair colour blue, 500 have blond hair and 9000 have black hair,
# the 13 persons with blue hair will be assigned black hair.
# dateCreated 20220908
# dateLastModified 20220919


discretiseByColumn<-function(tmp,column){
  mostCommon = which.max(table(tmp[[column]]))
  discretise <- (table(tmp[[column]])<20)[(table(tmp[[column]])<20)=="TRUE"]
  # replace with most popular value in column:
  tmp[[column]][tmp[[column]] %in% names(discretise)]<-mostCommon
  return(tmp)
}

library(testthat)
data(iris)
# prepare data for unit test:
discretiseByColumn(round(iris[,1:4]), "Petal.Length")
table(discretiseByColumn(round(iris[,1:4]), "Petal.Length"))
table(discretiseByColumn(round(iris[,1:4]), "Petal.Length")$Petal.Length)
length(table(discretiseByColumn(round(iris[,1:4]), "Petal.Length")$Petal.Length))

test_that("discretiseByColumn leaves 5 columns in iris.Petal.Length"
      ,{
expect_equal(length(table(discretiseByColumn(round(iris[,1:4]), "Petal.Length")$Petal.Length))
,5)
})

# prepare data for unit test:
class(iris$Petal.Length)
discretiseByColumn(round(iris[,1:4]),"Petal.Length")$Petal.Length
class(discretiseByColumn(round(iris[,1:4]),"Petal.Length")$Petal.Length)
test_that("discretiseByColumn returns same class as input iris"
  ,{expect_equal(class(discretiseByColumn(round(iris[,1:4]),"Petal.Length")$Petal.Length),
                 (class(iris$Petal.Length)))})

