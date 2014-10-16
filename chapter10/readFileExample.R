source('grades.R')
source('ops.R')
source('overriding.R')

dir(pattern="csv$")
course <- Course()
course <- SetGradeTypes(course,c("test","hw","quiz","project","final"))
course <- SetFileName(course,"shortList.csv")
course <- ReadGrades(course)

