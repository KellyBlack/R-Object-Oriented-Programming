source('grades.R')
source('ops.R')
source('overriding.R')

course <- Course()
course <- SetFileName(course,"math100.csv")
course <- ReadGrades(course)

y <- course['test2']
y <- SetName(y,"Test 2")

z <- course['project1']
z <- SetName(z,"Project 1")

GradeReport(y,maxGrade=100)
GradeReport(z)
