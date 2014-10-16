source('antActivity.R')
source('antActivityMethods.R')
source('femaleAnt.R')

female <- FemaleAnt(Position=c(1,2,3),Length=5.2)
female

female <- SetLength(female,5.6)
GetLength(female)
