source('antActivity.R')
source('antActivityMethods.R')
source('femaleAnt.R')
source('workerAnt.R')

worker <- WorkerAnt(Position=c(-1,3,5),Length=2.5)
worker

worker <- SetLength(worker,3.5)
GetLength(worker)
