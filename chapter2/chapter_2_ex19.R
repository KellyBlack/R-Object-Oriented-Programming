measured <- ts(c(1,3,6,10),start=1.0,end=4.0)
measured
lag(measured,2)-measured
diff(measured,2)
