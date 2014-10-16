a <- c("Johann Carl Friedrich Gauss","Jean Baptiste Joseph Fourier","Isaac Newton","Brahmagupta")
simpler <-  sub("(\\w) [A-Z]+[a-z]* [A-Z]+[a-z]* (\\w)","\\1 \\2",a)
simpler
