usgs <- socketConnection(host = "waterdata.usgs.gov",80)
writeLines("GET /ny/nwis/dv?cb_00060=on&format=rdb&site_no=04267500&referred_module=sw&period=&begin_date=2013-05-08&end_date=2014-05-08 HTTP/1.1",con=usgs)
writeLines("Host: waterdata.usgs.gov",con=usgs)
writeLines("\n\n",con=usgs)
lines = readLines(usgs)
lines
close(usgs)
