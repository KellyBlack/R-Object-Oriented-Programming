socketRead <- make.socket("waterdata.usgs.gov",80)
write.socket(socketRead,"GET /ny/nwis/dv?cb_00060=on&format=rdb&site_no=04267500&referred_module=sw&period=&begin_date=2013-05-08&end_date=2014-05-08 HTTP/1.1\n");
write.socket(socketRead,"Host: waterdata.usgs.gov\n\n\n");
incoming <- read.socket(socketRead);
close.socket(socketRead)
print(incoming)
