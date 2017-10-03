# Profiling route searcher

kml_file <- KMLDocument$new(filename=here("inst","kmls","oman_route.kml"),ignore_altitude=TRUE)

Rprof(here("_devel","profile.txt"))
## some code to be profiled
kml_file$get_route_between_two_points(kml_file,1,1,57,25)
Rprof(NULL)
summaryRprof(here("_devel","profile.txt"))

x<-data.frame(long=c(1,2,3),lat=c(2,3,5))
y<-c(3,4)

distances <- rowSums((x-y)**2)