list.files()


setwd('~/Google Drive/PhD 2014/2014 R Research/CSEP1')
obs = read.table('ANSS_RELM_395.dat')


df.names = c("minimum longitude", "max longitude", "min latitude",
             "max latitude", "min depth", "max depth", "min magnitude", "max magnitude", "rate" ,"mask")


# non extended 
# bird = read.table("bird_liu.neokinema-fromXML.dat"); names(bird) = df.names
# ebel = read.table("ebel.aftershock-fromXML.dat"); names(ebel) = df.names;
# kagan = read.table("kagan_et_al.aftershock-fromXML.dat"); names(kagan) = df.names;
# shen = read.table("shen_et_al.geodetic.aftershock-fromXML.dat"); names(shen) = df.names;
# helms = read.table("helmstetter_et_al.hkj.aftershock-fromXML.dat"); names(helms) = df.names;
# 

setwd('~/Google Drive/PhD 2014/2014 R Research/CSEP1/relm')

shen = read.table("shen_extended.dat", header=T);names(shen) = df.names;
helms = read.table("helmstetter_extended.dat", header=T); names(helms) = df.names;
kagan = read.table("kagan_extended.dat",header=T); names(kagan) = df.names; 

setwd('~/Google Drive/PhD 2014/2014 R Research/CSEP1/relm')
save(bird, ebel, kagan,shen, helms, obs,  file = "RELMdata.RData")
