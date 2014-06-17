### R code from vignette source 'streamMOA.Rnw'

###################################################
### code chunk number 1: streamMOA.Rnw:5-6
###################################################
options(width = 75, digits = 3, prompt = 'R> ')


###################################################
### code chunk number 2: streamMOA.Rnw:31-32
###################################################
set.seed(1000) 


###################################################
### code chunk number 3: data_bng
###################################################
library("stream") 
dsd <- DSD_Wrapper(DSD_BarsAndGaussians(noise=0.05), n=1500)
dsd
plot(dsd)


###################################################
### code chunk number 4: streamMOA.Rnw:57-61
###################################################
sample <- DSC_Sample(k=100) 
window <- DSC_Window(horizon=100)
dstream <- DSC_DStream(gridsize=.7)
tNN <- DSC_tNN(r=.5)


###################################################
### code chunk number 5: streamMOA.Rnw:66-69
###################################################
library("streamMOA")
denstream <- DSC_DenStream(epsilon=.5, mu=1) 
clustream <- DSC_CluStream(m=100, k=4) 


###################################################
### code chunk number 6: streamMOA.Rnw:76-82
###################################################
algorithms <- list(Sample=sample, Window=window, 'D-Stream'=dstream, tNN=tNN, 
  DenStream=denstream, CluStream=clustream)
for(a in algorithms) {
  reset_stream(dsd) 
  cluster(a, dsd, 1000)
}


###################################################
### code chunk number 7: streamMOA.Rnw:86-87
###################################################
sapply(algorithms, nclusters)


###################################################
### code chunk number 8: microclusters
###################################################
op <- par(no.readonly = TRUE)
layout(mat=matrix(1:6, ncol=2))
for(a in algorithms) {
  reset_stream(dsd) 
  plot(a, dsd, main=a$description)
}
par(op)


###################################################
### code chunk number 9: microclusters_assignment
###################################################
op <- par(no.readonly = TRUE)
layout(mat=matrix(1:6, ncol=2))
for(a in algorithms) {
  reset_stream(dsd) 
  plot(a, dsd, main=a$description, assignment=TRUE, weight=FALSE)
}
par(op)


###################################################
### code chunk number 10: streamMOA.Rnw:169-175
###################################################
sapply(algorithms, FUN=function(a) {
  reset_stream(dsd, 1001) 
  evaluate(a, dsd, 
    measure=c("numMicroClusters", "purity", "SSQ", "silhouette"), 
    n=500, assignmentMethod="auto")
})


###################################################
### code chunk number 11: streamMOA.Rnw:198-205
###################################################
sample_km <- DSC_Kmeans(k=4, description="Sample + weighted k-means")
recluster(sample_km, algorithms$Sample)
algorithms$Sample <- sample_km 

window_km <- DSC_Kmeans(k=4, description= "Window + weighted k-means")
recluster(window_km, algorithms$Window)
algorithms$Window <- window_km 


###################################################
### code chunk number 12: macroclusters
###################################################
op <- par(no.readonly = TRUE)
layout(mat=matrix(1:6, ncol=2))
for(a in algorithms) {
  reset_stream(dsd) 
  plot(a, dsd, main=a$description, type="both")
}
par(op)


###################################################
### code chunk number 13: streamMOA.Rnw:232-237
###################################################
sapply(algorithms, FUN=function(a) {
  reset_stream(dsd, 1001) 
  evaluate(a, dsd, measure=c("numMacroClusters","purity", "SSQ", "cRand"), 
    n=500, assign="micro", type="macro")
})


###################################################
### code chunk number 14: streamMOA.Rnw:345-347
###################################################
set.seed(0)
dsd <- DSD_Wrapper(DSD_Benchmark(1), 5000)


###################################################
### code chunk number 15: moa1
###################################################
plot(dsd, 250, xlim=c(0,1), ylim=c(0,1))
arrows(.15,.85,.85,.15, col=rgb(.8,.8,.8,.6), lwd=10)
arrows(.15,.15,.85,.85, col=rgb(.8,.8,.8,.6), lwd=10)
reset_stream(dsd)


###################################################
### code chunk number 16: streamMOA.Rnw:372-379
###################################################
sample <- DSC_Sample(k=100, biased=TRUE) 
window <- DSC_Window(horizon=100, lambda=.01)

dstream <- DSC_DStream(gridsize=.05, lambda=.01)
tNN <- DSC_tNN(r=.02, lambda=.01)
denstream <- DSC_DenStream(epsilon=.05, lambda=.01) 
clustream <- DSC_CluStream(m=100, k=2) 


###################################################
### code chunk number 17: streamMOA.Rnw:389-431
###################################################
evaluation <- list()
n <- 5000
horizon <- 250
reset_stream(dsd)
evaluation[["D-Stream"]] <- evaluate_cluster(dstream, dsd, 
  type="macro", assign="micro",
  measure=c("numMicro","numMacro","SSQ", "crand"), 
  n=n, horizon=horizon)

reset_stream(dsd)
evaluation[["tNN"]] <- evaluate_cluster(tNN, dsd, 
  type="macro", assign="micro",
  measure=c("numMicro","numMacro","SSQ","crand"), 
  n=n, horizon=horizon)


reset_stream(dsd)
evaluation[["DenStream"]] <- evaluate_cluster(denstream, dsd, 
  type="macro", assign="micro",
  measure=c("numMicro","numMacro","SSQ", "crand"), 
  n=n, horizon=horizon)

reset_stream(dsd)
evaluation[["CluStream"]] <- evaluate_cluster(clustream, dsd, 
  type="macro", assign="micro",
  measure=c("numMicro","numMacro","SSQ", "crand"), 
  n=n, horizon=horizon)


reset_stream(dsd)
evaluation[["Sample"]] <- evaluate_cluster(sample, dsd, 
  macro=DSC_Kmeans(k=2), 
  type="macro", assign="micro",
  measure=c("numMicro","numMacro","SSQ", "crand"), 
  n=n, horizon=horizon)

reset_stream(dsd)
evaluation[["Window"]] <- evaluate_cluster(window, dsd, 
  macro=DSC_Kmeans(k=2), 
  type="macro", assign="micro",
  measure=c("numMicro","numMacro","SSQ", "crand"), 
  n=n, horizon=horizon)


###################################################
### code chunk number 18: dynamic
###################################################
Position <- evaluation[[1]][,"points"]
cRand <- sapply(evaluation, FUN=function(x) x[,"cRand"])
cRand
matplot(Position, cRand, type="l", lwd=2)
legend("bottomleft", legend=names(evaluation), 
  col=1:6, lty=1:6, bty="n", lwd=2)


###################################################
### code chunk number 19: dynamic_box
###################################################
boxplot(cRand, las=2)


###################################################
### code chunk number 20: dynamic2
###################################################
SSQ <- sapply(evaluation, FUN=function(x) x[,"SSQ"])
SSQ
matplot(Position, SSQ, type="l", lwd=2)


###################################################
### code chunk number 21: dynamic_box2
###################################################
boxplot(SSQ, las=2)


