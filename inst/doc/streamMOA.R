### R code from vignette source 'streamMOA.Rnw'

###################################################
### code chunk number 1: streamMOA.Rnw:5-6
###################################################
options(width = 75, digits = 3, prompt = 'R> ')


###################################################
### code chunk number 2: streamMOA.Rnw:31-32
###################################################
set.seed(1234)


###################################################
### code chunk number 3: data_bng
###################################################
library("streamMOA")


###################################################
### code chunk number 4: data_bng
###################################################
stream <- DSD_BarsAndGaussians(noise=0.05) %>% DSD_Memory(n = 5500)
stream
plot(stream)


###################################################
### code chunk number 5: streamMOA.Rnw:62-69
###################################################
algorithms <- list(
  'Sample + k-means' = DSC_TwoStage(micro = DSC_Sample(k = 100),
                                    macro = DSC_Kmeans(k = 4)),
  'DenStream'     = DSC_DenStream(epsilon = .5, mu = 1),
  'cluStream'     = DSC_CluStream(m = 100, k = 4),
  'Bico'          = DSC_BICO_MOA(Cluster = 4, Dimensions = 2, MaxClusterFeatures = 100)
)


###################################################
### code chunk number 6: streamMOA.Rnw:76-80
###################################################
for (a in algorithms) {
  reset_stream(stream)
  update(a, stream, 1000)
}


###################################################
### code chunk number 7: streamMOA.Rnw:84-85
###################################################
sapply(algorithms, nclusters, type = "micro")


###################################################
### code chunk number 8: microclusters
###################################################
op <- par(no.readonly = TRUE)
layout(mat = matrix(1:4, ncol = 2))
for (a in algorithms) {
  reset_stream(stream)
  plot(a, stream, main = description(a), type = "micro")
}
par(op)


###################################################
### code chunk number 9: microclusters_assignment
###################################################
op <- par(no.readonly = TRUE)
layout(mat = matrix(1:4, ncol = 2))
for (a in algorithms) {
  reset_stream(stream)
  plot(
    a,
    stream,
    main = description(a),
    assignment = TRUE,
    weight = FALSE,
    type = "micro"
  )
}
par(op)


###################################################
### code chunk number 10: streamMOA.Rnw:162-175
###################################################
sapply(
  algorithms,
  FUN = function(a) {
    reset_stream(stream, 1001)
    evaluate_static(
      a,
      stream,
      measure = c("numMicroClusters", "purity", "SSQ", "silhouette"),
      n = 500,
      assignmentMethod = "auto",
      type = "micro"
    )
})


###################################################
### code chunk number 11: outlier1
###################################################
library(stream)
set.seed(1000)
stream <- DSD_Gaussians(k = 3, d = 2,
            variance_limit = c(0.1, 1),
            space_limit = c(0, 30),
            noise = .01,
            noise_limit = c(0, 30),
            noise_separation = 6,
            separation_type = "Mahalanobis"
  ) %>% DSD_Memory(n = 1000)


###################################################
### code chunk number 12: outlier2
###################################################
plot(stream, n = 1000)


###################################################
### code chunk number 13: outlier3
###################################################
reset_stream(stream)
mic_c <- DSOutlier_MCOD(r = 2, w = 1000)
evaluate_static(
  mic_c,
  stream,
  n = 1000,
  type = "micro",
  measure = c("crand", "outlierjaccard")
)


###################################################
### code chunk number 14: outlier4
###################################################
reset_stream(stream)
plot(mic_c, stream, n = 1000)


