# mitra2013
R code for Mitra et al. (2013) JASA article

# Questions

1. Do we need to try to update off-diagonal elements in `update_beta` function? I currently don't change off-diagonal elements when updating beta. Note that the last of the updating steps, in which G potentially changes, results in a corresponding change in beta matrix (specifically, in an off-diagonal element of beta matrix)

