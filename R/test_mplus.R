# m2_a <- create_profiles_mclust(iris[, -5], n_profiles=4, model_name="EEI")
# m2_a
# summary(m2_a)
# str(m2_a)
# m2_a$parameters$variance
# m2_a$classification
# str(m2_a)
# m2_a$z
# round(m2_a$z, 3)
# round(m2_a$uncertainty, 3)
#
# # certainty: 1 - m2_a$uncertainty
#
# # x <- split(m2_a, as.factor(m2_a$classification))
# #
# # x1 <- x[[1]][, -5]
# # x2 <- x[[2]][, -5]
# #
# # var(x[[1]][, -5])
# # var(x[[2]][, -5])
# #
# # sapply((x[[1]][, -5]), mean)
# # sapply((x[[2]][, -5]), mean)
#
# m2_b <- create_profiles_mclust(iris[, -5], n_profiles=2, model_name="EEE")
# m2_b %>% group_by(classification) %>% summarize_all(mean)
#
# m2_c <- create_profiles_mclust(iris[, -5], n_profiles=2, model_name="VVV")
# m2_c %>% group_by(classification) %>% summarize_all(var)
