library(wordcloud)

words <- c("Causality", "Dependency", "Association",
           "Missing data", "Uncertainty",
	   "Prediction", "Modeling", "Regression",
	   "Independence", "Correlation", "Discrete", "Continuous", "Categorical", "Binary",
	   "Latent factors", "Networks", "Data integration",
	   "Time series",
	   "Self-organizing map", "Visualization", "Relevance",
	   "Non-parametrics", "Clustering", "Classification",
	   "Significance", "Noise",
	   "Bayesian methods", "Sensitivity", "Specificity", "Sample size",
	   "Stochasticity", "Random variables", "Mixed effects", "Bias",
	   "Probabilities", "Components", "Statistical power")

freq <- sample(seq(3,20), length(words), replace = TRUE)

set.seed(46)

png("wordcloud.png")
wordcloud(words,freq, random.color = TRUE, rot.per = 0.25, color = rainbow(7))
dev.off()
