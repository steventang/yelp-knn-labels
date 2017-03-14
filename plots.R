library(ggplot2)
library(corrplot)
library(reshape2)
library(wordcloud)

label_counts <- array()
i <- 1
for (tag in tags) {
	label_counts[i] <- sum(data[, tag])
	i <- i + 1
}

label_type <- array()
i <- 1
for (tag in tags) {
	if (tag %in% c("breakfast", "brunch", "lunch", "dinner", "dessert", "latenight")) {
		label_type[i] <- "good for"
	}
	else {
		label_type[i] <- "ambience"
	}
	i <- i + 1
}

label_counts.df <- data.frame(
  labels = tags,
  num_businesses = label_counts,
  category = label_type
)

#### Label count graph
label_count_graph <- ggplot(data=label_counts.df, aes(x = reorder(labels, num_businesses), y = num_businesses, fill = category)) +
									   geom_bar(stat="identity") +
									   coord_flip() +
									   labs(title = "Number of businesses that have a label (15,474 total businesses)\n", y = "Number of businesses", x = "Label") +
									   scale_y_continuous(breaks=c(0, 2500, 5000, 7500, 10000))

#### Label Correlation graph
cormat <- cor(data[, tags])
dd <- as.dist((1-cormat)/2)
hc <- hclust(dd) # Heirarchal clustering
cormat <-cormat[hc$order, hc$order]

melted_cormat <- melt(cormat)

cormat_plot <- ggplot(melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
							 geom_tile() + 
							 scale_fill_gradient2(low = "#cc3a0d", high = "#0ecc4a", mid = "#f7f7f7", 
							    midpoint = 0, limit = c(-.5, .5), space = "Lab", 
							    name="Correlation") +
							 theme_minimal() +
							 theme(axis.text.x = element_text(angle = 90), plot.title = element_text(hjust = 0.5),
								 axis.title.y = element_blank(), axis.title.x = element_blank(),
								 legend.key.height = unit(2, "cm")) +
							 labs(title = "Correlations between labels\n")


# The most negative correlation is casual and divey at -0.3152046
# The most positive correlation is breakfast and brunch at 0.4600965

### Errors at different values of K
# Hamming loss
k_hammings <- data.frame(
	neighbors = 1:30,
	hamming_loss = c(.09086993, 0.08793177, 0.08540279, .08417069, .08352882, 0.08308499, .08296023, .08269313, .08243045, .08202102, .08223219, .08192197, .08183994, .08230545, .08207270, .08197791, .08190031, .08184427, .08198231, .08200376, .08210276, .08186151, .08195641, .08218478, .08231822, .08196929, .08210721, .08209849, .08196490, .08202528)
)
k_hammings_plot <- ggplot(data = k_hammings, aes(x = neighbors, y = hamming_loss, group = 1, color = "hamming_loss")) +
							     geom_line() +
							     geom_point() +
							     theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
							     labs(title = "Hamming loss at different values of K\n")

# Coverage
k_coverage <- data.frame(
	neighbors = 1:30,
	coverage = c(4.318023, 4.116260, 4.010920, 3.942280, 3.887744, 3.854785, 3.814657, 3.790679, 3.780405, 3.761728, 3.748282, 3.738713, 3.727981, 3.721194, 3.711370, 3.705553, 3.698641, 3.691080, 3.686107, 3.685782, 3.686171, 3.683454, 3.682420, 3.679256, 3.676476, 3.673374, 3.671240, 3.670721, 3.673826, 3.669882)
)
k_coverage_plot <- ggplot(data = k_coverage, aes(x = neighbors, y = coverage, group = 1, color = "coverage")) +
							     geom_line() +
							     geom_point() +
							     theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
							     labs(title = "Coverage at different values of K\n")

# Average precision
k_avgprec <- data.frame(
	neighbors = 1:30,
	avgprec = c(.7929951, .8169313, .8251716, .8299008, .8325722, .8351098, .8376325, .8368828, .8381434, .8396348, .8398375, .8401522, .8410606, .8410254, .8417900, .8426388, .8423144, .8430406, .8424029, .8422678, .8426153, .8426595, .8428022, .8431199, .8419793, 0.8421888, .8419923, 0.8428063, 0.8422868, 0.8422297)
)
k_avgprec_plot <- ggplot(data = k_avgprec, aes(x = neighbors, y = avgprec, group = 1, color = "avgprec")) +
						      geom_line() +
						      geom_point() +
						      theme(legend.position = "none", plot.title = element_text(hjust = 0.5)) +
						      labs(title = "Average Precision at different values of K\n")

#### Wordclouds

set.seed(5)
tfidf_wordcloud <- textplot_wordcloud(dfm_tfidf, max.words = 100, random.order = FALSE,
									                    rot.per = .25, 
									                    colors = RColorBrewer::brewer.pal(8,"Dark2"))

set.seed(5)
rawcount_wordcloud <- textplot_wordcloud(dfm, max.words = 100, random.order = FALSE,
											                   rot.per = .25, 
											                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

data_casual <- data %>% filter(casual == 1)
dfm_casual <- dfm(corpus(data_casual$text), remove = stopwords("english"), stem = TRUE, removePunct = TRUE)
dfm_casual <- dfm_trim(dfm_casual, sparsity = .9)
dfm_casual <- dfm_weight(tf(dfm_casual, scheme = "prop"), weights = docfreq(dfm_casual, scheme = "inverse"))

set.seed(5)
casual_wordcloud <- textplot_wordcloud(dfm_casual, max.words = 100, random.order = FALSE,
									                     rot.per = .25, 
									                     colors = RColorBrewer::brewer.pal(8,"Dark2"))

data_upscale <- data %>% filter(upscale == 1)
dfm_upscale <- dfm(corpus(data_upscale$text), remove = stopwords("english"), stem = TRUE, removePunct = TRUE)
dfm_upscale <- dfm_trim(dfm_upscale, sparsity = .9)
dfm_upscale <- dfm_weight(tf(dfm_upscale, scheme = "prop"), weights = docfreq(dfm_upscale, scheme = "inverse"))

set.seed(5)
upscale_wordcloud <- textplot_wordcloud(dfm_upscale, max.words = 100, random.order = FALSE,
				  			                        rot.per = .25, 
				 				                        colors = RColorBrewer::brewer.pal(8,"Dark2"))

data_breakfast <- data %>% filter(breakfast == 1)
dfm_breakfast <- dfm(corpus(data_breakfast$text), remove = stopwords("english"), stem = TRUE, removePunct = TRUE)
dfm_breakfast <- dfm_trim(dfm_breakfast, sparsity = .9)
dfm_breakfast <- dfm_weight(tf(dfm_breakfast, scheme = "prop"), weights = docfreq(dfm_breakfast, scheme = "inverse"))

set.seed(5)
breakfast_wordcloud <- textplot_wordcloud(dfm_breakfast, max.words = 100, random.order = FALSE,
						  			                      rot.per = .25, 
						 				                      colors = RColorBrewer::brewer.pal(8,"Dark2"))

data_dinner <- data %>% filter(dinner == 1)
dfm_dinner <- dfm(corpus(data_dinner$text), remove = stopwords("english"), stem = TRUE, removePunct = TRUE)
dfm_dinner <- dfm_trim(dfm_dinner, sparsity = .9)
dfm_dinner <- dfm_weight(tf(dfm_dinner, scheme = "prop"), weights = docfreq(dfm_dinner, scheme = "inverse"))

set.seed(5)
dinner_wordcloud <- textplot_wordcloud(dfm_dinner, min.freq = 1.1844, random.order = FALSE,
					  			                     rot.per = .25, 
					 				                     colors = RColorBrewer::brewer.pal(8,"Dark2"))