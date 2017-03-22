library(quanteda)

# Function that returns a vector of all indices for CV. K rows, first col is test, second col is train
cross_validation_splits <- function(dfm, k) {
  fold_size <- round(ndoc(dfm) / k)
  remaining_indices <- 1:ndoc(dfm) # List of indices remaining to sample from
  cv_splits <- matrix(list(), nrow = k, ncol = 2) # Matrix of lists

  for (fold in 1:(k-1)) {
    cv_splits[[fold, 1]] <- sample(remaining_indices, fold_size, replace = FALSE)
    # Train indicies are those in our entire dfm that are not in test indicies
    cv_splits[[fold, 2]] <- (1:ndoc(dfm))[-cv_splits[[fold, 1]]]
    # Remove the test indices we already used from our remaining indices
    remaining_indices <- setdiff(remaining_indices, cv_splits[[fold, 1]])
  } 
  # For final fold, just use whater is remaining in our indices
  cv_splits[[k,1]] <- remaining_indices
  cv_splits[[k,2]] <- (1:ndoc(dfm))[-cv_splits[[k,1]]]
  return(cv_splits)
}

# Returns knn_matrix
find_nearest_neighbors <- function(dfm, test_dfm, train_dfm, num_neighbors, remove_test = TRUE) {
  # Turn our entire dfm into a tf matrix, keeping original dfm in tact
  print("Calculating tfidf scores...")
  dfm_tf <- tf(dfm, scheme = "prop")
  train_idf <- docfreq(train_dfm, scheme = "inverse")
  # Because our training set will have fewer features than our entire dfm, set idf of missing features to 0 (else returns Inf idf score)
  train_idf[!is.finite(train_idf)] <- 0
  # Our entire dfm is now tfidf-scored using our training idf values
  dfm_tfidf <- dfm_weight(dfm_tf, weights = train_idf)
  
  # Find cosine distances of our test docs compared to our entire dfmsparse. Note this matrix still contains similarities to our test docs
  print("Calculating cosine similarity matrix...")
  simil_matrix <- similarity(dfm_tfidf, docnames(test_dfm), margin = "documents", method = "cosine") # Make global, we access this matrix elsewhere
  
  row_i <- 1
  knn_matrix <- matrix(NA, nrow = length(docnames(test_dfm)), ncol = num_neighbors + 1) # +1 since first col is name of our doc
  
  # Construct new matrix of doc and all top neighbor similarities to each doc
  # knn_matrix is a matrix where first column represents our test doc, and subsequent columns in the row are its nearest neighbors
  print("Creating nearest neighbors matrix...")
  for (doc in names(simil_matrix)){
    if (remove_test) {
      # Remove all test values from our similarity matrix so we don't use them as neighbors. We only want to compare test docs to training neighbors
      # In calculating knn for just our training set, we will pass in our test and training dfm as the same dfm, so we'd keep nothing if we removed all test values
      sub <- subset(simil_matrix[[doc]], !(names(simil_matrix[[doc]]) %in% docnames(test_dfm)))
    }
    else {
      sub <- simil_matrix[[doc]]
    }
    knn_matrix[row_i, ] <- c(doc, names(sub[1:num_neighbors]))
    row_i <- row_i + 1
  }
  return(knn_matrix)
}

# Take in knn matrix, turn it into a matrix that counts how many neighbors have each label
classify_neighbors <- function(knn_matrix, tags) {
  count_matrix <- matrix(0, nrow = nrow(knn_matrix), ncol = (length(tags) + 1))
  colnames(count_matrix) <- c("docname", tags)
  
  # Presumes that our corpus is labeled with docvars corresponding to our tags accessed thru corpus$documents[[tag]]
  for (row_i in (1:nrow(knn_matrix))) {
    count_matrix[row_i, 1] <- knn_matrix[row_i, 1] # Keep name of doc in first col
    for (tag in tags) {
      # For all of a docs neighbors, examine whether any each of them have this tag.
      # The length of the list should be # neighbors that have this tag
      count_matrix[row_i, tag] <- sum(as.integer(corpus_subset(corpus, docnames(corpus) %in% knn_matrix[row_i, -1])$documents[[tag]])) # Sum() counts num TRUE
    }
  }
  return(count_matrix)
}

# Prior probability of a doc having a tag in our training set
mlknn_priors <- function(train_dfm, tags) {
  priors_matrix <- list()
  for (tag in tags) {
    # Count all documents in training set with tag
    doc_count <- sum(as.integer(corpus_subset(corpus, docnames(corpus) %in% docnames(train_dfm))$documents[[tag]]))
    priors_matrix[tag] <- (1 + doc_count) / (2 + ndoc(train_dfm))
  }
  return(priors_matrix)
}

mlknn_posteriors <- function(train_dfm, count_matrix, tags, K) {
  # Matrix values are posterior probability that exactly j neighbors (row) either have/don't have label L (col)
  posteriors_has_label_matrix <- matrix(0, nrow = K + 1, ncol = length(tags) + 1)
  colnames(posteriors_has_label_matrix) <- c("num_neighbors", tags)
  posteriors_no_label_matrix <- matrix(0, nrow = K + 1, ncol = length(tags) + 1)
  colnames(posteriors_no_label_matrix) <- c("num_neighbors", tags)
  # Create a training count_matrix to get tags of closest neighbors of everyone in our training docs.
  count_matrix <- classify_neighbors(find_nearest_neighbors(train_dfm, train_dfm, train_dfm, K, remove_test = FALSE), tags)
  for (tag in tags) {
    cat("Calculating posteriors for tag", tag, " ")
    # c[j] is the number of docs in training set that have some label L while exactly j of its KNN's also have label L
    c_match <- list(rep(0, K + 1))[[1]] # K+1 since we also look at 0 neighbor casea
    c_prime <- list(rep(0, K + 1))[[1]]
    
    for (row_i in (1:ndoc(train_dfm))) {
      delta <- as.integer(count_matrix[[row_i, tag]])
      # If the document is labeled with tag...
      if (corpus[[count_matrix[[row_i, 'docname']], tag]] == 1) {
        c_match[delta + 1] <- c_match[delta + 1] + 1 # delta+1 because our R array indices start at 1. c[1] is 0 neighbor case
      }
      else {
        c_prime[delta + 1] <- c_prime[delta + 1] + 1
      }
    }
    
    for (j in 0:K) {
      posteriors_has_label_matrix[j + 1, "num_neighbors"] <- j
      posteriors_no_label_matrix[j + 1, "num_neighbors"] <- j
      posteriors_has_label_matrix[j + 1, tag] <- (1 + c_match[j + 1]) / (K + 1 + sum(c_match)) # Laplace smoothing
      posteriors_no_label_matrix[j + 1, tag] <- (1 + c_prime[j + 1]) / (K + 1 + sum(c_prime))
    }
  }
  return(list(has_label = posteriors_has_label_matrix, 
              no_label = posteriors_no_label_matrix))
}

# Pass in test count_matrix, and priors/posteriors from training set
# Prediction matrix where rows are docs, cols are tags
mlknn_predict <- function(count_matrix, priors, posteriors, tags, K) {
  predict_matrix <- matrix(0, nrow = nrow(count_matrix), ncol = length(tags) + 1)
  colnames(predict_matrix) <- c("docname", tags)
  for (row_i in (1:nrow(count_matrix))) {
    predict_matrix[row_i, 1] <- count_matrix[[row_i, 1]]
    for (tag in tags) {
      # Posterior is calculated by seeing how many neighbors this doc has have for this tag, then seeing what that corresponds to in our posterior matrix
      prob_has_label <- priors[[tag]] * posteriors$has_label[[which(posteriors$has_label[, 'num_neighbors'] == as.integer(count_matrix[[row_i, tag]])), tag]]
      prob_no_label <- (1 - priors[[tag]]) * posteriors$no_label[[which(posteriors$no_label[, 'num_neighbors'] == as.integer(count_matrix[[row_i, tag]])), tag]]
      if (which.max(c(prob_has_label, prob_no_label)) == 1) {
        predict_matrix[row_i, tag] <- 1
      }
      else {
        predict_matrix[row_i, tag] <- 0
      }
    }
  }
  return(predict_matrix)
}


# If we are to measure how useful as a recommendation tool to Yelp, ranking labels is useful
# Rank matrix returns the probability of the doc having the tag in each position
rank_labels <- function(count_matrix, priors, posteriors, tags, K) {
  rank_matrix <- matrix(0, nrow = nrow(count_matrix), ncol = length(tags) + 1)
  colnames(rank_matrix) <- c("docname", tags)
  for(row_i in (1:nrow(count_matrix))) {
    rank_matrix[row_i, 1] <- count_matrix[[row_i, 1]]
    for (tag in tags) {
      # Posterior is calculated by seeing how many neighbors this doc has have for this tag, then seeing what that corresponds to in our posterior matrix
      prob_has_label <- priors[[tag]] * posteriors$has_label[[which(posteriors$has_label[, 'num_neighbors'] == as.integer(count_matrix[[row_i, tag]])), tag]]
      prob_no_label <- (1 - priors[[tag]]) * posteriors$no_label[[which(posteriors$no_label[, 'num_neighbors'] == as.integer(count_matrix[[row_i, tag]])), tag]]
      rank_matrix[row_i, tag] <- prob_has_label / (prob_has_label + prob_no_label)
    }
  }
  return(rank_matrix)
}

hamming_loss <- function(mlknn_predictions, tags) {
  num_labels <- length(tags)
  loss <- 0
  for(row_i in (1:nrow(mlknn_predictions))) {
    misclassifications <- 0
    prediction <- mlknn_predictions[row_i, ]
    truth <- corpus[[mlknn_predictions[[row_i, 'docname']], tags]]
    for (tag in tags) {
      if (as.integer(prediction[tag]) != truth[[tag]]) {
        misclassifications <- misclassifications + 1
      }
    }
    loss <- loss + (misclassifications / num_labels)
  }
  return(loss / nrow(mlknn_predictions))
}

# Return a list of docs where our predictions where perfect
exact_matches <- function(mlknn_predictions, tags) {
  exacts <- array()
  i <- 1
  for (row_i in (1:nrow(mlknn_predictions))) {
    misclassifications <- 0
    prediction <- mlknn_predictions[row_i, ]
    truth <- corpus[[mlknn_predictions[[row_i, 'docname']], tags]]
    for (tag in tags) {
      if (as.integer(prediction[tag]) != truth[[tag]]) {
        misclassifications <- misclassifications + 1
      }
    }
    if(misclassifications == 0) {
      exacts[i] <- mlknn_predictions[[row_i, 'docname']]
      i <- i + 1
    }
  }
  return(exacts)
}

# Return a list of docs where our predictions where all wrong
all_wrong <- function(mlknn_predictions, tags) {
  wrongs <- array()
  i <- 1
  for (row_i in (1:nrow(mlknn_predictions))) {
    falsepos <- 0
    prediction <- mlknn_predictions[row_i, ]
    truth <- corpus[[mlknn_predictions[[row_i, 'docname']], tags]]
    for (tag in tags) {
      if (as.integer(prediction[tag]) == 1 & truth[[tag]] != 1) {
        falsepos <- falsepos + 1
      }
    }
    if(falsepos == sum(as.integer(prediction[-1])) & sum(as.integer(prediction[-1]) != 0)) {
      wrongs[i] <- mlknn_predictions[[row_i, 'docname']]
      i <- i + 1
    }
  }
  return(wrongs)
}

# Accuracy doesn't account for false positive misclassifications, ie we say 1 but truth is 0
# It is total number of times our predicted 1 equalled a truth 1 divided by total number of 1s
accuracy <-function(mlknn_predictions, tags) {
  sum_accuracy <- 0
  for(row_i in (1:nrow(mlknn_predictions))) {
    prediction <- mlknn_predictions[row_i, ]
    truth <- corpus[[mlknn_predictions[[row_i, 'docname']], tags]]
    correct <- 0
    total <- 0
    for(tag in tags) {
      # If we correctly predicted a labelling
      if(as.integer(prediction[tag]) == 1 & truth[[tag]] == 1) {
        correct <- correct + 1
      }
      # Total number of labels positive labelings in both truth and predictions
      if(as.integer(prediction[tag]) == 1 | truth[[tag]] == 1) {
        total <- total + 1
      }
    }
    if(total != 0) {
      sum_accuracy <- sum_accuracy + (correct / total)
    }	
  }
  return(sum_accuracy / nrow(mlknn_predictions))
}

f1 <- function(mlknn_predictions, tags) {
  sum_score <- 0
  for(row_i in (1:nrow(mlknn_predictions))) {
    prediction <- mlknn_predictions[row_i, tags]
    truth <- corpus[[mlknn_predictions[[row_i, 'docname']], tags]]
    correct <- 0
    for(tag in tags) {
    # If we correctly predicted a labelling
      if(as.integer(prediction[tag]) == 1 & truth[[tag]] == 1) {
        correct <- correct + 1
      }
    }
    sum_score <- sum_score + ((2 * correct) / (sum(as.integer(prediction)) + sum(truth)))
  }
  return (sum_score / nrow(mlknn_predictions))
}

# How many times top predicted label is not in a set of true labels in an instance
one_error <- function(rank_matrix) {
  error <- 0
  for(row_i in (1:nrow(rank_matrix))) {
    # Find name of top label
    top_label <- names(which.max(rank_matrix[row_i, -1]))
    # Check whether true observation does not have our top label
    if(corpus[[rank_matrix[[row_i, 'docname']], top_label]] == 0) {
      error <- error + 1
    }
  }
  return(error / nrow(rank_matrix))
}

# Also useful - how far do we go down ranking list before finding all correct label on average
coverage <- function(rank_matrix, tags) {
  total <- 0
  for(row_i in (1:nrow(rank_matrix))) {
    # Find label names in our truth
    truth <- corpus[[rank_matrix[[row_i, 'docname']], tags]]
    true_labels <- names(truth[grep(1, truth)]) # Find position of 1s to get true label names
    
    # Find an ordered list of predicted label rankings
    ranked_labels <-  names(sort(rank_matrix[row_i, -1], decreasing = TRUE))
    position <- 1
    
    # Iterate over ranked label list until we find all true labels, tracking position 
    while(length(true_labels) > 0) {
      if(ranked_labels[position] %in% true_labels) {
        # Find position of the label and delete it
        true_labels <- true_labels[-match(ranked_labels[position], true_labels)]
      }
      position <- position + 1
    }
    total <- total + position - 1
  }
  return(total / nrow(rank_matrix))
}

avgprec <- function(rank_matrix, tags) {
  precision <- 0
  for(row_i in (1:nrow(rank_matrix))) {
    truth <- corpus[[rank_matrix[[row_i, 'docname']], tags]]
    true_labels <- names(truth[grep(1, truth)])
    ranked_labels <-  names(sort(rank_matrix[row_i, -1], decreasing = TRUE))
    sum <- 0
    for(lab in true_labels) {
      # Find position of the label in our rank matrix
      lab_rank <- which(ranked_labels == lab)
      # Get set of label names that rank higher than the label
      higher_ranked <- ranked_labels[1:lab_rank]
      num_higher <- length(intersect(higher_ranked, true_labels))
      sum <- sum + (num_higher / lab_rank)
    }
    precision <- precision + ((1 / length(true_labels)) * sum)
  }
  return(precision / nrow(rank_matrix))
}

# Returns our CV Splits matrix with errors in a new col 
cv_error <- function(dfm, cv_splits, tags, K) {
  errors <- matrix(0, ncol = 5, nrow = nrow(cv_splits)  + 1)
  colnames(errors) <- c("hamming", "accuracy", "one-error", "coverage", "avgprec")
  for(row_i  in (1:nrow(cv_splits))) {
    
    print(cat("Performing cross validation for fold ", row_i, " *********************"))
    test_indicies <- cv_splits[[row_i ,1]]
    train_indicies <- cv_splits[[row_i ,2]]
    test <- dfm[test_indicies, ]
    train <- dfm[train_indicies, ]
    
    print("Finding neighbors of our test set (knn_matrix)...")
    knn_matrix <- find_nearest_neighbors(dfm, test, train, K, remove_test = TRUE)
    
    print("Classifying neighbors (count_matrix)...")
    count_matrix <- classify_neighbors(knn_matrix, tags)
    
    print ("Calculating priors...")
    priors <- mlknn_priors(train, tags)
    
    print("Calculating posteriors...")
    posteriors <- mlknn_posteriors(train, count_matrix, tags, K)
    
    print("Making ML-KNN predictions...")
    predictions <<- mlknn_predict(count_matrix, priors, posteriors, tags, K)
    print(predictions)
    
    print("Calculating label rankings")
    ranks <<- rank_labels(count_matrix, priors, posteriors, tags, K)
    
    print("Calculating hamming loss...") y
    errors[row_i, 1] <- hamming_loss(predictions, tags)
    
    print("Calculating accuracy...")
    errors[row_i, 2] <- accuracy(predictions, tags)
    
    print("Calculating one-error...")
    errors[row_i, 3] <- one_error(ranks)
    
    print("Calculating coverage...")
    errors[row_i, 4] <- coverage(ranks, tags)

    print("Calculating avgprec...")
    errors[row_i, 5] <- avgprec(ranks, tags)
  }
  
  for (type in colnames(errors)) {
    errors[nrow(cv_splits) + 1, type] <- mean(errors[1:nrow(cv_splits), type])
  }
  print(errors)
  return(errors)
}