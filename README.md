# MRS
Movie Recommendation System
Recommender systems have become ubiquitous in our lives. Yet,
currently, they are far from optimal. In this project, we attempt
to understand the different kinds of recommendation systems and
compare their performance on the MovieLens dataset. We attempt
to build a scalable model to perform this analysis. We start by
preparing and comparing the various models on a smaller dataset
of 100,000 ratings. Then, we try to scale the algorithm so that it is
able to handle 20 million ratings by using Apache Spark. We find
that for the smaller dataset, using user-based collaborative filtering
results in the lowest Mean Squared Error on our dataset.
     The basic idea of CFR systems is that, if two users share the same interests in the past, e.g. they liked the same book or the same movie, they will also have similar tastes in the future. If, for example, user A and user B have a similar purchase history and user A recently bought a book that user B has not yet seen, the basic idea is to propose this book to user B.
     The collaborative filtering approach considers only user preferences and does not take into account the features or contents of the items (books or movies) being recommended.
