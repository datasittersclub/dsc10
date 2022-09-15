# Functions for Average Descriptions of Things
# wordlist.score is a 2 column data frame. The first column is a vector of words, the second is a vector of multipliers

cleanChunk<-function(text.chunk){
  text.chunk<-paste(text.chunk, collapse=" ")
  text.chunk<-unlist(strsplit(text.chunk, ""))
  text.chunk<-tolower(text.chunk)
  text.chunk<-text.chunk[which(text.chunk %in% c(letters, " "))]
  text.chunk<-paste(text.chunk, collapse="")
  text.clean<-unlist(strsplit(text.chunk, " "))
  return(text.clean)
}

#function for calculating typicality of a corpus of texts (designed for author corpora)
corpusTypicality<-function(corpus.dir, meta.data.file, output.dir, top.words=1000, corpus.name){
  library(tm)
  all.files<-paste(corpus.dir, meta.data.file$Filename, sep="/")
  all.texts<-lapply(all.files, function(x) scan(x, what="character", sep="/", quiet=T))
  all.texts<-lapply(all.texts, function(x) paste(x, collapse=" "))
  all.texts<-lapply(all.texts, function(x) cleanChunk(x))
  all.texts<-lapply(all.texts, function(x) paste(x, collapse=" "))
  all.texts<-unlist(all.texts)
  text.corpus<-Corpus(VectorSource(all.texts))
  text.dtm<-DocumentTermMatrix(text.corpus)
  text.dtm<-as.matrix(text.dtm)
  word.freqs<-sort(colSums(text.dtm), decreasing=T)
  top.terms<-names(word.freqs[1:top.words])
  write.csv(x = top.terms, file = paste(c(output.dir, '/', 'top_terms_', corpus.name,'.csv'), collapse = ''))
  dtm.scale<-text.dtm/rowSums(text.dtm)
  dtm.mfw<-dtm.scale[,which(colnames(dtm.scale) %in% top.terms)]
  pca.coords<-prcomp(dtm.mfw)
  pca.coords<-pca.coords$x[,1:2]
  colnames(pca.coords)<-c("PC1", "PC2")
  final.table<-data.frame(pca.coords, meta.data.file)
  x.mean<-mean(pca.coords[,1])
  x.median<-median(pca.coords[,1])
  y.mean<-mean(pca.coords[,2])
  y.median<-median(pca.coords[,2])
  empty.rows<-matrix(rep(NA, (ncol(meta.data.file)*2)), nrow=2)
  x.add<-c(x.mean, x.median)
  y.add<-c(y.mean, y.median)
  add.table<-data.frame(x.add, y.add, empty.rows)
  colnames(add.table)<-colnames(final.table)
  final.table<-rbind(final.table, add.table)
  type.vector<-rep("text", nrow(final.table))
  type.vector[c(length(type.vector)-1, length(type.vector))]<-c("mean", "median")
  final.table$Type<-type.vector
  out.filename<-paste(corpus.name, "_pca.csv", sep="")
  out.filename<-paste(output.dir, out.filename, sep="/")
  write.csv(final.table, out.filename, row.names=F)
}

corpusTypicalityBiplot <- function(corpus.dir, meta.data.file, output.dir, top.words = evaluated.words, corpus.name){
  #Loads the text mining package
  library(tm)
  #Sets up a path for each file, with the corpus directory + filename
  all.files <- paste(corpus.dir, meta.data.file$Filename, sep="/")
  #Extracts text from the text files
  all.texts <- lapply(all.files, function(x) scan(x, what="character", sep="\n", quiet=T))
  #Pastes everything back together in a single string
  all.texts <- lapply(all.texts, function(x) paste(x, collapse=" "))
  #Applies Mark's special cleaning function
  all.texts <- lapply(all.texts, function(x) cleanChunk(x))
  #Pastes all texts back together in a single string
  all.texts <- lapply(all.texts, function(x) paste(x, collapse=" "))
  #Returns a list, changing the text strings into a vector
  all.texts <- unlist(all.texts)
  #Defines text.corpus as a vectorization of all the texts
  text.corpus <- Corpus(VectorSource(all.texts))
  #Creates a document-term matrix from the vectorization
  text.dtm <- DocumentTermMatrix(text.corpus)
  #Sets the document-term matrix as a matrix
  text.dtm <- as.matrix(text.dtm)
  #Defines word frequency by sorting the words in order of occurrence
  word.freqs <- sort(colSums(text.dtm), decreasing=T)
  #Defines top terms as being the # of terms that you specify when you run the code
  #The "starts.with", which you also specify when you run the code, lets you exclude the most-frequent terms
  #This might be useful if you're using all words (e.g. not just nouns) and want to
  #exclude the highest-frequency words that are mostly indicative of author signal (e.g. 'the', 'a', 'and')
  top.terms <- names(word.freqs[starts.with:top.words])
  #Writes a CSV with the top terms that it's using for its analysis
  write.csv(x = top.terms, file = paste(c(output.dir, '/', 'top_terms_', corpus.name,'.csv'), collapse = ''))
  #Converts word counts to frequencies, dividing by book length
  #Otherwise, books would look different from one another just because of different length
  dtm.scale <- text.dtm/rowSums(text.dtm)
  #Reshapes the document/term matrix to just columns with frequencies for the top words
  dtm.mfw <- dtm.scale[,which(colnames(dtm.scale) %in% top.terms)]
  #Computes PCA coordinates using the words in the range you specified for analysis
  #LITERALLY JUST ADD 'scale=T' TO THIS LINE TO SCALE IT USING Z-SCORES
  pca.coords <- prcomp(dtm.mfw)
  #Creates a PDF output for the biplot
  pdf('bsc_topnouns_biplot.pdf', height=100, width=100)
  #Creates the biplot
  biplot(pca.coords, xlabs = meta.data.file$Title)
  dev.off()
}

corpusTypicalityBiplotZscore <- function(corpus.dir, meta.data.file, output.dir, top.words = evaluated.words, corpus.name){
  #Loads the text mining package
  library(tm)
  #Sets up a path for each file, with the corpus directory + filename
  all.files <- paste(corpus.dir, meta.data.file$Filename, sep="/")
  #Extracts text from the text files
  all.texts <- lapply(all.files, function(x) scan(x, what="character", sep="\n", quiet=T))
  #Pastes everything back together in a single string
  all.texts <- lapply(all.texts, function(x) paste(x, collapse=" "))
  #Applies Mark's special cleaning function
  all.texts <- lapply(all.texts, function(x) cleanChunk(x))
  #Pastes all texts back together in a single string
  all.texts <- lapply(all.texts, function(x) paste(x, collapse=" "))
  #Returns a list, changing the text strings into a vector
  all.texts <- unlist(all.texts)
  #Defines text.corpus as a vectorization of all the texts
  text.corpus <- Corpus(VectorSource(all.texts))
  #Creates a document-term matrix from the vectorization
  text.dtm <- DocumentTermMatrix(text.corpus)
  #Sets the document-term matrix as a matrix
  text.dtm <- as.matrix(text.dtm)
  #Defines word frequency by sorting the words in order of occurrence
  word.freqs <- sort(colSums(text.dtm), decreasing=T)
  #Defines top terms as being the # of terms that you specify when you run the code
  #The "starts.with", which you also specify when you run the code, lets you exclude the most-frequent terms
  #This might be useful if you're using all words (e.g. not just nouns) and want to
  #exclude the highest-frequency words that are mostly indicative of author signal (e.g. 'the', 'a', 'and')
  top.terms <- names(word.freqs[starts.with:top.words])
  #Writes a CSV with the top terms that it's using for its analysis
  write.csv(x = top.terms, file = paste(c(output.dir, '/', 'top_terms_', corpus.name,'.csv'), collapse = ''))
  #Converts word counts to frequencies, dividing by book length
  #Otherwise, books would look different from one another just because of different length
  dtm.scale <- text.dtm/rowSums(text.dtm)
  #Reshapes the document/term matrix to just columns with frequencies for the top words
  dtm.mfw <- dtm.scale[,which(colnames(dtm.scale) %in% top.terms)]
  #Computes PCA coordinates using the words in the range you specified for analysis
  #LITERALLY JUST ADD 'scale=T' TO THIS LINE TO SCALE IT USING Z-SCORES
  pca.coords <- prcomp(dtm.mfw, scale=T)
  #Creates a PDF output for the biplot
  pdf('bsc_topnouns_biplot_scaled.pdf', height=100, width=100)
  #Creates the biplot
  biplot(pca.coords, xlabs = meta.data.file$Title)
  dev.off()
}
