library('e1071');
library('SparseM');
library('tm');

# LOAD DATA FROM CSV
news <- read.csv("/Users/timjurka/French_news1_csv.csv");

# CREATE DATA FRAME OF 1000 TRAINING ARTICLES AND 500
# TEST ARTICLES INCLUDING 'text' AND 'Code' (columns 4 and 6)
traindata <- as.data.frame(news[1:100,c(3,6)]);
testdata <- as.data.frame(news[101:200,c(3,6)]);

# SEPARATE TEXT VECTOR TO CREATE Source(),
# Corpus() CONSTRUCTOR FOR DOCUMENT TERM
# MATRIX TAKES Source()
trainvector <- as.vector(traindata$headline);
testvector <- as.vector(testdata$headline);

# DEBUGGING
# is.vector(trainvector);
# is.vector(testvector);

# CREATE SOURCE FOR VECTORS
trainsource <- VectorSource(trainvector);
testsource <- VectorSource(testvector);

# CREATE CORPUS FOR DATA
traincorpus <- Corpus(trainsource)
testcorpus <- Corpus(testsource)

# STEM WORDS, REMOVE STOPWORDS, TRIM WHITESPACE
traincorpus <- tm_map(traincorpus,stripWhitespace)
traincorpus <- tm_map(traincorpus,tolower)
system.time(
  traincorpus <- tm_map(traincorpus, removeWords,stopwords("french"))
)

testcorpus <- tm_map(testcorpus,stripWhitespace)
testcorpus <- tm_map(testcorpus,tolower)
system.time(
  testcorpus <- tm_map(testcorpus, removeWords,stopwords("french"))
)

# CREATE DOCUMENT TERM MATRIX
trainmatrix <- t(TermDocumentMatrix(traincorpus));
testmatrix <- t(TermDocumentMatrix(testcorpus));

# TRAIN NAIVE BAYES MODEL USING trainmatrix DATA AND traindate$Code CLASS VECTOR
model <- naiveBayes(as.matrix(trainmatrix),as.factor(traindata$Code));

# PREDICTION
results <- predict(model,as.matrix(testmatrix));