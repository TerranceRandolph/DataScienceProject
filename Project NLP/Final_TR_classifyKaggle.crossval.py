'''
  This program shell reads phrase data for the kaggle phrase sentiment classification problem.
  The input to the program is the path to the kaggle directory "corpus" and a limit number.
  The program reads all of the kaggle phrases, and then picks a random selection of the limit number.
  It creates a "phrasedocs" variable with a list of phrases consisting of a pair
    with the list of tokenized words from the phrase and the label number from 1 to 4
  It prints a few example phrases.
  In comments, it is shown how to get word lists from the two sentiment lexicons:
      subjectivity and LIWC, if you want to use them in your features
  Your task is to generate features sets and train and test a classifier.

  Usage:  python classifyKaggle.py  <corpus directory path> <limit number>

  This version uses cross-validation with the Naive Bayes classifier in NLTK.
  It computes the evaluation measures of precision, recall and F1 measure for each fold.
  It also averages across folds and across labels.
'''
# open python and nltk packages needed for processing
import os
import re
import sys
import itertools
import random
import nltk
from nltk.corpus import stopwords
from nltk import FreqDist
from nltk.collocations import *
from pprint import pprint

# this code is commented off now, but can be used for sentiment lists
import sentiment_read_subjectivity as srs

#initialize the positive, neutral and negative word lists
(positivelist, neutrallist, negativelist) = srs.read_subjectivity_three_types('SentimentLexicons/subjclueslen1-HLTEMNLP05.tff')
sentiment_list = srs.readSubjectivity('SentimentLexicons/subjclueslen1-HLTEMNLP05.tff')

import sentiment_read_LIWC_pos_neg_words
from sentiment_read_LIWC_pos_neg_words import isPresent
#initialize positve and negative word prefix lists from LIWC 
#  note there is another function isPresent to test if a word's prefix is in the list
(poslist, neglist) = sentiment_read_LIWC_pos_neg_words.read_words()

# define a feature definition function here
# see line 421 ... Step 2 code

# this function define features (keywords) of a document for a BOW/unigram baseline
# each feature is 'V_(keyword)' and is true or false depending
# on whether that keyword is in the document
def document_features(document, word_features):
    document_words = set(document)
    features = {}
    for word in word_features:
        features['V_{}'.format(word)] = (word in document_words)
    return features

# cross-validation ##
# this function takes the number of folds, the feature sets and the labels
# it iterates over the folds, using different sections for training and testing in turn
#   it prints the performance for each fold and the average performance at the end
def cross_validation_PRF(num_folds, featuresets, labels):
    subset_size = int(len(featuresets)/num_folds)
    print('Each fold size:', subset_size)
    # for the number of labels - start the totals lists with zeroes
    num_labels = len(labels)
    total_precision_list = [0] * num_labels
    total_recall_list = [0] * num_labels
    total_F1_list = [0] * num_labels

    # iterate over the folds
    for i in range(num_folds):
        test_this_round = featuresets[(i*subset_size):][:subset_size]
        train_this_round = featuresets[:(i*subset_size)] + featuresets[((i+1)*subset_size):]
        # train using train_this_round
        classifier = nltk.NaiveBayesClassifier.train(train_this_round)
        # evaluate against test_this_round to produce the gold and predicted labels
        goldlist = []
        predictedlist = []
        for (features, label) in test_this_round:
            goldlist.append(label)
            predictedlist.append(classifier.classify(features))

        # computes evaluation measures for this fold and
        #   returns list of measures for each label
        print('Fold', i)
        (precision_list, recall_list, F1_list) \
                  = eval_measures(goldlist, predictedlist, labels)
        # take off triple string to print precision, recall and F1 for each fold
        
        print('\tPrecision\tRecall\t\tF1')
        # print measures for each label
        for i, lab in enumerate(labels):
            print(lab, '\t', "{:10.3f}".format(precision_list[i]), \
              "{:10.3f}".format(recall_list[i]), "{:10.3f}".format(F1_list[i]))
        
        # for each label add to the sums in the total lists
        for i in range(num_labels):
            # for each label, add the 3 measures to the 3 lists of totals
            total_precision_list[i] += precision_list[i]
            total_recall_list[i] += recall_list[i]
            total_F1_list[i] += F1_list[i]

    # find precision, recall and F measure averaged over all rounds for all labels
    # compute averages from the totals lists
    precision_list = [tot/num_folds for tot in total_precision_list]
    recall_list = [tot/num_folds for tot in total_recall_list]
    F1_list = [tot/num_folds for tot in total_F1_list]
    # the evaluation measures in a table with one row per label
    print('\nAverage Precision\tRecall\t\tF1 \tPer Label')
    # print measures for each label
    for i, lab in enumerate(labels):
        print(lab, '\t', "{:10.3f}".format(precision_list[i]), \
          "{:10.3f}".format(recall_list[i]), "{:10.3f}".format(F1_list[i]))
    
    # print macro average over all labels - treats each label equally
    print('\nMacro Average Precision\tRecall\t\tF1 \tOver All Labels')
    print('\t', "{:10.3f}".format(sum(precision_list)/num_labels), \
          "{:10.3f}".format(sum(recall_list)/num_labels), \
          "{:10.3f}".format(sum(F1_list)/num_labels))

    # for micro averaging, weight the scores for each label by the number of items
    #    this is better for labels with imbalance
    # first intialize a dictionary for label counts and then count them
    label_counts = {}
    for lab in labels:
      label_counts[lab] = 0 
    # count the labels
    for (doc, lab) in featuresets:
      label_counts[lab] += 1
    # make weights compared to the number of documents in featuresets
    num_docs = len(featuresets)
    label_weights = [(label_counts[lab] / num_docs) for lab in labels]
    print('\nLabel Counts', label_counts)
    #print('Label weights', label_weights)
    # print macro average over all labels
    print('Micro Average Precision\tRecall\t\tF1 \tOver All Labels')
    precision = sum([a * b for a,b in zip(precision_list, label_weights)])
    recall = sum([a * b for a,b in zip(recall_list, label_weights)])
    F1 = sum([a * b for a,b in zip(F1_list, label_weights)])
    print( '\t', "{:10.3f}".format(precision), \
      "{:10.3f}".format(recall), "{:10.3f}".format(F1))
    

# Function to compute precision, recall and F1 for each label
#  and for any number of labels
# Input: list of gold labels, list of predicted labels (in same order)
# Output: returns lists of precision, recall and F1 for each label
#      (for computing averages across folds and labels)
def eval_measures(gold, predicted, labels):
    
    # these lists have values for each label 
    recall_list = []
    precision_list = []
    F1_list = []

    for lab in labels:
        # for each label, compare gold and predicted lists and compute values
        TP = FP = FN = TN = 0
        for i, val in enumerate(gold):
            if val == lab and predicted[i] == lab:  TP += 1
            if val == lab and predicted[i] != lab:  FN += 1
            if val != lab and predicted[i] == lab:  FP += 1
            if val != lab and predicted[i] != lab:  TN += 1
        # use these to compute recall, precision, F1
        # for small numbers, guard against dividing by zero in computing measures
        if (TP == 0) or (FP == 0) or (FN == 0):
          recall_list.append (0)
          precision_list.append (0)
          F1_list.append(0)
        else:
          recall = TP / (TP + FP)
          precision = TP / (TP + FN)
          recall_list.append(recall)
          precision_list.append(precision)
          F1_list.append( 2 * (recall * precision) / (recall + precision))

    # the evaluation measures in a table with one row per label
    return (precision_list, recall_list, F1_list)


## function to read kaggle training file, train and test a classifier 
# changed original function name because _eval helped me keep track of new functions
def processkaggle_eval(dirPath,limitStr):
  # convert the limit argument from a string to an int
  limit = int(limitStr)
  
  os.chdir(dirPath)
  
  f = open('./train.tsv', 'r')
  # loop over lines in the file and use the first limit of them
  phrasedata = []
  for line in f:
    # ignore the first line starting with Phrase and read all lines
    if (not line.startswith('Phrase')):
      # remove final end of line character
      line = line.strip()
      # each line has 4 items separated by tabs
      # ignore the phrase and sentence ids, and keep the phrase and sentiment
      phrasedata.append(line.split('\t')[2:4])
  
  # pick a random sample of length limit because of phrase overlapping sequences
  random.shuffle(phrasedata)
  phraselist = phrasedata[:limit]

  print('Read', len(phrasedata), 'phrases, using', len(phraselist), 'random phrases')
  
  # create list of phrase documents as (list of words, label)
  phrasedocs = []
  # add all the phrases

  # each phrase has a list of tokens and the sentiment label (from 0 to 4)
  ### bin to only 3 categories for better performance
  for phrase in phraselist:
    tokens = nltk.word_tokenize(phrase[0])
    phrasedocs.append((tokens, int(phrase[1])))

  # possibly filter tokens
  # lowercase - each phrase is a pair consisting of a token list and a label
  docs = []
  for phrase in phrasedocs:
    lowerphrase = ([w.lower() for w in phrase[0]], phrase[1])
    docs.append (lowerphrase)
  # print a few
  #for phrase in docs[:10]:
    #print (phrase)

  # continue as usual to get all words and create word features
  all_words_list = [word for (sent,cat) in docs for word in sent]
  all_words = nltk.FreqDist(all_words_list)
  print(len(all_words))



  # get the 1500 most frequently appearing keywords in the corpus
  word_items = all_words.most_common(1500)
  word_features = [word for (word,count) in word_items]

  # feature sets from a feature definition function
  featuresets = [(document_features(d, word_features), c) for (d, c) in docs]


  # train classifier and show performance in cross-validation
  # make a list of labels
  label_list = [c for (d,c) in docs]
  labels = list(set(label_list))    # gets only unique labels
  num_folds = 5
  cross_validation_PRF(num_folds, featuresets, labels)

#################################################################
#                       *STEP 2 FUNCTIONS*                      #
#                         TR_CODE_START                         #
#################################################################

#  ---------------------- Professor Code... Then Additional code ---------------------- #
## function to read kaggle training file, train and test a classifier 
def processkaggle_s2(dirPath,limitStr):
  # convert the limit argument from a string to an int
  limit = int(limitStr)
  
  os.chdir(dirPath)
  
  f = open('./train.tsv', 'r')
  # loop over lines in the file and use the first limit of them
  phrasedata = []
  for line in f:
    # ignore the first line starting with Phrase and read all lines
    if (not line.startswith('Phrase')):
      # remove final end of line character
      line = line.strip()
      # each line has 4 items separated by tabs
      # ignore the phrase and sentence ids, and keep the phrase and sentiment
      phrasedata.append(line.split('\t')[2:4])
  
  # comment out random sample due to BEFORE & AFTER filtering 
  #   displaying different sentences  
  random.shuffle(phrasedata)
  phraselist = phrasedata[:limit]

  print('Read', len(phrasedata), 'phrases, using', len(phraselist), 'random phrases')
  
  # create list of phrase documents as (list of words, label)
  phrasedocs = []
  # add all the phrases

  # each phrase has a list of tokens and the sentiment label (from 0 to 4)
  ### bin to only 3 categories for better performance
  for phrase in phraselist:
    tokens = nltk.word_tokenize(phrase[0])
    phrasedocs.append((tokens, int(phrase[1])))

  # possibly filter tokens
  # lowercase - each phrase is a pair consisting of a token list and a label
  docs = []
  for phrase in phrasedocs:
    lowerphrase = ([w.lower() for w in phrase[0]], phrase[1])
    docs.append (lowerphrase)
  print('\n'+'----- PHRASE SAMPLED -----'+'\n')
  print(docs)
  print('\n'+'----- PHRASE AFTER -----'+'\n')
  return(docs)

# ---------------------------------------------------------------------------------------- #

# Stopwords and Negations
def sw_negation(docs_tokens):
  stopwords = nltk.corpus.stopwords.words('english')
  # Negation
  negationwords = ['ain', 'aren', 'couldn', 'didn', 'doesn', 'hadn',
                      'hasn', 'haven', 'isn', 'ma', 'mightn', 'mustn', 
                      'needn', 'shan', 'shouldn', 'wasn', 'weren', 'won',
                      'wouldn']
  # join both stopwords and negation
  new_sw = stopwords + negationwords
  # implement new stopwords
  docs_sw = []
  for phrase in docs_tokens:
    filter_sw = ([w for w in phrase[0] if not w in new_sw], phrase[1])
    docs_sw.append(filter_sw)
  return(docs_sw)

# ---------------------------------------------------------------------------------------- #

def isPresent_c(word, emotionlist):
  isFound = False
  # loop over all elements of list
  for emotionword in emotionlist:
    # test if a word or a stem
    if not emotionword[-1] == '*':
      # it's a word!
      # when a match is found, can quit the loop with True
      if word == emotionword:
        isFound = True
        break
    else:
      # it's a stem!
      # when a match is found, can quit the loop with True
      if word.startswith(emotionword[0:-1]):
        isFound = True
        break
  # end of loop
  return isFound

# ---------------------------------------------------------------------------------------- #

# Stemmer Lancaster 
def stem_lan(docs_tokens):
  lancaster = nltk.LancasterStemmer()
  docs_lan = []
  for phrase in docs_tokens:
    stemmed = ([lancaster.stem(w) for w in phrase[0]], phrase[1])
    docs_lan.append(stemmed)
  return(docs_lan)

# ---------------------------------------------------------------------------------------- #

# Bigrams Frequency Filter Raw
def freqFilter_Raw(docs_tokens):
  # What tokens occor more that 5x over the first(random) 1500 words
  docs_freq = []
  for phrase in docs_tokens:
    phrase = ([w for w in phrase[0]])
    docs_freq.append(phrase)
  # Merge sentences tokens to understand how often words occor in dataset
  merged = list(itertools.chain.from_iterable(docs_freq))
  # Raw filter freqency of 2 for top/all words in list
  bigram_measures = nltk.collocations.BigramAssocMeasures()
  # created bigram coleection finder
  finder_Filter = BigramCollocationFinder.from_words(merged)
  finder_Filter.apply_freq_filter(2)
  # when Looking for bigrams mesures the list will often be empty
  #   due to the lack of *REPEAT BIGRAMS*.
  #     BEST LIMIT >= 100.
  scored = finder_Filter.score_ngrams(bigram_measures.raw_freq)
  # What tokens occor more that 5x over the first(random) 1500 words
  print("\n"+" --- RAW FREQUENCY TOP 2 PAIRS --- ")
  if len(scored) >=1:
    pprint(scored)
  else:
    print('Increase Review Sentence limits for results')

# ---------------------------------------------------------------------------------------- #


# Bigrams Frequency Filter PMI
def freqFilter_Pmi(docs_tokens):
  # Pointwise Mutual Information filter freqency of 5 for top 50
  # What tokens occor more that 5x over the first(random) 1500 words
  docs_freq = []
  for phrase in docs_tokens:
    phrase = ([w for w in phrase[0]])
    docs_freq.append(phrase)
  # Merge sentences tokens to understand how often words occor in dataset
  merged = list(itertools.chain.from_iterable(docs_freq))
  # Raw filter freqency of 2 for top/all words in list
  bigram_measures = nltk.collocations.BigramAssocMeasures()
  # created bigram coleection finder
  finder_Filter = BigramCollocationFinder.from_words(merged)
  finder_Filter.apply_freq_filter(2)
  # when Looking for bigrams mesures the list will often be empty
  #   due to the lack of *REPEAT BIGRAMS*.
  #     BEST LIMIT >= 100.
  scored = finder_Filter.score_ngrams(bigram_measures.pmi)
  # What tokens occor more that 5x over the first(random) 1500 words
  print("\n"+" --- PMI FREQUENCY TOP 2 PAIRS --- ")
  if len(scored) >=1:
    pprint(scored)
  else:
    print('Increase Review Sentence limits for results')

# ---------------------------------------------------------------------------------------- #

# Freqency Most Common
def MostCommon(docs_tokens):
  # Find the 3 most common words 
  #   b/c limit is unknown from other users by me
  docs_freq = []
  for phrase in docs_tokens:
    phrase = ([w for w in phrase[0]])
    docs_freq.append(phrase)
  # Merge sentences tokens to understand how often words occor in dataset
  merged = list(itertools.chain.from_iterable(docs_freq))
  # Find 3 most common words
  mc = []
  for common in FreqDist(merged).most_common(3):
    mc.append(common)
  print("\n"+" --- 3 MOST COMMON WORDS --- ")
  pprint(mc)



# ---------------------------------------------------------------------------------------- #

# Regular expressions 
def non_alpha_filter(docs_tokens):
  docs_freq = []
  for phrase in docs_tokens:
    phrase = ([w for w in phrase[0]],phrase[1])
    for i in phrase[0]:
      # pattern to match word of non-alphabetical characters
      pattern = re.compile('^[^a-z]+$')
      if pattern.match(i) == True:
        phrase.remove(i)
    docs_freq.append(phrase)
  return(docs_freq)

# ---------------------------------------------------------------------------------------- #

# Sentiment Lexicon
def pos_neg_def(poslist,neglist,docs_tokens):
    docs_freq = []
    for phrase in docs_tokens:
      phrase = ([w for w in phrase[0]])
      docs_freq.append(phrase)
    # Merge sentences tokens to understand how often words occor in dataset
    merged = list(itertools.chain.from_iterable(docs_freq))
    for word in merged:
        print ('     '+word+' ==> Pos->' ,
               isPresent(word, poslist),' ... Neg->',
               isPresent(word, neglist),'\n')

# ---------------------------------------------------------------------------------------- #

# sentiment subjectivity Lexicon filed 
def pos_neutral_neg(positivelist, neutrallist, negativelist,docs_tokens):
    docs_freq = []
    for phrase in docs_tokens:
      phrase = ([w for w in phrase[0]])
      docs_freq.append(phrase)
    # Merge sentences tokens to understand how often words occor in dataset
    merged = list(itertools.chain.from_iterable(docs_freq))
    for word in merged:
        print ('     '+word+' ==> Pos->' , 
              isPresent_c(word, positivelist),' ... Ntrl->',
              isPresent_c(word, neutrallist),' ... Neg->',
              isPresent_c(word, negativelist),'\n')


# Read Subjectivity doc
def subj_def(sentiment_list,docs_tokens):
    docs_freq = []
    for phrase in docs_tokens:
      phrase = ([w for w in phrase[0]])
      docs_freq.append(phrase)
    # Merge sentences tokens to understand how often words occor in dataset
    merged = list(itertools.chain.from_iterable(docs_freq))
    # Loop throuh each index and match according to word
    # for word in merged:
    #   print(word)
    #   for subj_word in sentiment_list:
    #     if subj_word[0] == word:
    #       print ('     '+word+' -----',subj_word[2])
               # isPresent(word, positivelist),
               # '-----', isPresent(word, neutrallist),
               # '-----', isPresent(word, negativelist),'\n')

# ---------------------------------------------------------------------------------------- #

#################################################################
#                       *STEP 2 FUNCTIONS*                      #
#                          TR_CODE_END                          #
#################################################################

#################################################################
#                       *STEP 3 FUNCTIONS*                      #
#                          TR_CODE_START                        #
#################################################################

def processkaggle_s3(dirPath,limitStr,filter_var,bol):# Yes for multi filter
  # convert the limit argument from a string to an int
  limit = int(limitStr)
  
  os.chdir(dirPath)
  
  f = open('./train.tsv', 'r')
  # loop over lines in the file and use the first limit of them
  phrasedata = []
  for line in f:
    # ignore the first line starting with Phrase and read all lines
    if (not line.startswith('Phrase')):
      # remove final end of line character
      line = line.strip()
      # each line has 4 items separated by tabs
      # ignore the phrase and sentence ids, and keep the phrase and sentiment
      phrasedata.append(line.split('\t')[2:4])
  
  # pick a random sample of length limit because of phrase overlapping sequences
  random.shuffle(phrasedata)
  phraselist = phrasedata[:limit]

  print('Read', len(phrasedata), 'phrases, using', len(phraselist), 'random phrases')
  
  # create list of phrase documents as (list of words, label)
  phrasedocs = []
  # add all the phrases

  # each phrase has a list of tokens and the sentiment label (from 0 to 4)
  ### bin to only 3 categories for better performance
  for phrase in phraselist:
    tokens = nltk.word_tokenize(phrase[0])
    phrasedocs.append((tokens, int(phrase[1])))

  # possibly filter tokens
  # lowercase - each phrase is a pair consisting of a token list and a label
  docs = []
  for phrase in phrasedocs:
    lowerphrase = ([w.lower() for w in phrase[0]], phrase[1])
    docs.append (lowerphrase)

#########################################################################
# --------------------------------------------------------------------- #
  # Perform filtering
  # True means Step 4 multi filter, hard coded below.
  if bol == False:
    docs_filter = filter_var(docs)
  else:
     fil_1 = sw_negation(docs)
     fil_2 = non_alpha_filter(fil_1)
     docs_filter = fil_2


# --------------------------------------------------------------------- #
#########################################################################

  # continue as usual to get all words and create word features
  all_words_list = [word for (sent,cat) in docs_filter for word in sent]#--- var from filter Here!!
  all_words = nltk.FreqDist(all_words_list)
  print(len(all_words))



  # get the 1500 most frequently appearing keywords in the corpus
  word_items = all_words.most_common(1500)
  word_features = [word for (word,count) in word_items]

  # feature sets from a feature definition function
  featuresets = [(document_features(d, word_features), c) for (d, c) in docs_filter]#--- var from filter Here!!


  # train classifier and show performance in cross-validation
  # make a list of labels
  label_list = [c for (d,c) in docs_filter]#--- var from filter Here!!
  labels = list(set(label_list))    # gets only unique labels
  num_folds = 5
  cross_validation_PRF(num_folds, featuresets, labels)

# ---------------------------------------------------------------------------------------- #

# non filtered classifer vs. filtered classifier
def filter_vie(dirPath,limitStr,filter_var,bol):

  print('\n'+'-------- Non_Filtered Classifier -------- ')
  processkaggle_eval(dirPath,limitStr)

  print('\n'+'-------- Filtered Classifier -------- ')
  processkaggle_s3(dirPath,limitStr,filter_var,bol)


#################################################################
#                       *STEP 3 FUNCTIONS*                      #
#                          TR_CODE_END                          #
#################################################################

#################################################################
#                       *STEP 4 FUNCTIONS*                      #
#                          TR_CODE_START                        #
#################################################################

# ---------------------------------------------------------------------------------------- #
# SK Learn code

def skLearn(dirPath,split_size):
  import pandas as pd
  import sklearn
  from sklearn.model_selection import train_test_split
  from sklearn.feature_extraction.text import CountVectorizer
  from sklearn.feature_extraction.text import TfidfTransformer
  from sklearn.naive_bayes import MultinomialNB
  from sklearn.pipeline import Pipeline
  from sklearn.metrics import classification_report 

  # -------------------------------------------------------------- #

  os.chdir(dirPath)
  f = open('./train.tsv', 'r')
  data = pd.read_csv('./train.tsv',sep='\t')

  # -------------------------------------------------------------- #

  print(data)
  print(data.iloc[0]['Phrase'],'Sentiment - ',data.iloc[0]['Sentiment'])
  print(data.iloc[1]['Phrase'],'Sentiment - ',data.iloc[1]['Sentiment'])

  print(data.iloc[32]['Phrase'],'Sentiment - ',data.iloc[32]['Sentiment'])
  print('\n')
  print(data.iloc[33]['Phrase'],'Sentiment - ',data.iloc[33]['Sentiment'])

  # -------------------------------------------------------------- #

  X = data['Phrase']
  y = data['Sentiment']

  # -------------------------------------------------------------- #
  phrase_train,phrase_test,sentiment_train,sentiment_test = train_test_split(X,y,test_size=split_size)
  pipeline = Pipeline([('vect',CountVectorizer()),
                      ('tfidf',TfidfTransformer()),
                      ('classifier',MultinomialNB())])
  pipeline.fit(phrase_train,sentiment_train)
  predictions = pipeline.predict(phrase_test)
  print(classification_report(sentiment_test,predictions))

  # -------------------------------------------------------------- #

  print('''
  Cite_Author_Name_      =  https://www.kaggle.com/harshitmakkar

  Cite_Notebook_URL      =  https://www.kaggle.com/harshitmakkar/sentiment-analysis-on-movie-reviews-nlp

  Cite_SK_Learn NB Model =  https://scikit-learn.org/stable/tutorial/text_analytics/working_with_text_data.html
  ''')


# ---------------------------------------------------------------------------------------- #




#################################################################
#                       *STEP 4 FUNCTIONS*                      #
#                          TR_CODE_END                          #
#################################################################

"""
commandline interface takes a directory name with kaggle subdirectory for train.tsv
   and a limit to the number of kaggle phrases to use
It then processes the files and trains a kaggle movie review sentiment classifier.

"""
if __name__ == '__main__':
    if (len(sys.argv) != 3):
        print ('usage: classifyKaggle.py <corpus-dir> <limit>')
        sys.exit(0)
    # get current directory path to add to system argument variable
    #   in order to properly read vaiables path for functions change
    #     in directory code: os.chdir()
    current_Dir = os.getcwd()
    txt = current_Dir + '\\' + sys.argv[1]
    lmt = sys.argv[2]
    print('\n'+'________________________________________________')
    print('\n'+'                ********  STEP 1 START  ********          ')
    # store unfiltered tokenized reviews
    pre = []
    for phrase in processkaggle_s2(txt,lmt):
      pre.append(phrase)
    print('\n'+ 'PHRASES SAMPLED: '+'\n')
    print( pre)

    print('\n'+'                ********  STEP 1 END  ********          ')
    print('\n'+'________________________________________________')
    # Wrote Feature Function interation for document tokens
    print('\n'+'               ********  STEP 2 START ********         ')
    # Feat_Func = [freqFilter_Raw,freqFilter_Pmi,MostCommon,] # [SentLex]
    # # print out filtered items then loop through freqency
    # for Func in Feat_Func:
    #   Func(processkaggle_s2(txt,lmt))
    # # ------------------------------------------------------- #
    # print("\n"+" --- NON ALPHA EXAMPLE AFTER --- ")
    # print(non_alpha_filter(processkaggle_s2(txt,lmt)))
    # print("\n"+" --- STOPWORDS NEGATION EXAMPLE AFTER --- ")
    # print(sw_negation(processkaggle_s2(txt,lmt)))
    # print("\n"+" --- STEMMER LANCASTER EXAMPLE AFTER --- ")
    # print(stem_lan(processkaggle_s2(txt,lmt)))
    # print("\n"+" --- LIWC SENTIMENT POS/NEG --- ")
    # pos_neg_def(poslist,neglist,processkaggle_s2(txt,lmt))
    # print("\n"+" --- SUBJECT CLAUSE SENTIMENT POS/Ntrl/NEG --- ")
    # pos_neutral_neg(positivelist, neutrallist, negativelist,processkaggle_s2(txt,lmt))

    print('\n'+'                ********  STEP 2  ********          ')
    print('\n'+'________________________________________________')
    print('\n'+'                ********  STEP 3 START  ********          ')
    print('\n'+ 'PHRASES SAMPLED: '+'\n')
    print( pre)
    # #list filtered Func
    # # filtered and non-filtered 
    filter_vars = {sw_negation:' ----- !STOPWORDS & NEGATION! ----- ',
                   non_alpha_filter:' ----- !NON-ALPHABET FILTER! ----- ',
                   stem_lan:' ----- !LANCASTER STEMMER! ----- '}

    for Func_filter,Name in filter_vars.items():
      print('\n'+'\n'+Name)
      filter_vie(txt,lmt,Func_filter,False)

    print('\n'+'                ********  STEP 3 END  ********          ')
    print('\n'+'________________________________________________')
    print('\n'+'                ********  STEP 4 START  ********          ')
    # print("\n"+" -------- NLTK Classifier BEFORE -------- "+'\n')
    # processkaggle_eval(txt,lmt)

    print("\n"+" -------- FULLY FILTERED NLTK Classifier -------- "+'\n')

    # # # NLTK classifier with stopwords, negation and non-alpha
    # # altered processkaggle file with a bool that wil process keep 
    # #   the changes from single or multiple filters and 
    # #     var X are the filters but, in this case i hard coded
    # #       the filters in function so its just a fillter
    # x = 'filler'
    # processkaggle_s3(txt,lmt,x,True)

    print("\n"+" -------- SCIKIT LEARN Classifier -------- "+'\n')
    # skLearn(txt,.2)# example format 0.5 for split


    print('\n'+'                ********  STEP 4 END  ********          ')