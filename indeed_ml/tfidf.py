import argparse
import math
from nltk.corpus import stopwords
import numpy as np
from nltk.stem.wordnet import WordNetLemmatizer
import nltk
import sys
import os
import string

def parse_args():
    '''
    Parses the arguments.
    '''
    parser = argparse.ArgumentParser(description="TFIDF vectorizer.")

    parser.add_argument('--train_file', nargs='?', default='indeed_ml_dataset/train.tsv',
                        help='')

    parser.add_argument('--test_file', nargs='?', default='indeed_ml_dataset/test.tsv',
                        help='')

    parser.add_argument('--threshold', type = int, nargs='?', default=500,
                        help='')

    return parser.parse_args()

args = parse_args()

tokenize = lambda doc: doc.lower().split(" ")

lmtzr = WordNetLemmatizer()

def jaccard_similarity(query, document):
    intersection = set(query).intersection(set(document))
    union = set(query).union(set(document))
    return len(intersection)/len(union)

def term_frequency(term, tokenized_document):
    return tokenized_document.count(term)

def sublinear_term_frequency(term, tokenized_document):
    count = tokenized_document.count(term)
    if count == 0:
        return 0
    return 1 + math.log(count)

def augmented_term_frequency(term, tokenized_document):
    max_count = max([term_frequency(t, tokenized_document) for t in tokenized_document])
    return (0.5 + ((0.5 * term_frequency(term, tokenized_document))/max_count))

def inverse_document_frequencies(tokenized_documents):
    idf_values = {}
    all_tokens_set = set([item for sublist in tokenized_documents for item in sublist])
    count = 1
    for tkn in all_tokens_set:
        if count % 100 == 0:
            print ('(IDF)token number ' + str(count) + '/' + str(len(all_tokens_set)))
        count += 1
        contains_token = map(lambda doc: tkn in doc, tokenized_documents)
        idf_values[tkn] = 1 + math.log(len(tokenized_documents)/(sum(contains_token)))
    return idf_values

def tfidf(documents):
    print ('Tokenizing')
    tokenized_documents = [tokenize(d) for d in documents]
    print ('IDF')
    idf = inverse_document_frequencies(tokenized_documents)
    tfidf_documents = []
    count = 1
    for document in tokenized_documents:
        if count % 50 == 0:
            print ('(TFIDF) document number ' + str(count) + '/' + str(len(tokenized_documents)))
        count += 1
        doc_tfidf = []
        for term in idf.keys():
            tf = sublinear_term_frequency(term, document)
            doc_tfidf.append(tf * idf[term])
        tfidf_documents.append(doc_tfidf)
    return tfidf_documents

# heuristic code
def strip_non_ascii(string):
    ''' Returns the string without non ASCII characters'''
    stripped = (c for c in string if 0 < ord(c) < 127)
    return ''.join(stripped)
# 
def remove_useless_words (text):
    res = text.replace ('EOE','')
    res = res.lower()
    res = res.translate(None, string.punctuation)
    res = strip_non_ascii (res)
    res = [lmtzr.lemmatize (word) for word in nltk.word_tokenize (res) if word not in stopwords.words('english')]
    res = ' '.join (res)
    return res

tag_list = ['part-time-job', 'full-time-job', 'hourly-wage', 'salary', 'associate-needed',  'bs-degree-needed', 'ms-or-phd-needed', 'licence-needed', \
'1-year-experience-needed', '2-4-years-experience-needed', '5-plus-years-experience-needed', 'supervising-job']

all_documents = []
all_tags = []

print ('Load train file')
with open (args.train_file, 'r') as f:
    count = 0
    for line in f:
        if count == 0:
            count += 1
            continue
        if count % 500 == 0:
            print ('Load line ' + str (count) + ' of train file')
        count += 1
        raw_tags, content = line.split ('\t')
        tags = raw_tags.split()
        found_tags = [False] * len (tag_list)
        for tag in tag_list:
            if tag in tags:
                found_tags [tag_list.index(tag)] = True
        all_tags.append(found_tags)
        content = remove_useless_words(content)
        all_documents.append(content.strip())

print ('Load test file')
with open (args.test_file, 'r') as f:
    count = 0
    for line in f:
        if count == 0:
            count += 1
            continue
        if count % 500 == 0:
            print ('Load line ' + str (count) + ' of test file')
        count += 1
        content = line.strip()
        content = remove_useless_words(content)
        all_documents.append(content)

tfidfs = tfidf(all_documents)

# with open ("tfidfs.txt",'w') as f:
#     for tf_idf in tfidfs:
#         content = map (str, tf_idf)
#         content = ' '.join(content)
#         f.write (content + '\n')

with open ('tags.txt','w') as f:
    f.write (' '.join(tag_list) + '\n')
    for tag in all_tags:
        content = map (str, tag)
        content = ' '.join (content)
        f.write (content + '\n')

a = np.array (tfidfs)
np.savetxt (fname = "tfidf_500.txt", X =a[:, (a != 0).sum(axis=0) >= args.threshold])

#process R File
os.system ("Rscript job_tags.R")

os.system ("python to_submission.py")