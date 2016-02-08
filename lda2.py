import pandas as pd
import nltk as nl
import gensim as gm
import cPickle as pickle


def pull_text_column(df, column_name):
    return [x for x in df[column_name] if type(x) is str]

lda = pickle.load(open("lda.p", "rb"))

sa = pd.read_csv('C:\Users\Rohan\Documents\GitHub\Sentiment-Analysis\SA_dataset.tsv', sep="\t", header=None)
desc = pull_text_column(sa, 1)
words = [a.split() for a in desc]
dictionary = gm.corpora.Dictionary(words)
corpus = [dictionary.doc2bow(word) for word in words]

size = len(corpus)
df = [0] * size
print "building df"
for i in range(size):
    df[i] = lda[corpus[i]]

df = [[x[1] for x in a] for a in df]
df = pd.DataFrame(df)
df.to_csv('lda.csv')
