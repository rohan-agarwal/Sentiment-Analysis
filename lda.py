import pandas as pd
import nltk as nl
import gensim as gm
import cPickle as pickle


def pull_text_column(df, column_name):
    return [x for x in df[column_name] if type(x) is str]


def tokenize(text):
    tokens = [nl.word_tokenize(x) for x in text]
    tokens = [item for sublist in tokens for item in sublist]
    filt = [w for w in tokens if w not in nl.corpus.stopwords.words(
        'english') and len(w) > 1]
    return filt


def perform_lda(text):
    print "tokenizing"
    words = [a.split() for a in text]
    # only use training data
    # w2 = words[0:int(0.8*len(words))]
    print "building dictionary"
    dictionary = gm.corpora.Dictionary(words)
    print "building corpus"
    corpus = [dictionary.doc2bow(word) for word in words]

    print "performing lda"
    lda = gm.models.ldamodel.LdaModel(corpus=corpus,
                                      id2word=dictionary,
                                      num_topics=10,
                                      update_every=1,
                                      chunksize=100,
                                      passes=5)

    # d2 = gm.corpora.Dictionary(words)
    # corpus2 = [d2.doc2bow(word) for word in words]

    size = len(corpus)
    df = [0] * size
    print "building df"
    for i in range(size):
        df[i] = lda[corpus[i]]

    df = [[x[1] for x in a] for a in df]
    df = pd.DataFrame(df)
    df.to_csv('lda22.csv')

    return lda

print "reading csv"
sa = pd.read_csv('SA_dataset.tsv', sep="\t", header=None)
print "formatting"
desc = pull_text_column(sa, 1)
lda = perform_lda(desc)

pickle.dump(lda, open("lda.p", "wb"))
