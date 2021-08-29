from os import listdir
import csv
import re
import sys
import time
t1 = time.time() #getting in the habit of keeping track of execution time

def morpheme_corpus_search(morpheme):
    count = 0
    corpus_count = 0
    with open('D:/Corpora/Corpora Code/100man Base and Surface Freqs.csv', 'w', encoding = 'euc_kr', errors='ignore') as output:
        for filename in listdir('D:/Corpora/Corpora Code/100ManEojeol_pos-tagged'): #this will open every txt file in the directory
            with open('D:/Corpora/Corpora Code/100ManEojeol_pos-tagged/' + filename, 'r', encoding = 'euc_kr', errors='ignore', newline='') as currentFile:
                corpus = csv.reader(currentFile, delimiter = '\t', skipinitialspace=True, quotechar="\x07") #had to choose a strange quotechar so that python would ignore single and double quotes and stop escaping them                 
                for line in corpus:
                    try: 
                        if line[0].startswith != '*' and line[0].startswith != "<":
                            corpus_count += 1
                        if line[1] == morpheme:
                            count += 1                        
                    except IndexError:
                        continue
        return(count, corpus_count)
               
#morpheme_corpus_search('걸린다')

def specific_corpus_search(specificword): #first column of each row represents the complete, undivided word, whereas the second column represents
    count2 = 0
    corpus_count = 0
    for filename in listdir('D:/Corpora/Corpora Code/100ManEojeol_pos-tagged'): #this will open every txt file in the directory
        with open('D:/Corpora/Corpora Code/100ManEojeol_pos-tagged/' + filename, 'r', encoding = 'euc_kr', errors='ignore', newline='') as currentFile:
            corpus = csv.reader(currentFile, delimiter = '\t', skipinitialspace=True, quotechar="\x07") #had to choose a strange quotechar so that python would ignore single and double quotes and stop escaping them                 
            for line in corpus:
                try: 
                    corpus_count += 1
                    if line[0] == specificword:
                        count2 += 1
                except IndexError:
                    continue
    return(count2)

#specific_corpus_search('걸린다')
testcount = 0


with open('D:/Corpora/Corpora Code/List of Passives.csv', 'r', encoding = 'euc_kr', errors='ignore') as passive_list:
    with open('D:/Corpora/Corpora Code/100man Base and Surface Freqs.csv', 'w', encoding = 'euc_kr', errors='ignore') as output:
        mycsvreader = csv.reader(passive_list, delimiter = '\t')
        mycsvwriter = csv.writer(output, delimiter=',', lineterminator='\n')
        mycsvwriter.writerow(["Number", "Subnumber", "Active Morpheme", "Passive Morpheme", "Specific Word", "Base Frequency", "Surface Frequency of the Passive Morpheme", "Surface Frequency of the Exact Word", "Corpus Count"])
        next(mycsvreader)
        for line in mycsvreader:
            number = line[0]
            subnumber = line[1]
            active_morpheme = line[2]
            passive_morpheme = line[3]
            specific_word = line[4]
            passive_morpheme_count, corpus_count = morpheme_corpus_search(passive_morpheme)
            active_morpheme_count = morpheme_corpus_search(active_morpheme)[0]
            specific_word_count = specific_corpus_search(specific_word)             
            base_frequency = int(active_morpheme_count) + int(passive_morpheme_count)
            mycsvwriter.writerow([number, subnumber, active_morpheme, passive_morpheme, specific_word, base_frequency, passive_morpheme_count, specific_word_count, corpus_count])

t2 = time.time()
print(t1-t2) #this gives time it took to run