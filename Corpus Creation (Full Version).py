#a program to convert the separate KAIST txt files into one searchable corpus

#while this is convenient to have a single file with all the corpus information, if you prefer to keep the txt files separate, 
#the corpus search scripts should be easy to adapt using: for filename in listdir('C:/wherever/the_files/are_located'). This is what I opt to do for the 100man corpus search.

import csv
import glob
import pandas


read_files = glob.glob("D:/Corpora/KAIST POS Tagged/**/*.txt", recursive=True) #read all of our txt files for the hand-tagged corpus, this is the 1million word corpus

with open("KAIST POS.txt", "wb") as outfile: #open our KAIST corpus file
    for f in read_files:
        with open(f, "rb") as infile:
            outfile.write(infile.read())

read_files2 = glob.glob("D:/Corpora/KAIST Automatically Analyzed Corpus/**/*.txt", recursive=True) #read all of our txt files for the automatically analyzed corpus, this is the 40million word corpus
with open("KAIST Automatically Analyzed Corpus.txt", "wb") as outfile:
    for f in read_files2:
        with open(f, "rb") as infile:
            outfile.write(infile.read())


# -*- coding: 949 -*-

import csv
import pandas as pd
import codecs
from encodings import euc_kr #need this for reading Korean encoding
from encodings import cp949


content = []
with open('D:/Corpora/Corpora Code/KAIST Automatically Analyzed Corpus.txt', 'r', encoding = 'euc_kr',errors='ignore') as file_in:
    content = file_in.readlines() #open our newly formed corpus

new_content = []
for line in content:
    clean_line = line.replace('\00', '') #get rid of all the null values. I'm not a huge fan of this solve, but losing a few data points isn't a huge problem given the size of this corpus.
    new_content.append(clean_line)

with open('D:/Corpora/Corpora Code/outfile.csv', 'w', encoding = 'euc_kr', errors='ignore') as file_out:
    file_out.writelines(new_content) #write our cleaned up corpus file

        
# -*- coding: 949 -*-

import csv
import re
import sys
import sys
import csv
maxInt = sys.maxsize #we had to use this to overcome the Overflow error

while True:
    # decrease the maxInt value by factor 10 
    # as long as the OverflowError occurs.

    try:
        csv.field_size_limit(maxInt)
        break
    except OverflowError:
        maxInt = int(maxInt/10)
with open('D:/Corpora/Corpora Code/outfile.csv', 'r', encoding = 'euc_kr', errors='ignore') as file_in: #open our input file created in the last section of code
    with open('D:/Corpora/Corpora Code/outfile_revised.csv', 'w', encoding = 'euc_kr', errors='ignore') as file_out: #create a revised output file
        corpus_writer = csv.writer(file_out, delimiter=',', lineterminator='\n') #write corpus
        content=csv.reader(file_in, delimiter = '\t') #read in our input file
        corpus_writer.writerow(["Full word", "First Morpheme", "First Morpheme Tag", "Second Morpheme", "Second Morpheme Tag", "Third Morpheme", "Third Morpheme Tag", "Fourth Morpheme", "Fourth Morpheme Tag", "Fifth Morpheme", "Fifth Morpheme Tag", "Sixth Morpheme", "Sixth Morpheme Tag"])
        for line in content:
            if len(line) == 2:
                out = [line[0]] + re.split('/|\\+', line[1]) #line1 has varying lengths split by slashes or + signs, this overcomes it
                corpus_writer.writerow(out) #write corpus
            else:
                corpus_writer.writerow(line)
#### This section creates some files that others may not need, including a corpus of only morphemes, and a corpus of only "exact matches." It was convenient to separate these out of the main corpus for me, but if you're worried about memory you can omit this part depending on your goal.
##################################################
##################################################
with open("D:/Corpora/Corpora Code/outfile_revised.csv", 'r', encoding = 'euc_kr', errors='ignore') as file: #open file created last section
    with open("D:/Corpora/Corpora Code/Morphemes Only.csv", 'w', encoding = 'euc_kr', errors='ignore') as output: #open file created earlier
        csv_reader = csv.reader(file, delimiter=',') #read csv
        mycsvwriter = csv.writer(output, delimiter=',', lineterminator='\n') #csv writer
        next(csv_reader) #skip first line
        for line in csv_reader:
            morphemes = line[1:] #first column is whole word, second (and third, fourth, etc) are morphemes within the whole word (e.g., 걸려요 - 걸리 - 어요)
            mycsvwriter.writerow(morphemes)

with open("D:/Corpora/Corpora Code/outfile_revised.csv", 'r', encoding = 'euc_kr', errors='ignore') as file: #open file
    with open("D:/Corpora/Corpora Code/Exact Match Only.csv", 'w', encoding = 'euc_kr', errors='ignore') as output: #create output file
        csv_reader = csv.reader(file, delimiter=',')
        mycsvwriter = csv.writer(output, delimiter=',', lineterminator='\n')
        next(csv_reader)
        for line in csv_reader:
            try:
                morphemes = line[0] #first column in morphemes only is the root word (e.g., 걸리 in 걸려요).
                mycsvwriter.writerow([morphemes])
            except IndexError:
                mycsvwriter.writerow('NULL') #take care of indexerror

with open("D:/Corpora/Corpora Code/outfile_revised.csv", 'r', encoding = 'euc_kr', errors='ignore') as file:
    with open("D:/Corpora/Corpora Code/Root Morpheme Only.csv", 'w', encoding = 'euc_kr', errors='ignore') as output: #write our final section of code
        csv_reader = csv.reader(file, delimiter=',')
        mycsvwriter = csv.writer(output, delimiter=',', lineterminator='\n')
        next(csv_reader)
        for line in csv_reader:
            try:
                morphemes = line[1]
                mycsvwriter.writerow([morphemes])
            except IndexError:
                mycsvwriter.writerow(['NULL'])
