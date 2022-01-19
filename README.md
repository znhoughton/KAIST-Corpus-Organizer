# KAIST-Corpus-Organizer

Python scripts intended to be used alongside the KAIST Automatically Analyzed Large Scale Corpus, available for free at: http://semanticweb.kaist.ac.kr/home/index.php/KAIST_Corpus

The power analysis script is a power analysis using the BRMS R package in order to see how many participants it would take to run a study to see if surface or base frequencies are a better predictor of Reaction Times in a lexical decision task. 

The "Corpus Creation" script combines and organizes all the corpus files, and the "Corpus Search" scripts, as you may have guessed, are intended to search the corpus. The specific code is for finding the surface and base frequencies of passives, but it should be fairly easy to adapt it for other searches.

I haven't tested the scripts on the smaller corpus either, but it should also be fairly easy to adapt it for the smaller KAIST corpus as well (which was hand-annotated, so may be more accurate).
