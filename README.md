Full Workflow: Text Processing, Sentiment Analysis, Zero-Shot Classification & Summarization

This repository contains a complete tutorial demonstrating how to analyze CMS public comments using Python, open-source NLP tools, and local LLM models.
The workflow replicates and extends common text-analysis tasks typically performed in R (tidytext), but implemented in Python for flexibility and modern LLM integration
What’s Included

The tutorial walks through 12 major steps, including:

Loading and inspecting the dataset

Cleaning and preparing text data

Tokenization and sentiment lexicon loading

Lexicon-based sentiment scoring

Histogram visualization (ggplot2-style)

Extracting top positive/negative comments

Grouping and summarizing sentiment

Local LLM stance classification (zero-shot)

Examples of extreme sentiment comments

Zero-shot classification demo

Summarizing long comments (~50 words)

Comparing two open-source LLMs for sentiment

 Models Used

facebook/bart-large-mnli — zero-shot classification

microsoft/deberta-v3-base — alternative zero-shot classifier

facebook/bart-large-cnn — summarization model

All models are open-source and require no API key.

 Dependencies

Main Python libraries:

pandas

numpy

nltk

seaborn / matplotlib

transformers

tqdm

Purpose

This tutorial demonstrates how modern open-source NLP and LLM tools can be used to quickly classify, analyze, and summarize public policy comments — enabling scalable text analysis without manual coding or proprietary APIs.# Tutorial-Methods
