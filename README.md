# hmt-analytics

Tools for analyzing the HMT corpus.  Contents:


## Datasets

In the `datasets` directory, the following files have data from the current archival version of the HMT covering 17 books of the Venetus A mansuscript:

- `scholia-2cols.txt`: A single file with all scholia, in tab-delimited two-column format (urn, text node).


## Scripts

The `scripts` directory contains some initial small scripts for use in house.  They are not heavily bug-proofed, but can be useful on very clean datasets. The following files are executable ammonite scripts:

- `elemnames.sc`: given a file with a list of well-formed XML fragments, one per line, compiles the set of TEI elements appearing in the list. Cut the text field out of all tabulated files, save it to a file, and run this script to get a list of all TEI elements appearing in the HMT archive.
- `indexNAttrs`: given a file in 2-column format, indexes all occurrences of a given element to the text node where they appear.
