# hmt-analytics

Tools for analyzing the HMT corpus.


## Datasets

In the `datasets` directory

- `hmt_2cols.tsv`: two-column representation of all texts in HMT as of Feb. 8, 2017

## Manipulating the data

`build.sbt` sets up a utwiddle environment.  Use it interactively from `sbt console`.  Save scripts, and load them from within the sbt console with `:load FILENAME`


## Other scripts

The `scripts` directory contains some initial small scripts for use in house to test and debug HMT data.  They are not heavily bug-proofed, but can be useful on very clean datasets (or by crashing on dirty data :-). The following files are executable ammonite scripts:

- `elemnames.sc`: given a file with a list of well-formed XML fragments, one per line, compiles the set of TEI elements appearing in the list. Cut the text field out of all tabulated files, save it to a file, and run this script to get a list of all TEI elements appearing in the HMT archive.
- `indexNAttrs`: given a file in 2-column format, indexes all occurrences of a given element to the text node where they appear.
