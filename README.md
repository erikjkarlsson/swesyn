# How do Download Dataset

 1. Download swedish synonym set `swesaurus.xml` [^1] from: [https://svn.spraakbanken.gu.se/sb-arkiv/pub/lmf/swesaurus/swesaurus.xml]

 2. Convert lexicon to sqlite database: `./convert.py --sqlite swesaurus.db`

# How to Use

 1. Load `swesyn.el`
 
 2. Enable `swesyn-mode`

 3. Type `C-c C-s` to interactively query synonym


# Dependencies 

 - Emacs compiled with `sqlite` support. 
 - `ivy` 

# Notes

All rights to the synonym set `swesaurus.xml` goes to Språkbanken Text (see [^1])
# Footnotes
[^1] Språkbanken Text. (2017-09-19). Swesaurus [Data set]. Språkbanken Text. [https://doi.org/10.23695/w5ww-x964]
