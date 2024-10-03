# How do download lexicon

 1. Download swedish synonym lexicon `swesaurus.xml` [^1] from: (https://svn.spraakbanken.gu.se/sb-arkiv/pub/lmf/swesaurus/swesaurus.xml)

 2. Convert lexicon to sqlite database: `./convert.py --sqlite swesaurus.db`


# How to use

 1. Load `swesyn.el`
 
 2. Enable `swesyn-mode`

 3. Type `C-c C-s` to interactively query synonym


[^1]
Swesaurus Dataset
Språkbanken Text,
Lexicon published 2024 via Språkbanken Text
Ett svenskt ordnät
https://doi.org/10.23695/w5ww-x964
