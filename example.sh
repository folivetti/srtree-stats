#!/bin/bash
stack run -- -f hl -i test/feynman-III-17-37_expressions_heuristiclab.txt -d test/feynman_III_17_37_train.csv --test test/feynman_III_17_37.tsv -c 0,1,2 --niter 10 --hasheader --simplify
