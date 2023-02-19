# srtree-stats

Calculates general statistics of SR trees for model selection.

## Instalation

```bash
stack install
```

## Command line help:

```bash
srtree-stats -h
```

## Example usage:

Parse the Heuristic Lab expressions in `test/feynman-III-17-37_expressions_heuristiclab.txt` file using the training data in `test/feynman_III_17_37_train.csv` (with variable names in the header) taking the first $50$ rows as the training set and the next rows as validation, select the columns $0,1,2$ as the input variables and runs a maximum of $10$ iterations. Calculates the statistics using the training, validation and test set (` test/feynman_III_17_37.tsv`). It also simplifies the expressions using equality saturation prior to optimizing.

```bash
srtree-opt -f hl -i test/feynman-III-17-37_expressions_heuristiclab.txt -d test/feynman_III_17_37_train.csv -r 50 --test test/feynman_III_17_37.tsv -c 0,1,2 --niter 10 --hasheader --simplify
```

By default the fitted expressions will be printed in stdout but you can specify the output files with the `-o` flag.
