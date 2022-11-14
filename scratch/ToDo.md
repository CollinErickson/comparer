# To do list

## ffexp

-   change_folder_path

    -   change folder_path
    -   check if folder already exists
    -   copy files over
    -   delete old folder

-   check if folder_path already exists when creating ffexp. If yes, maybe don't.

-   If saved object exists in folder, stop. Add arg to overwrite.

-   ffexp run_all for time tests, cleanup

-   recover temp gives error, print which one

-   Delete results of a trial (flip completed, remove from outlist and outdfs)

-   Rename ffexp?

-   Create non-R6 function for ffexp. FFexp, Experiment, CompExp?

-   Run fractional experiments?

-   ffexp plot effects?

-   calculate effects that calculates slopes instead of for each level.

-   list outputs, get some for outcleandf

-   parallel masteroutput to "" by default to make it easier. Usually don't want it anyways

-   superbatch has nsb wrong. Give either n batches or n trials per batch.

-   ffexp: if parallel error, error message say to use varlist?

-   Fix calculate effects 2

-   run_all, not parallel, a lot, don't print, use progress?

## hype

-   add hype to descr, readme, vignette

-   tests

-   Give best inputs, either already run, or optimized

-   Store proportion of time spent with DK vs running experiment

-   Run_all in parallel, does it work?

-   Give DK GP info. Plot? Show which inputs are important.

-   Maximize option?

-   set parallel in init, give in varlist, save_folder

-   qEI is slow, print what it's doing?

-   Avoid outliers. Allow output on log scale.

-   Don't print out all from ffexp, do progress

-   par_unordered/ordered/discrete/int: plot, pairs, ...

-   Make xgboost param tuning example

- par_integer/discrete on log scale

-   Remove unevaluated run. E.g., add EI but want to undo it.

-   Plot run time

- Run EI for time: add based on EI/(exp time to fit)

- when add data, override runtime to be NA

- Add in verbose printouts

- Only update GauPro model, instead of refit, when no change to spec

- EI/qEI work for discretenum/int. Make sure it doesn't pick same point twice.

- Reduce print of hype

- EI isn't actually using unevaluated X?

## MAB: multi-armed bandit

-   run batches, easy access function, df/list input, log scale. Check that all have been run. Binary. Pass through args.

## mbc

- Change print of object

- Fix plot error

- Change plots from base to ggplot2
