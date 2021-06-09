# To do list

## ffexp

* change_folder_path
  - change folder_path
  - check if folder already exists
  - copy files over
  - delete old folder

* check if folder_path already exists when creating ffexp.
If yes, maybe don't.

* If saved object exists in folder, stop. Add arg to overwrite.

* ffexp run_all for time tests, cleanup

* recover temp gives error, print which one

* Delete results of a trial (flip completed, remove from outlist and outdfs)

* Rename ffexp? 

* Create non-R6 function for ffexp. FFexp, Experiment, CompExp?

* Run fractional experiments?

* ffexp plot effects?

* calculate effects that calculates slopes instead of for each level.

* list outputs, get some for outcleandf

* superbatches: If run_all with 100, give option to run in groups of 10.
  would allow for progress print out and saving object

* parallel masteroutput to "" by default to make it easier. Usually don't want it anyways

* superbatch has nsb wrong. Give either n batches or n trials per batch.

* ffexp: if parallel error, error message say to use varlist?

* Fix calculate effects 2

* run_all, not parallel, a lot, don't print, use progress?

## hype

* move to new hype package?

* add hype to descr, readme, vignette

* tests

* Give best inputs, either already run, or optimized

* Store proportion of time spent with DK vs running experiment

* Run_all in parallel, does it work?

* Factor levels. Start with binary?

* Give DK GP info. Plot? Show which inputs are important.

* Save mod as active so it's only fit in one spot.

* Maximize option?

* set parallel in init, give in varlist, save_folder

* qEI is slow, print what it's doing?

* Avoid outliers. Allow output on log scale.

* Don't print out all from ffexp, do progress

* par_discrete: fix add LHS, EI search, plot, ...

## MAB: multi-armed bandit

* run batches, easy access function, df/list input, log scale.
   Check that all have been run. Binary. Pass through args.
