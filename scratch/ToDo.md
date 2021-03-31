# To do list

* change_folder_path
  - change folder_path
  - check if folder already exists
  - copy files over
  - delete old folder

* check if folder_path already exists when creating ffexp.
If yes, maybe don't.

* add hype to descr, readme, vignette

* tests

* log distribution params

* If saved object exists in folder, stop. Add arg to overwrite.

* output dat csv doesn't exist

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

* Multi-armed bandit. run batches, easy access function, df/list input, log scale.
   Check that all have been run. Binary.

* parallel masteroutput to "" by default to make it easier. Usually don't want it anyways

* superbatch has nsb wrong. Give either n batches or n trials per batch.

* hype: if no params given in, give message telling what to do
