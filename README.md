This is not an officially supported Google product.

This project is a fork of the 'synergyfinder' project, available in github at
https://github.com/hly89/synergyfinder, and also available in BioConductor.  The
code here was originally offered as a patch to the main project, with some code
clean-up, bug fixes, and unit tests.  The patch proved difficult to complete
(in fact a fork was specifically suggested by the institution that maintains the
original project), so the code is offered here instead.

SUMMARY

The biggest part of this change is 'refactoring' of PlotSynergy according to
(my humble interpretation of) software engineering best practices. The overall
logic of the original code is maintained, but is now expressed as a set of
related functions with the following properties:

* The new functions are each small, with relatively few local variables.
* The new functions are documented with comments.
* The new functions are tested with unit tests.
* The new tests provide ways to see specific expected inputs and outputs at a
  code level (rather than just an output level), and to ensure that functions
  fulfill their interface contracts even if reimplemented. For example, a new
  function that rounds a value up to the next multiple of 10 has a test with a
  few input values. If some other method of rounding is chosen in the future, it
  may be deemed correct if it passes the same tests that the current version
  passes. If it does not pass, it may still be correct in some sense; but the
  tests will (by breaking) explicitly highlight the behavior change so that
  callers of the function may be adjusted as needed.

Much logic that was replicated between '2D' and 'all', or between '3D' and
'all', is now consolidated and shared. For example, the only major difference in
2D plots between the '2D' and 'all' cases was a single layout directive issued
before any part of the plot was drawn; yet all of the plotting logic was
duplicated. Now, small functions retain the logic that was held in common, and
the layout directive is issued by the caller before the functions are invoked.

There are some changes to the vignettes. They are not all technical.  I have
gone so far as to suggest some modifications to English content. It should be
reviewed very carefully to ensure continued scientific correctness. The
technical changes permit the following:

* All of the vignettes now execute end-to-end and uninterrupted when RStudio's
  'run all' button is clicked.
* The vignettes fully execute and generate their PDF output when RStudio's
  'Compile PDF' button is clicked.
* RStudio's 'Check' feature inspects the entire package and completes
  successfully.

CHANGE DETAILS

1. Updates to LICENSE.  I fixed an apparent copy/paste error from the R
   DESCRIPTION template, and added my employer to the list of copyright holders.  
2. Various updates to DESCRIPTION.  
  2.1. Update 'imports' to include 'SpatialExtremes' instead of 'kriging'.  
  2.2. My name is added to the 'Author' list in this file.  
  2.3. Long lines are wrapped, for easier readability.  
  2.4. Reduce the required version of ggplot2.  
3. Eliminate all mention of 'kriging' from NAMESPACE.  
4. Make moderate adjustments to Plot_dose_response.R.  
  4.1. Adjust semantics for displaying a plot or saving it to a file.  
  ..4.1.1. Eliminate post-loop plot 'replay'.  
  ..4.1.2. Use dev.new() only when not saving to a file.  
  ..4.1.3. Use dev.off() only when saving to a file.  
  4.2. Use explicit package namespace prefixes for calls to functions in
  'ggplot2' and 'grid'.  
  4.3. Remove some commented-out code, clean up some whitespace, reflow some
  comments.  
5. Perform a near-rewrite refactoring of Plot_synergy.R.  
  5.1. Adjust semantics of displaying/saving plots, similarly to
     Plot_dose_response.R. Furthermore, update output filenames to include
     'type' ('2D', '3D', or 'all').  
  5.2. Use numerous tightly-scoped, commented functions for individual pieces
     of functionality.  
  ..5.2.1. On the simple side, this includes some one-line specialty
      array-slicing implementations, and a function to take an argument's
      absolute value and round it to the next highest multiple of 10.  
  ..5.2.2. On the more complicated side, this includes use of helper functions
      for 2D and 3D plots. The upshot is that the plotting functionality is now
      shared with the 'plot all' feature, instead of replicated for it. This
      eliminates significant code duplication.  
  5.3. Add comprehensive unit tests (in test_Plot_synergy.R) for Plot_synergy.R
     helper functions that determine the data to be included in plots. The plot
     generation functions themselves are not unit-tested, but retain very little
     non-trivial logic.  
  5.4. Multiple adjustments to whitespace and comment flow. API documentation
     has been added for PlotSynergy parameters that were previously
     undocumented.  
  5.5. Explicit 'for' loops have been eliminated in favor of 'lapply'.  
  5.6. PlotSynergy is now defined to return NULL, but with a side effect of
     either saving a file or displaying a plot.  
6. Several updates to the vignettes in synergyfinder.Rnw:  
  6.1. A few minor English tweaks either adjust grammar for clarity/rigor or
     satisfy TeX's 'badness' warnings. There are various whitespace changes as
     well.  
  6.2. Uses of 'h!' that TeX warned it was rewriting as 'ht!' are now explicitly
    'ht!'.  
  6.3. File names of dose.reponse PDF inclusions now actually match (including
     with '.pdf' suffix) the files that are meant to be included. Quotes are
     removed because they were being parsed as part of the file names.  
  6.4. Examples for PlotSynergy() now explicitly execute for every combination
    of 'type' and 'save.file'.  
  6.5. The filenames of PDF output to be included in the vignette document are
     adjusted to include 'type' (which, as noted above, is a newly-added
     constituent of the filename in this change).  
