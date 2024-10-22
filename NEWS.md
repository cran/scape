# scape 2.3.5 (2024-10-22)

* Maintenance release.




# scape 2.3.3 (2020-11-23)

* Maintenance release.




# scape 2.3-1 (2017-06-20)

* Added function importADCAM() to import results from ADCAM-type models.

* Added example object x.saithe.

* Hardwired stringsAsFactors=FALSE in importCol() and removed '...' argument.

* User can pass a data frame as the main argument to functions, instead of a
  scape object.




# scape 2.2-0 (2014-03-11)

* Package loads quietly, thanks to explicit namespace handling.

* Added functions importMCMC() and importProj() to import MCMC results from
  Coleraine. These functions were a part of the "scapeMCMC" package, which has
  now been renamed to "plotMCMC" and no longer contains Coleraine-specific
  functions.

* Removed argument 'info' from importCol().

* Improved plotCA() and plotCL() handling of NA data values.




# scape 2.1-0 (2011-04-28)

* Added argument 'swap' to plotCA(), plotCL(), and plotN().

* Added argument 'strip' to plotCA(), plotCL(), plotIndex(), plotLA(), plotN(),
  and plotSel().

* Added type "l"[ast year] to plotN().

* Improved type "r"[ecruitment] plotN(), so age at recruitment is no longer
  hardwired as 1.




# scape 2.0-1 (2010-09-13)

* Improved package help page.




# scape 2.0-0 (2010-09-12)

* Added suite of functions to diagnose sigmas and sample sizes: estN(),
  estN.int(), estSigmaI(), estSigmaR(), getN(), getSigmaI(), getSigmaR(), and
  iterate().

* Fixed missing error bars in plotIndex, by reestablishing Hmisc dependency.




# scape 1.1-5 (2010-09-03)

* Improved handling of character/factor/integer in plotting functions.

* Improved importCol() so that Year column is integer, not double.




# scape 1.1-4 (2010-09-02)

* Minor improvements in help pages.




# scape 1.1-2 (2010-09-01)

* Added argument '...' to importCol(), e.g. to pass stringsAsFactors=FALSE.

* Example objects x.cod, x.ling, x.oreo, and x.sbw have character instead of
  factor columns.




# scape 1.1-0 (2010-08-31)

* Changed default value of 'what' in plotIndex() to "s" (survey biomass index).

* Added argument 'col.strip' to plotCA(), plotCL(), plotIndex(), plotLA(),
  plotN(), and plotSel(), with gray as default.

* Reduced clutter during startup, by importing Hmisc and suggesting gdata.

* Added three vignettes: dsc-vignette.pdf, gallery.pdf, and mymodel.pdf.




# scape 1.0-8 (2007-09-29)

* Added argument 'col.points' to plotN().




# scape 1.0-7 (2005-10-24)

* Fixed plotIndex() so it returns trellis object or data frame, as documented.




# scape 1.0-6 (2005-10-19)

* Improved help pages.




# scape 1.0-5 (2005-10-18)

* Added help page for scape-package.

* Renamed importRes() to importCol().

* Renamed plot*() arguments: 'log.transform'->'log', 'base.log'->'base', and
  'plot.it'->'plot'.

* Improved plotIndex() and plotLA() so default y-axis limits extend 4% beyond
  data extremes, except lower limit is zero when values are not log-transformed.




# scape 1.0-4 (2005-09-02)

* Fixed documentation for plotN() argument 'axes'.




# scape 1.0-3 (2005-09-01)

* Fixed importRes() example.




# scape 1.0-2 (2005-08-28)

* Fixed discrepancy between default argument values in functions and help pages.




# scape 1.0-1 (2005-08-26)

* Removed redundant argument 'fitted.also' from plotIndex(). User can pass
  lty.lines=0 to suppress fitted lines.

* Renamed argument 'fitted.also' to 'fit' in plotCA() and plotCL().




# scape 1.0-0 (2005-08-25)

* Standardized and vectorized graphical arguments further.

* Submitted to CRAN.




# scape 0.2-2 (2005-08-24)

* Added help pages for all functions.

* Improved importRes() so both survey and commercial selectivity are imported.

* Renamed Survey and Gear columns to Series, in scape elements CAc, CAs, CLc,
  CLs, and Sel.

* Changed plotCA() and plotCL() so they assume what="s" was intended if what="c"
  failed.

* Added argument 'together' to plotSel() to plot gears in one panel.

* Standardized graphical arguments further between plot functions.




# scape 0.2-1 (2005-08-17)

* Added argument 'cex.legend' to plotB() to set legend text size.

* Added argument 'ratio.bars' to plotN() to set bar width in barplots.

* Improved tutorial by adding DSC proceedings paper.




# scape 0.2-0 (2005-08-10)

* Added function plotB() to plot biomass trajectories, landings, and stock
  recruitment.

* Changed plotCA(), plotCL(), and plotIndex() so first series is plotted by
  default.

* Added argument 'together' to plotLA() to plot both sexes in one panel.

* Changed importRes() so imported Coleraine models have attributes call=[call]
  and scape.version=[version]. The info attribute is now empty quotes.

* Changed importRes() to CPUE data are imported without a Gear column. Series
  name can contain the gear name if the user wishes.

* Renamed scape element B column Years to Year.

* Changed graphical arguments in all plot functions, to simplify and
  standardize.

* Updated x.ling and ling.res to a newer version of Coleraine.




# scape 0.1-7 (2004-11-09)

* Added function plotIndex(), making plotCPUE() and plotSurvey() obsolete.

* Improved importRes() to support gear-specific vulnerable biomass, and label
  annual Dev elements with years instead of cohorts.

* Improved x.ling so it does not contain CLs element, since survey
  catch-at-length data were not used in that assessment.




# scape 0.1-6 (2004-07-26)

* Renamed plotSelMat() to plotSel(), and added support for gear-specific plots.

* Renamed scape element SelMat to Sel, with a new Gear column.

* Improved importRes() so CPUE element will not contain empty Gear:Series
  combinations.

* Added example object x.ling and example Coleraine file ling.res.




# scape 0.1-5 (2004-05-24)

*  Updated tutorial.

*  Improved plotCPUE() and plotSurvey() multipanel layout control, using
   'as.table' instead of 'rev.series'.

*  Improved plotCPUE() and plotSurvey() grid lines at low values.




# scape 0.1-4 (2004-05-23)

* Renamed argument 'div' to 'q' in plotCPUE() and plotSurvey() to clarify its
  purpose.

* Improved plotCPUE() and plotSurvey() so internal arguments 'yobs' and 'yfit'
  do not cause a conflict with top-level argument abbreviation 'fit'.

* Changed plotSelMat() so default cex.points=1.

* Improved importRes() so needless warnings are suppressed.

* Improved plotCPUE() and plotSurvey() so strip labels are more informative.

* Added argument 'fitted.also' to plotCPUE(), plotLA(), and plotSurvey().

* Improved importRes() so it looks for correctly spelled Number_of_survey_C@A
  flag.

* Improved x.cod so it contains CAs element.

* Updated tutorial.

* Improved importRes() so maturity is calculated without NaN values.




# scape 0.1-3 (2004-05-19)

* Updated tutorial.




# scape 0.1-2 (2004-05-18)

* Added tutorial.

* Added example Coleraine files cod.res, l_at_age.txt, oreo.res, oreo.txt, and
  sbw.res.

* Improved plotN() to draw smarter y axis.

* Changed importRes() so imported Coleraine models have attributes class="scape"
  and info=[path].

* Removed functions importCor(), plotB(), and plotPearsonCLc().




# scape 0.1-1 (2004-05-12)

* Added example objects x.cod, x.oreo, and x.sbw.




# scape 0.1-0 (2004-05-11)

* Added functions importCor(), importRes(), plotB(), plotCA(), plotCL(),
  plotCPUE(), plotLA(), plotN(), plotPearsonCLc(), plotSelMat(), and
  plotSurvey().
