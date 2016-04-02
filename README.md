## rpivotGadget:  RStudio add-in for rpivotTable

This package provide an RStudio add-in wrapper arount the [rpivotTable](https://github.com/smartinsightsfromdata/rpivotTable) HTML widget.

![Screenshot](Screenshot.JPG)

#### Installation

Currently depends on a custom version of MiniUI due to a [bug](https://github.com/rstudio/miniUI/issues/5) in the tabStrip. It will still work without this but you will need to click in the ACE editor before the text will update. This does not effect the pivot table function. 

```R
devtools::install_github("dkilfoyle/rpivotGadget"))
```
