
[![Binder](https://mybinder.org/badge.svg)](https://mybinder.org/v2/gh/DataHaskell/data-glue/9a5b348?urlpath=lab/tree/tutorials/jlab_hvega.ipynb)

# Haskell Data-Glue #

Data-glue integrates several data analysis libraries into a single project with iHaskell frontend. It aims to provide a directly usable data science environment and ensure compatibility among all the gathered libraries.

Data-Glue contains:
- **_Data structures_**
  - **foldl** v1.4.2 [![Hackage](https://img.shields.io/hackage/v/foldl.svg)](https://hackage.haskell.org/package/foldl) [![foldl](http://stackage.org/package/foldl/badge/lts-11)](http://stackage.org/lts-11/package/foldl) [![foldl](http://stackage.org/package/foldl/badge/nightly)](http://stackage.org/nightly/package/foldl)
  - **Frames** v0.4.0 [![Hackage](https://img.shields.io/hackage/v/Frames.svg)](https://hackage.haskell.org/package/Frames) [![Frames](http://stackage.org/package/Frames/badge/lts-11)](http://stackage.org/lts-11/package/Frames) [![Frames](http://stackage.org/package/Frames/badge/nightly)](http://stackage.org/nightly/package/Frames)
  - **vinyl** v0.8.1 [![Hackage](https://img.shields.io/hackage/v/vinyl.svg)](https://hackage.haskell.org/package/vinyl) [![vinyl](http://stackage.org/package/vinyl/badge/lts-11)](http://stackage.org/lts-11/package/vinyl) [![vinyl](http://stackage.org/package/vinyl/badge/nightly)](http://stackage.org/nightly/package/vinyl)

- **_QuasiQuoter_**
  - **PyF** v0.6.1.0 [![Hackage](https://img.shields.io/hackage/v/PyF.svg)](https://hackage.haskell.org/package/PyF) [![PyF](http://stackage.org/package/PyF/badge/lts-11)](http://stackage.org/lts-11/package/PyF) [![PyF](http://stackage.org/package/PyF/badge/nightly)](http://stackage.org/nightly/package/PyF)
  - **string-qq** v0.0.2 [![Hackage](https://img.shields.io/hackage/v/string-qq.svg)](https://hackage.haskell.org/package/string-qq) [![string-qq](http://stackage.org/package/string-qq/badge/lts-11)](http://stackage.org/lts-11/package/string-qq) [![string-qq](http://stackage.org/package/string-qq/badge/nightly)](http://stackage.org/nightly/package/string-qq)

- **_Interoperability_**
  - **inline-r** v0.9.2 [![Hackage](https://img.shields.io/hackage/v/inline-r.svg)](https://hackage.haskell.org/package/inline-r) [![inline-r](http://stackage.org/package/inline-r/badge/lts-11)](http://stackage.org/lts-11/package/inline-r) [![inline-r](http://stackage.org/package/inline-r/badge/nightly)](http://stackage.org/nightly/package/inline-r)

- **_Visualisation_**
  - **ihaskell** v0.9.1.0 [![Hackage](https://img.shields.io/hackage/v/ihaskell.svg)](https://hackage.haskell.org/package/ihaskell) [![ihaskell](http://stackage.org/package/ihaskell/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell) [![ihaskell](http://stackage.org/package/ihaskell/badge/nightly)](http://stackage.org/nightly/package/ihaskell)
    - **ihaskell-aeson** v0.3.0.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-aeson.svg)](https://hackage.haskell.org/package/ihaskell-aeson) [![ihaskell-aeson](http://stackage.org/package/ihaskell-aeson/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-aeson) [![ihaskell-aeson](http://stackage.org/package/ihaskell-aeson/badge/nightly)](http://stackage.org/nightly/package/ihaskell-aeson)
    - **ihaskell-blaze** v0.3.0.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-blaze.svg)](https://hackage.haskell.org/package/ihaskell-blaze) [![ihaskell-blaze](http://stackage.org/package/ihaskell-blaze/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-blaze) [![ihaskell-blaze](http://stackage.org/package/ihaskell-blaze/badge/nightly)](http://stackage.org/nightly/package/ihaskell-blaze)
    - **ihaskell-charts** v0.3.0.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-charts.svg)](https://hackage.haskell.org/package/ihaskell-charts) [![ihaskell-charts](http://stackage.org/package/ihaskell-charts/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-charts) [![ihaskell-charts](http://stackage.org/package/ihaskell-charts/badge/nightly)](http://stackage.org/nightly/package/ihaskell-charts)
    - **ihaskell-diagrams** v0.3.2.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-diagrams.svg)](https://hackage.haskell.org/package/ihaskell-diagrams) [![ihaskell-diagrams](http://stackage.org/package/ihaskell-diagrams/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-diagrams) [![ihaskell-diagrams](http://stackage.org/package/ihaskell-diagrams/badge/nightly)](http://stackage.org/nightly/package/ihaskell-diagrams)
    - **ihaskell-gnuplot** v0.1.0.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-gnuplot.svg)](https://hackage.haskell.org/package/ihaskell-gnuplot) [![ihaskell-gnuplot](http://stackage.org/package/ihaskell-gnuplot/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-gnuplot) [![ihaskell-gnuplot](http://stackage.org/package/ihaskell-gnuplot/badge/nightly)](http://stackage.org/nightly/package/ihaskell-gnuplot)
    - **ihaskell-hatex** v0.2.1.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-hatex.svg)](https://hackage.haskell.org/package/ihaskell-hatex) [![ihaskell-hatex](http://stackage.org/package/ihaskell-hatex/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-hatex) [![ihaskell-hatex](http://stackage.org/package/ihaskell-hatex/badge/nightly)](http://stackage.org/nightly/package/ihaskell-hatex)
    - **ihaskell-inline-r** v0.1.1.0 [![Hackage](https://img.shields.io/hackage/v/ihaskell-inline-r.svg)](https://hackage.haskell.org/package/ihaskell-inline-r) [![ihaskell-inline-r](http://stackage.org/package/ihaskell-inline-r/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-inline-r) [![ihaskell-inline-r](http://stackage.org/package/ihaskell-inline-r/badge/nightly)](http://stackage.org/nightly/package/ihaskell-inline-r)
    - **ihaskell-juicypixels** v1.1.0.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-juicypixels.svg)](https://hackage.haskell.org/package/ihaskell-juicypixels) [![ihaskell-juicypixels](http://stackage.org/package/ihaskell-juicypixels/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-juicypixels) [![ihaskell-juicypixels](http://stackage.org/package/ihaskell-juicypixels/badge/nightly)](http://stackage.org/nightly/package/ihaskell-juicypixels)
    - **ihaskell-magic** v0.3.0.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-magic.svg)](https://hackage.haskell.org/package/ihaskell-magic) [![ihaskell-magic](http://stackage.org/package/ihaskell-magic/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-magic) [![ihaskell-magic](http://stackage.org/package/ihaskell-magic/badge/nightly)](http://stackage.org/nightly/package/ihaskell-magic)
    - **ihaskell-plot** v0.3.0.1 [![Hackage](https://img.shields.io/hackage/v/ihaskell-plot.svg)](https://hackage.haskell.org/package/ihaskell-plot) [![ihaskell-plot](http://stackage.org/package/ihaskell-plot/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-plot) [![ihaskell-plot](http://stackage.org/package/ihaskell-plot/badge/nightly)](http://stackage.org/nightly/package/ihaskell-plot)
    - **ihaskell-widgets** v0.2.3.2 [![Hackage](https://img.shields.io/hackage/v/ihaskell-widgets.svg)](https://hackage.haskell.org/package/ihaskell-widgets) [![ihaskell-widgets](http://stackage.org/package/ihaskell-widgets/badge/lts-11)](http://stackage.org/lts-11/package/ihaskell-widgets) [![ihaskell-widgets](http://stackage.org/package/ihaskell-widgets/badge/nightly)](http://stackage.org/nightly/package/ihaskell-widgets)
  - **hvega** v0.1.0.0 [![Hackage](https://img.shields.io/hackage/v/hvega.svg)](https://hackage.haskell.org/package/hvega) [![hvega](http://stackage.org/package/hvega/badge/lts-11)](http://stackage.org/lts-11/package/hvega) [![hvega](http://stackage.org/package/hvega/badge/nightly)](http://stackage.org/nightly/package/hvega)

## Tutorials ##

This contains some [interactive tutorials](https://github.com/DataHaskell/data-glue/tree/master/tutorials) that show how Haskell can be used for
typical data science workflows.

## Datasets ##

from https://archive.ics.uci.edu/ml/datasets.html
