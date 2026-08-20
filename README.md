# Scrooge McDuck <img src="assets/scrooge-mcduck-sticker.png" align="right" width="120" alt="Scrooge McDuck"/>

<!-- badges: start -->

[![](https://img.shields.io/github/last-commit/<user>/Scrooge_McDuck.svg)](https://github.com/<user>/Scrooge_McDuck/commits/main) [![Made with Quarto](https://img.shields.io/badge/Made%20with-Quarto-2b8cbe)](https://quarto.org) [![Data: OECD ReICO](https://img.shields.io/badge/Data-OECD%20ReICO-lightgrey)](https://data-explorer.oecd.org/vis?df%5Bag%5D=OECD.STI.STP&df%5Bds%5D=DisseminateFinalDMZ&df%5Bid%5D=DSD_REICO_FULL%40DF_RDL&lc=en)

<!-- badges: end -->

<br>

Money, job security and happiness across 36 countries — does a PhD pay off?

A Quarto website built on 1,359 records from the OECD [Research and Innovation Careers Observatory (ReICO)](https://data-explorer.oecd.org/vis?df%5Bag%5D=OECD.STI.STP&df%5Bds%5D=DisseminateFinalDMZ&df%5Bid%5D=DSD_REICO_FULL%40DF_RDL&lc=en), comparing earnings, hours worked, contract type and well-being for Master's and doctorate holders (2022).

## Pages

Explore the [website](https://rmauron.github.io/Scrooge_McDuck/).

| Page | Content |
| --- | --- |
| [`Follow the money`](https://rmauron.github.io/Scrooge_McDuck/analysis_1.html) | Follow the money — earnings by degree, country and age |
| [`Happiness on a map`](https://rmauron.github.io/Scrooge_McDuck/analysis_4.html) | Happiness on a map — interactive Shinylive map |
| [`Find your match`](https://rmauron.github.io/Scrooge_McDuck/analysis_3.html) | Find your match — country recommendation from your priorities |
| [`Behind the data`](https://rmauron.github.io/Scrooge_McDuck/analysis_2.html) | Behind the data — cleaning and reshaping the OECD extract |

## Run locally

```bash
Rscript -e 'renv::restore()'
quarto preview
```

## Team

Agne, Miren, Maryam and Raphaël — [RaukR 2026](https://nbisweden.github.io/raukr-2026/).
