<p align="center">
  <img src="assets/scrooge-mcduck-sticker.png" alt="Scrooge McDuck" width="220">
</p>

# Scrooge McDuck

Money, job security and happiness across 36 countries — does a PhD pay off?

A Quarto website built on 1,359 records from the OECD [Research and Innovation Careers Observatory (ReICO)](https://data-explorer.oecd.org/vis?df%5Bag%5D=OECD.STI.STP&df%5Bds%5D=DisseminateFinalDMZ&df%5Bid%5D=DSD_REICO_FULL%40DF_RDL&lc=en), comparing earnings, hours worked, contract type and well-being for Master's and doctorate holders (2022).

## Pages

| Page | Content |
| --- | --- |
| [`Follow the money`](analysis_1.qmd) | Follow the money — earnings by degree, country and age |
| [`Happiness on a map`](analysis_4.qmd) | Happiness on a map — interactive Shinylive map |
| [`Find your match`](analysis_3.qmd) | Find your match — country recommendation from your priorities |
| [`Behind the data`](analysis_2.qmd) | Behind the data — cleaning and reshaping the OECD extract |

## Run locally

```bash
Rscript -e 'renv::restore()'
quarto preview
```

## Team

Agne, Miren, Maryam and Raphaël — [RaukR 2026](https://nbisweden.github.io/raukr-2026/).
