# metacrawlr


``` r
if(!require(devtools)){install.packages("devtools")}
devtools::install_github("marbotte/metacrawlr")
```

``` r
library(metacrawlr)
```

## Metadata extraction from the datadir of an IPT instance

Here, as an example, I downloaded the datadir of an IPT instance on my
computer:

``` r
DATADIR<-"/home/marius_gt/Travail/Data/IPTs/permisos-3.1.5/resources/"
```

First we list the files and their encodings/languages (this takes a lot
of time, if you are confident in knowing the encodings, I suggest using
the encoding argument instead).

``` r
filesPermi<-ipt_listFiles(DATADIR)
```

    Guessing encoding of the files
    ...

    DONE

``` r
knitr::kable(head(filesPermi))
```

| name | encoding | language | extension | dataFolder | full_name |
|:---|:---|:---|:---|:---|:---|
| 00000974_gallineral_20190815-1.0.rtf | ISO-8859-1 | pt | rtf | 00000974_gallineral_20190815 | /home/marius_gt/Travail/Data/IPTs/permisos-3.1.5/resources//00000974_gallineral_20190815/00000974_gallineral_20190815-1.0.rtf |
| 00000974_gallineral_20190815-1.1.rtf | ISO-8859-1 | pt | rtf | 00000974_gallineral_20190815 | /home/marius_gt/Travail/Data/IPTs/permisos-3.1.5/resources//00000974_gallineral_20190815/00000974_gallineral_20190815-1.1.rtf |
| dwca-1.0.zip | ISO-8859-1 | pt | zip | 00000974_gallineral_20190815 | /home/marius_gt/Travail/Data/IPTs/permisos-3.1.5/resources//00000974_gallineral_20190815/dwca-1.0.zip |
| dwca-1.1.zip | ISO-8859-1 | pt | zip | 00000974_gallineral_20190815 | /home/marius_gt/Travail/Data/IPTs/permisos-3.1.5/resources//00000974_gallineral_20190815/dwca-1.1.zip |
| eml-1.0.xml | ISO-8859-1 | pt | xml | 00000974_gallineral_20190815 | /home/marius_gt/Travail/Data/IPTs/permisos-3.1.5/resources//00000974_gallineral_20190815/eml-1.0.xml |
| eml-1.1.xml | ISO-8859-1 | pt | xml | 00000974_gallineral_20190815 | /home/marius_gt/Travail/Data/IPTs/permisos-3.1.5/resources//00000974_gallineral_20190815/eml-1.1.xml |

``` r
extractedXml<-ipt_extractXml(filesPermi, modeExtract ="both" )
```

``` r
metaList_permi<-metaListFromXml(extractedXml)
```

``` r
structPermi<-extractStructureListDocuments(metaList_permi)
gnv_permi<-groupsAndVariables(structPermi)
tabs_permi<-extractTables(metaList_permi,structPermi,gpsAndVar = gnv_permi)
```

``` r
xlFile_permi<-"../data_metadatos_catalogos/exportPermi.xlsx"
sqlite_permi<-"../data_metadatos_catalogos/permi.sqlite"
tabs_permi<-sqlize_extractedTables(tabs_permi)
exportXL(tabs_permi,file=xlFile_permi)
```

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : onde se encuentr ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : fico
    de todas la ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : ato donde se enc ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : tro fotográfico
     ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : General para la  ... cia Chao1 y ACE. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : rdeidae  Ardea   al ... ia   albiventris  is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : ocorax affinis
    L ... phis marsupialis is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : iaca
    Musa paradi ... ta Bojer ex Sims is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : llo
    Aniba sp. 2
     ... yrsine┬ácoriacea is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : eus griseus
    Sten ... nocereus griseus is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : ma aggregatum (P ... x Willd.) Britto is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : onde se encuentr ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : fico
    de todas la ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : ato donde se enc ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : tro fotográfico
     ... itas vasculares. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, :  la literatura ( ... io de Caramanta. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : berturas vegetal ... da por el IDEAM. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : General para la  ... cia Chao1 y ACE. is truncated. 
    Number of characters exeed the limit of 32767.

    Warning in wb$writeData(df = x, colNames = TRUE, sheet = sheet, startRow = startRow, : RV:2017:E01BSCaP ... ancton_Fondo-R84 is truncated. 
    Number of characters exeed the limit of 32767.

``` r
dbpermi<-exportSQLite(tabs_permi,sqlite_file = sqlite_permi)
```
