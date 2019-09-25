GIDEON
================

``` r
g<-read.csv("2019-09-23 19_27_25.csv",header = F)
head(g)
```

    ##                    V1                          V2                    V3
    ## 1 Abrothrix olivaceus  Trypanosomiasis - American                 Chile
    ## 2    Acerodon jubatus                       Ebola           Philippines
    ## 3    Acinonyx jubatus                     Anthrax               Namibia
    ## 4    Acinonyx jubatus   Bartonellosis - cat borne               Namibia
    ## 5    Acinonyx jubatus   Bartonellosis - cat borne         United States
    ## 6    Acinonyx jubatus               Toxoplasmosis  United Arab Emirates

``` r
dim(g)
```

    ## [1] 3849    3

``` r
g$V1<-as.character(g$V1)
g$V2<-as.character(g$V2)
g$V2<-trimws(g$V2, which = c("l")
       #, whitespace =   "[ \t\r\n]"
       )
head(g$V2
     )
```

    ## [1] "Trypanosomiasis - American" "Ebola"                     
    ## [3] "Anthrax"                    "Bartonellosis - cat borne" 
    ## [5] "Bartonellosis - cat borne"  "Toxoplasmosis"

``` r
all<-read.csv("allmammGID-clean20190812.csv")
head(all)
```

    ##                     Spp Label        Order Zoonoses
    ## 1    Carpitalpa_arendsi     0 Afrosoricida         
    ## 2  Chlorotalpa_duthieae     0 Afrosoricida         
    ## 3  Chlorotalpa_sclateri     0 Afrosoricida         
    ## 4 Cryptochloris_wintoni     0 Afrosoricida         
    ## 5    Cryptochloris_zyli     0 Afrosoricida         
    ## 6    Amblysomus_corriae     0 Afrosoricida

``` r
#subset zoonotic
all3<-all[all$Label ==1,]
head(all3)
```

    ##                    Spp Label        Order                    Zoonoses
    ## 51    Tenrec_ecaudatus     1 Afrosoricida                      Plague
    ## 63 Addax_nasomaculatus     1 Artiodactyla  Spotted fevers - Old World
    ## 64 Addax_nasomaculatus     1 Artiodactyla Echinococcosis - unilocular
    ## 65  Aepyceros_melampus     1 Artiodactyla                 Brucellosis
    ## 66  Aepyceros_melampus     1 Artiodactyla           Cryptosporidiosis
    ## 67  Aepyceros_melampus     1 Artiodactyla Echinococcosis - unilocular

``` r
#subset matched columns
all2<-all3[,c("Spp","Zoonoses")]

all2$Spp<-gsub("_"," ",all2$Spp)
head(all2)
```

    ##                    Spp                    Zoonoses
    ## 51    Tenrec ecaudatus                      Plague
    ## 63 Addax nasomaculatus  Spotted fevers - Old World
    ## 64 Addax nasomaculatus Echinococcosis - unilocular
    ## 65  Aepyceros melampus                 Brucellosis
    ## 66  Aepyceros melampus           Cryptosporidiosis
    ## 67  Aepyceros melampus Echinococcosis - unilocular

``` r
dim(all2)
```

    ## [1] 2107    2

\#check if spp-zoonoses combs match

``` r
g$sp_z<-paste(g$V1,"_",g$V2
              #,sep=""
              )

head(g)
```

    ##                    V1                         V2                    V3
    ## 1 Abrothrix olivaceus Trypanosomiasis - American                 Chile
    ## 2    Acerodon jubatus                      Ebola           Philippines
    ## 3    Acinonyx jubatus                    Anthrax               Namibia
    ## 4    Acinonyx jubatus  Bartonellosis - cat borne               Namibia
    ## 5    Acinonyx jubatus  Bartonellosis - cat borne         United States
    ## 6    Acinonyx jubatus              Toxoplasmosis  United Arab Emirates
    ##                                               sp_z
    ## 1 Abrothrix olivaceus _ Trypanosomiasis - American
    ## 2                         Acerodon jubatus _ Ebola
    ## 3                       Acinonyx jubatus _ Anthrax
    ## 4     Acinonyx jubatus _ Bartonellosis - cat borne
    ## 5     Acinonyx jubatus _ Bartonellosis - cat borne
    ## 6                 Acinonyx jubatus _ Toxoplasmosis

``` r
dim(g)
```

    ## [1] 3849    4

``` r
g_u<-unique(g[,c("V1","V2","sp_z")])
dim(g_u)
```

    ## [1] 2376    3

``` r
all2$sp_z<-paste(all2$Spp,"_",all2$Zoonoses )
head(all2)
```

    ##                    Spp                    Zoonoses
    ## 51    Tenrec ecaudatus                      Plague
    ## 63 Addax nasomaculatus  Spotted fevers - Old World
    ## 64 Addax nasomaculatus Echinococcosis - unilocular
    ## 65  Aepyceros melampus                 Brucellosis
    ## 66  Aepyceros melampus           Cryptosporidiosis
    ## 67  Aepyceros melampus Echinococcosis - unilocular
    ##                                                 sp_z
    ## 51                         Tenrec ecaudatus _ Plague
    ## 63  Addax nasomaculatus _ Spotted fevers - Old World
    ## 64 Addax nasomaculatus _ Echinococcosis - unilocular
    ## 65                  Aepyceros melampus _ Brucellosis
    ## 66            Aepyceros melampus _ Cryptosporidiosis
    ## 67  Aepyceros melampus _ Echinococcosis - unilocular

``` r
dim(all2)
```

    ## [1] 2107    3

``` r
compare<-merge(g,all2,by= "sp_z")
dim(compare)
```

    ## [1] 2813    6

``` r
compare_u<-merge(g_u,all2,by= "sp_z")
dim(compare_u)
```

    ## [1] 1622    5

``` r
missing<-g_u[g_u$sp_z%in%all2$sp_z==F,]
dim(missing)
```

    ## [1] 776   3

``` r
missing_country = merge(missing, g)
dim(missing_country)
```

    ## [1] 1065    4

``` r
names(missing_country) = c("Spp", "Disease", "sp_z", "Country")
missing_country = missing_country[,c("Spp", "Disease", "Country")]
write.csv(missing_country, "missing.compare.csv")
```
