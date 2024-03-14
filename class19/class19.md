# Class 19: Cancer Mutation Mini-Project
Raquel Gonzalez (A16207442)

> Q2. \[6pts\] What are the tumor specific mutations in this particular
> case (e.g.A130V)?

``` r
library(bio3d)
sequence <- read.fasta("A16207442_mutant_seq.fa")
sequence
```

                   1        .         .         .         .         .         60 
    wt_healthy     MAALSGGGGGGAEPGQALFNGDMEPEAGAGAGAAASSAADPAIPEEVWNIKQMIKLTQEH
    mutant_tumor   MAALSGGGGGGAEPGQALFNGDMEPEAGAGAGAAASSAADPAIPEEVWNIKQMIKLTQEH
                   ************************************************************ 
                   1        .         .         .         .         .         60 

                  61        .         .         .         .         .         120 
    wt_healthy     IEALLDKFGGEHNPPSIYLEAYEEYTSKLDALQQREQQLLESLGNGTDFSVSSSASMDTV
    mutant_tumor   IEALLDKFGGEHNPPSIYLEAYEEYTSKLDALQQREQQLLESLGNGTDFSVSSSASMDTV
                   ************************************************************ 
                  61        .         .         .         .         .         120 

                 121        .         .         .         .         .         180 
    wt_healthy     TSSSSSSLSVLPSSLSVFQNPTDVARSNPKSPQKPIVRVFLPNKQRTVVPARCGVTVRDS
    mutant_tumor   TSSSSSSLSVLPSSLSVFQNPTDVARSNPKSPQKPIVRVFLPNKQRTVVPARCGVTVRDS
                   ************************************************************ 
                 121        .         .         .         .         .         180 

                 181        .         .         .         .         .         240 
    wt_healthy     LKKALMMRGLIPECCAVYRIQDGEKKPIGWDTDISWLTGEELHVEVLENVPLTTHNFVRK
    mutant_tumor   LKKALMMRGLIPECCAVYRIQDGEKKPIGWDTDISWLTGEELHVEVLENVPLTTHNFVRK
                   ************************************************************ 
                 181        .         .         .         .         .         240 

                 241        .         .         .         .         .         300 
    wt_healthy     TFFTLAFCDFCRKLLFQGFRCQTCGYKFHQRCSTEVPLMCVNYDQLDLLFVSKFFEHHPI
    mutant_tumor   TFFTLAFCDFCRKLLFQGFRCQTCGYKFHQRCSTEVPLMCVNYDQLDLLFVSKFFEHHPI
                   ************************************************************ 
                 241        .         .         .         .         .         300 

                 301        .         .         .         .         .         360 
    wt_healthy     PQEEASLAETALTSGSSPSAPASDSIGPQILTSPSPSKSIPIPQPFRPADEDHRNQFGQR
    mutant_tumor   PQEEASLAETALTSGSSPSAPASDSIGPQILTSPSPSKSIPIPQPFRPADEDHRNQFGQR
                   ************************************************************ 
                 301        .         .         .         .         .         360 

                 361        .         .         .         .         .         420 
    wt_healthy     DRSSSAPNVHINTIEPVNIDDLIRDQGFRGDGGSTTGLSATPPASLPGSLTNVKALQKSP
    mutant_tumor   DRSSSAPNVHINTIEPVNIDDLIRDQGFRGDGGSTTGLSATPPASLPGSLTNVKALQKSP
                   ************************************************************ 
                 361        .         .         .         .         .         420 

                 421        .         .         .         .         .         480 
    wt_healthy     GPQRERKSSSSSEDRNRMKTLGRRDSSDDWEIPDGQITVGQRIGSGSFGTVYKGKWHGDV
    mutant_tumor   GPQRERKSSSSSEDRNRMKTLGRRDSSDDWEIPDGQITVGQRIGSGSFGTVYKGKWHGDV
                   ************************************************************ 
                 421        .         .         .         .         .         480 

                 481        .         .         .         .         .         540 
    wt_healthy     AVKMLNVTAPTPQQLQAFKNEVGVLRKTRHVNILLFMGYSTKPQLAIVTQWCEGSSLYHH
    mutant_tumor   AVKMLNVTAPTPQQLQAFKNEVGVLRKTRHVNILLFMGYSTKPQLAIVTQWCEGSSLYHH
                   ************************************************************ 
                 481        .         .         .         .         .         540 

                 541        .         .         .         .         .         600 
    wt_healthy     LHIIETKFEMIKLIDIARQTAQGMDYLHAKSIIHRDLKSNNIFLHEDLTVKIGDFGLATV
    mutant_tumor   LHIIETKFEMIKLIDIARQTAQGMDYLHAKSIIHRDLKSNNIFLHEDLTVKIGDFGLATV
                   ************************************************************ 
                 541        .         .         .         .         .         600 

                 601        .         .         .         .         .         660 
    wt_healthy     KSRWSGSHQFEQLSGSILWMAPEVIRMQDKNPYSFQSDVYAFGIVLYELMTGQLPYSNIN
    mutant_tumor   KSRWSGSHQFEQLSGSILWMAPEVIRMQDKNPYSFQSDVYAFGIVLYELMTGQLPYSVIN
                   ********************************************************* ** 
                 601        .         .         .         .         .         660 

                 661        .         .         .         .         .         720 
    wt_healthy     NRDQIIFMVGRGYLSPDLSKVRSNCPKAMKRLMAECLKKKRDERPLFPQILASIELLARS
    mutant_tumor   NRDQIIFMVGRGYLSPDLSKVRSNEPKAMKRLMAECLKKKRDERPLYPQILASIELLARS
                   ************************ *********************^************* 
                 661        .         .         .         .         .         720 

                 721        .         .         .         .     766 
    wt_healthy     LPKIHRSASEPSLNRAGFQTEDFSLYACASPKTPIQAGGYGAFPVH
    mutant_tumor   LPKIHRSASEPSLNRAGFQTEDFSLYACASPKTPIQAGGYGAFPVH
                   ********************************************** 
                 721        .         .         .         .     766 

    Call:
      read.fasta(file = "A16207442_mutant_seq.fa")

    Class:
      fasta

    Alignment dimensions:
      2 sequence rows; 766 position columns (766 non-gap, 0 gap) 

    + attr: id, ali, call

Score residue conservation

``` r
which(conserv(sequence) < 1.0)
```

    [1] 658 685 707

``` r
sequence$ali[,658]
```

      wt_healthy mutant_tumor 
             "N"          "V" 

``` r
sequence$ali[,685]
```

      wt_healthy mutant_tumor 
             "C"          "E" 

``` r
sequence$ali[,707]
```

      wt_healthy mutant_tumor 
             "F"          "Y" 

``` r
sequence$ali[,which(conserv(sequence) < 1.0)]
```

                 [,1] [,2] [,3]
    wt_healthy   "N"  "C"  "F" 
    mutant_tumor "V"  "E"  "Y" 
