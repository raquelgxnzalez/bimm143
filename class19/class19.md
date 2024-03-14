# Class 19: Cancer Mutation Mini-Project
Raquel Gonzalez (A16207442)

## Background

To identify somatic mutations in a tumor, DNA from the tumor is
sequenced and compared to DNA from normal tissue in the same individual
using variant calling algorithms.

Comparison of tumor sequences to those from normal tissue (rather than
‘the human genome’) is important to ensure that the detected differences
are not germline mutations.

To identify which of the somatic mutations leads to the production of
aberrant proteins, the location of the mutation in the genome is
inspected to identify non-synonymous mutations (i.e. those that fall
into protein coding regions and change the encoded amino acid).

> Q1: Download your student specific sequences. These sequences resulted
> from an NGS analysis of patient healthy and tumor tissue. What protein
> do these sequences correspond to?

``` r
library(bio3d)
sequence <- read.fasta("A16207442_mutant_seq.fa")
#head(blast.pdb(sequence))
#omitted for clarity: Top Hit 6Q0K_A
```

``` r
pdb.annotate("6Q0K_A")
```

           structureId chainId macromoleculeType chainLength experimentalTechnique
    6Q0K_A        6Q0K       A           Protein         805                    EM
           resolution scopDomain
    6Q0K_A        6.8       <NA>
                                                                    pfam ligandId
    6Q0K_A Protein tyrosine and serine/threonine kinase (PK_Tyr_Ser-Thr)     <NA>
           ligandName       source                      structureTitle
    6Q0K_A       <NA> Homo sapiens Structure of a MAPK pathway complex
                                 citation rObserved rFree rWork spaceGroup
    6Q0K_A Park, E., et al. Nature (2019)        NA    NA    NA       <NA>

B-Raf proto-oncogene, serine/threonine kinase (BRAF)

> Q2: What are the tumor specific mutations in this particular case
> (e.g.A130V)?

``` r
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

Score residue conservation to find which amino acids were mutated.

``` r
which(conserv(sequence) < 1.0)
```

    [1] 658 685 707

``` r
sequence$ali[,which(conserv(sequence) < 1.0)]
```

                 [,1] [,2] [,3]
    wt_healthy   "N"  "C"  "F" 
    mutant_tumor "V"  "E"  "Y" 

The tumor specific mutations are N658V, C685E, and F707Y.

> Q3: Do your mutations cluster to any particular domain and if so give
> the name and PFAM id of this domain? Alternately note whether your
> protein is single domain and provide it’s PFAM id/accession and name
> (e.g. PF00613 and PI3Ka).

A search on PFAM Interpro yielded the following domains: protein kinase
domain (PF00069) and Serine-threonine/tyrosine-protein kinase, catalytic
doman (PF07714).

> Q4: Using the [NCI-GDC](https://portal.gdc.cancer.gov/) list the
> observed top 2 missense mutations in this protein (amino acid
> substitutions)?

BRAF V640E and BRAF V640M

> Q5: What two TCGA projects have the most cases affected by mutations
> of this gene? (Give the TCGA “code” and “Project Name” for example
> “TCGA-BRCA” and “Breast Invasive Carcinoma”).

TCGA-THCA Thyroid Carcinoma and TCGA-SKCM Skin Cutaneous Melanoma.

> Q6: List one RCSB PDB identifier with 100% identity to the wt_healthy
> sequence and detail the percent coverage of your query sequence for
> this known structure? Alternately, provide the most similar in
> sequence PDB structure along with it’s percent identity, coverage and
> E-value. Does this structure “cover” (i.e. include or span the amino
> acid residue positions) of your previously identified tumor specific
> mutations?

Using a blast search of the top match found in Q1: PDB identifier:
NP_004324.2 serine/threonine-protein kinase B-raf isoform 1 \[Homo
sapiens\] Percent Identity: 100% Coverage: 100% E-Value: 0.0

This structure does cover my previously identified tumor specific
mutations, as they share the same amino acid length.

> Q7: Using [AlphaFold
> notebook](https://colab.research.google.com/github/sokrypton/ColabFold/blob/main/AlphaFold2.ipynb)
> generate a structural model using the default parameters for your
> mutant sequence.

Mutant amino acid side chains are shown as spacefill. The protein is
cartoon colored by local alpha fold pLDDT quality score.

![](class19%20q7.png)

> Q8: Considering only your mutations in high quality structure regions
> (with a pLDDT score \> 70) are any of the mutations on the surface of
> the protein and hence have a potential to interfere with
> protein-protein interaction events?

N658V, C685E, and F707Y are embedded moreso in the core of the protein,
and thus have less of a potential to interfere with protein-protein
interaction events.

> Q9: Please comment on how useful and/or reliable you think your
> AlphaFold structural model is for your entire sequence and the main
> domain where your mutations lie? You may wish to compare your model to
> the PDB structure you found in Q6.

I think the AlphaFold structural model is more reliable for the main
domain where the mutations lie than for the entire sequence. The main
domain is more highly conserved with a much higher PLDDT score, but the
remaining sequences of the protein, especially those on the surface,
have very low PLDDT scores.
