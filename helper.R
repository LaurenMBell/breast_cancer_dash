#helper script for some of the meatier functions in the dashboard

about_overview <- function() {
  print("The data used in this dashboard is from this Kaggle 
        dataset: https://www.kaggle.com/datasets/raghadalharbi/breast-cancer-gene-expression-profiles-metabric /n
        It is a collection of transcriptomic, genomic, and long-term clinical data collected into the METABRIC
        database.")
}

about_breast_cancer <- function() {
  print("")
}

about_metabric <- function() {
  print("The Molecular Taxonomy of Breast Cancer International Consortium, or METABRIC, is a database
        of targeted sequencing data of ~2000 primary breast cancer samples, with their corresponding
        clinical and genomic data. It was collected by Professor Carlos Caldas (Cambridge Research 
        Institute) and Professor Sam Aparico (British Columbia Cancer Centre), and was first published in 
        Nature Communications in their paper, 'The somatic mutation profiles of 2,433 breast cancers refine 
        their genomic and transcriptomic landscapes' (linked below). They sequenced 173 genes in breast tumors 
        with copy number aberration, as well as collected genomic and long-term clinical follow-up data. As a
        result, they found 40 mutation-driver (or, Mut-driver) genes, and several important associations between
        factors contirubting to cancer and overall survival. 
        
        \n 
        Pereira et al, 2016: https://www.nature.com/articles/ncomms11479 ")
}

about_transcriptomics <- function() {
  print("hello world one more")
}