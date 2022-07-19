
#Using https://www.bioinformatics.org/sms2/random_dna.html website as DNA generator

DNA=c("gctgctttccatcctacggtatttcacaactggcacaggccaataataagtgccactataccggtgagttcgaccaggcatctggcattccggtatcatatacagacggggttgtgaaacacgactccctttgattactctcttccaagacaagcgggatgactggcacgccgcggagtggaatcctctcgacggtttctttcagctttagtcccacccagtcgcttcattgatttttaggtccagacaccagtttgggcataagcaactgaattcgcacgagctgattggatcgctaccactgaagtgcgagtccaattggttgcggcgtcccgctcactctggtagaggatttatcgccatcaactaaagatcatattggactagtgacgagccagatctacctgttggttgtactctgataatctgttcagtatgggcagctcagaaagagcgtattagcccgaggaactcggtagggcgcttactgacggcccctcaaattaattgttccagatgtatcggcagcagtttctcactgctgacgtcggggcattgggtcccaagctcaagtccacttaagtttttgagttgttgctaaccgttacaggttgcgcagactcgtctgcgcagtcaagctcggtactggggctactagcgcctacacaccatataaggtcagatgtaaataggagaatgtagcagagcttgctgtagcaagtcgtgtgtataattctcctcttcaaggcctattgaaacaattgggacatgtatcgaacaacacgtaggtgtcaacccttctcgcagtagagaatttgggcccatacggagcgtttcaccacgggccatacacgtgacctcgcttatccgctcctctctactatggtagcatcgtgggagccgcgtgctccgcgatccgcgaatcgtgaataactggttataaatgaacgaccccaataatctctaaaagttctggtagatcacatacacgacttttttt")
DNA=strsplit(DNA, split = "")
DNA=DNA[[1]]


## ---------------Among of function that returns the proportion of the nucleotides---------

#1
proportion=function(data_dna,word1,word2){
  counts=0
  total=0
  
  
  for(i in 1:(length(data_dna)-1)){
    if(data_dna[i]==word1)
      {
      total=total+1
    if(data_dna[i+1]==word2)
      counts=counts+1 
    }
  }
  return(counts/total)
}


#2
nucleo_finder=function(data_dna){
  data_dna=levels(factor(data_dna))
  return(data_dna)
}

#3 final
nucleo_prob_matrix=function(data_dna){
  nucleotides=nucleo_finder(data_dna)
  nucleo_matrix=matrix(rep(0,length(nucleotides)**2),byrow = T,nrow=4)
  
  for(j in 1:length(nucleotides))
    for(i in 1:length(nucleotides)){
      nucleo_matrix[j,i]=proportion(data_dna,nucleotides[j],nucleotides[i])
    }
  
     
return(nucleo_matrix)
     
    
}

nucleo_prob_matrix(DNA)










