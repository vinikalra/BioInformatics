# Assign sequences
seq1 = "gctgctggaaggggagctggccggtgggccatggccggctgcaggctctgggtttcgctgctgctggcggcggcgttggcttgcttggccacggcactgt"
seq1 = substring(seq1, seq(1:nchar(seq1)), seq(1:nchar(seq1)))
seq2 = "acgtgattcgccgataagtcacgggggcgccgctcacctgaccagggtctcacgtggccagccccctccgagaggggagaccagcgggccatgacaagct"
seq2 = substring(seq2, seq(1:nchar(seq2)), seq(1:nchar(seq2)))
f_seq1 = c()
f_seq2 = c()
len1 = length(seq1)+1
len2 = length(seq2) +1

# Initialize scoring variables
match_s = 5
mismatch_s = -4
gap_s = -8

# Initialize nw matrix
nwmatrix = matrix(0,nrow = len1, ncol= len2)
for(i in (2:len1)){
  nwmatrix[i,1] = nwmatrix[i-1] + gap_s
}

for(i in (2:len2)){
  nwmatrix[1,i] = nwmatrix[i-1] + gap_s
}

# Initialize direction matrix
direction = matrix("X", nrow=len1, ncol=len2)

# Function for assigning score to cell with given index, i->row, j->col
score_cell <- function(i,j)
{
  up = nwmatrix[i-1, j] + gap_s
  left = nwmatrix[i, j-1] + gap_s
  
  if(seq1[i-1] == seq2[j-1])
  {
    diag = nwmatrix[i-1, j-1] + match_s
  }
  else
  {
    diag = nwmatrix[i-1, j-1] + mismatch_s
  }
  
  nwmatrix[i, j] <<- max(up, left, diag)
  options = c(up, left, diag)
  ind = which(options==max(options))
  
  if(3 %in% ind)
  {
    direction[i,j] <<- "d"
  }
  else if(2 %in% ind)
  {
    direction[i,j] <<- "l"
  }
  else
  {
    direction[i,j] <<- "u"
  }
}

# Scoring
for (i in (2:len1)){
  for (j in (2:len2)){
    score_cell(i,j)
  }
}

trace_alignment <- function()
{
  
  i =len1
  j= len2
  while(i != 1 && j != 1)
  {
    cur <- nwmatrix[i, j]
    d <- direction[i, j]
    if(d == "d")
    {
      f_seq1[length(f_seq1)+1] <<- seq1[i-1]
      f_seq2[length(f_seq2)+1] <<- seq2[j-1]
      i = i-1
      j = j-1
    }
    else if(d== "l")
    {
      f_seq1[length(f_seq1)+1] <<- "-"
      f_seq2[length(f_seq2)+1] <<- seq2[j-1]
      j = j-1
    }
    else
    {
      f_seq1[length(f_seq1)+1] <<- seq1[i-1]
      f_seq2[length(f_seq2)+1] <<- "-"
      i = i-1
    }
  }
}

# print(nwmatrix)
# print(direction)
trace_alignment()
# print(rev(f_seq1))
# print(rev(f_seq2))

print(paste(rev(f_seq1), collapse = ""))
print(paste(rev(f_seq2), collapse = ""))

