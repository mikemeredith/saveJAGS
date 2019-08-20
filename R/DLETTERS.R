
# DLETTERS is a vector of sorted doubled upper-case letters, AA, AB, AC, ... ZZ.
# Used as chain IDs from version 0.0.3.9015.

DLETTERS <- as.vector(t(outer(LETTERS, LETTERS, paste0)))
