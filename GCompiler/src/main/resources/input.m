A = load("inputfile",4,4);
B = binarize(A);
C = B' * B;
D = C ./ maxValue(C)
