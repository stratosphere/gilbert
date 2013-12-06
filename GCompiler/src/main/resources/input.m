A = load("inputfile",10,10);
B = binarize(A);
C = B' * B;
D = C ./ maxValue(C)
