% load network dataset
numVertices = 10;
N = load("network.csv", numVertices, numVertices);

% create the adjacency matrix
A = spones(N);

% outdegree per vertex
d = sum(A,2);

% create the column-stochastic transition matrix
T = (diag(1 ./ d) * A)';

% initialize the ranks
r_0 = ones(numVertices, 1) / numVertices;

% compute PageRank
e = ones(numVertices, 1);

ranks = fixpoint(r_0, @(r) (.85 * T * r + .15 * e));

% save result
ranks