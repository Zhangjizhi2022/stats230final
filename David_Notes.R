## Summary Statistics

An essential element of their work, which represents a significant departure
from all existing subsampling methodologies, is that it introduces a probability
distribution on the set of subsamples. 

This is equation (12) where U and \Delta_{n}(U) are as defined.

The other proposed methodologies have assumed uniform distribution on the subsamples. This is when
\varepsilon = 0.

Moving away to general models amounts to relaxing the sufficient
statistics existence assumption as well as the $\varepsilon -> \infty$

Instead of sufficient statistics, one uses summary statistics.
Examples are given in equation (17). 

## Transition Kernel

We can think of ISS-MCMC as producing a Markov chain on the extended
space $\Theta \times U_{n}$. The sequence of subsamples $\{U_{i}, i \in \mathbb{N}\}$
is randomly updated in a way that favors those subsets whose
summary statistics vector is close to that of the full dataset

So the transition kernel is a symmetric on 