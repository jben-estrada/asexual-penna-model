# Asexual Penna model

## Model description
An implementation of a biological aging model in Fortran 2008 as first introduced by T.J.P. Penna [1]. The model we consider here however conforms more with the description of S. Oliveira [2] than the that of the original with some important modifications, namely the Verhulst factor.

In Oliveira's and Penna's description, Verhulst factor is constant throughout the run. In some implementations, the Verhulst factor is disabled for newborn individuals. Here, we let the Verhulst factor freely vary with the age of individuals.

$$V_a = 1 - w_a\frac{N(t)}{N_{max}},$$

where $V_a$ is the Verhulst factor at age $a$, $w_a$ is the *Verhulst weight* at age $a$, $N(t)$ is the population size at time $t$, and $N_{max}$ is the carrying capacity. The *Verhulst weight* is a real number in the interval [0, 1]. To summarize death by Verhulst factor, an individual dies if a random number $r \in (0, 1)$ is greater than the Verhulst factor. If we let $w_a = 0$, $V_a$ is equal to 1 at age $a$. Since $r$ cannot be greater than 1, individuals at this age cannot randomly die due to the Verhulst factor. On the other hand, if $w_a = 1$, $V_a$ becomes the original Verhulst factor.

Varying Verhulst factor can be useful when, say, modelling *survivability* of some species where younger ones are more prone to dying than the older ones due to environmental factors.
___
## Reference
1. Thadeu Penna. "A Bit-String Model for Biological Aging". In: *Journal of Statistical Physics* 78 (Mar. 1995). DOI: 10.1007/BF02180147.
2. S. Oliveira. "Evolution, ageing and speciation: Monte Carlo  simulations of biological systems", In: *Brazilian Journal of Physics* 34.3B (2004), pp. 1066-1076.
