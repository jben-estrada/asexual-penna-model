# Asexual Penna model

## Model description

An implementation of a biological aging model in Fortran 2008 as first introduced by T.J.P. Penna \[[1](#Reference)\]. The model we consider here however conforms more with the description of S. Oliveira \[[2](#Reference)\] than that of the original with some important modifications in the Verhulst factor.

In Oliveira's description, Verhulst factor is constant with age of the individuals throughout the run. In some implementations, the Verhulst factor is disabled for newborn individuals and active throughout their lives. Here, we let the Verhulst factor freely vary with the age of the individuals.

$$V_a = 1 - w_a\frac{N(t)}{N_{max}},$$

where $V_a$ is the Verhulst factor at age $a$, $w_a$ is the *Verhulst weight* at age $a$, $N(t)$ is the population size at time $t$, and $N_{max}$ is the carrying capacity. The *Verhulst weight* is a real number in the interval [0, 1].

We note the unity term in $V_a$. Oliveira defined the age-independent Verhulst factor to be $V = N(t)/N_{max}$. In their implementation of the model, individuals die if some real number $r \in (0, 1)$ is less than the $V$. In our case, they die if $r$ is greater than $V$. As such, both definitions of the Verhulst factor are equivalent given that the implementation of death corresponds with whatever definition we use. Here, we consider the following definition for the age-independent Verhulst factor from which the age-dependent one is derived.

$$V = 1 - \frac{N(t)}{N_{max}}$$

Now looking at $w_a$ (which we have called *Verhulst weight*), if we let $w_a = 0$, $V_a$ is equal to 1 at age $a$. Since $r$ cannot be greater than 1, individuals at this age cannot randomly die due to the Verhulst factor. On the other hand, if $w_a = 1$, $V_a$ becomes the original Verhulst factor.

Varying the Verhulst factor can be useful in, say, modelling *survivability*. By letting $w_a$ increase with age, individuals are more likely to survive as they grow older. Conversely, young individuals are more likely to die due to them being more fragile.

## Prerequisite
- Python <=3.12
- A Fortran 2008 compliant compiler
    - e.g. `Intel Fortran Compiler 2024.2.0` and `GFortran 12`

## Installation
WIP

## Reference

1. Thadeu Penna. "A Bit-String Model for Biological Aging". In: *Journal of Statistical Physics* 78 (Mar. 1995). DOI: 10.1007/BF02180147.
2. S. Oliveira. "Evolution, ageing and speciation: Monte Carlo  simulations of biological systems", In: *Brazilian Journal of Physics* 34.3B (2004), pp. 1066-1076.
