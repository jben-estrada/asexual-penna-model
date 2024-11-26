# Asexual Penna model

**Author**:
John Benedick A. Estrada (jaestrada2@up.edu.ph)

## Model description

An implementation of a biological aging model in Fortran 2008 as first introduced by T.J.P. Penna \[[1, 2](#reference)\]. This program follows the original model with some important changes.

#### Age-dependent Verhulst factor

In the original description \[[1](#reference)\], the Verhulst factor is constant with the age of the individuals. In other variants, the Verhulst factor is disabled for newborn individuals and active throughout their lives. Here, we let the Verhulst factor freely vary with the age of the individuals.

$$V_a = 1 - w_a\frac{N(t)}{N_{max}},$$

where $V_a$ is the Verhulst factor at age $a$, $w_a$ is the *Verhulst weight* at age $a$, $N(t)$ is the population size at time $t$, and $N_{max}$ is the carrying capacity. The *Verhulst weight* is a real number in the interval [0, 1].

We note the unity term in $V_a$. Oliveira \[[2](#reference)\] defined the age-independent Verhulst factor to be $V = N(t)/N_{max}$. In their implementation of the model, individuals die if some real number $r \in (0, 1)$ is less than the $V$. In our case, they die if $r$ is greater than $V$. As such, both definitions of the Verhulst factor are equivalent given that the implementation of death corresponds with whatever definition we use. Here, we consider the following definition for the age-independent Verhulst factor from which the age-dependent one is derived.

$$V = 1 - \frac{N(t)}{N_{max}}$$

Now looking at $w_a$ (which we call as the *Verhulst weight*), if we let $w_a = 0$, $V_a$ is equal to 1 at age $a$. Since $r$ cannot be greater than 1, individuals at this age cannot randomly die due to the Verhulst factor. On the other hand, if $w_a = 1$, $V_a$ becomes the original Verhulst factor.

Varying the Verhulst factor can be useful in, say, modelling *survivability*. By letting $w_a$ increase with age, individuals are more likely to survive as they grow older. Conversely, young individuals are more likely to die due to being fragile and less adept in their environment.

#### Genome initialization in the initial population

The number of deleterious random mutations per genome in the initial population can be specified. It is also possible to randomize this number per genome to completely randomize the initialization.

#### Time-dependent model parameters

In this implementation of the Penna model, it is also possible to specify one of the select model parameters and make them vary with time. At the moment, only birth rate $B$, mutation rate $M$, minimum and maximum reproduction ages $R$ and $R_{max}$, and mutation threshold $T$ can be chosen. Note that for $R$ and $R_{max}$, both parameters vary at the same rate. The rate of change is also positive, incrementing by 1 every $\Delta t$ time steps where $\Delta t$ is specified by the user.

Go to [Top](#asexual-penna-model).
## Installation
### Prerequisite
- Python <=3.11
- A Fortran 2008 compliant compiler
    - e.g. `Intel Fortran Compiler 2024.2.0` and `GFortran 13`

### Installing the required Python libraries
This project requires the Python library `FoBiS.py` to build the Fortran code.

Currently, `FoBiS.py` can be installed using `pip`:
```bash
pip install FoBiS.py
```

To my knowledge, it is also available on Anaconda but only the older versions. Alternatively you can install `FoBiS.py` manually. Refer to the [Wiki page](https://github.com/szaghi/FoBiS/wiki/Manual-Installation) of this project.

### Building the Penna model program

Once you have downloaded or cloned the project, run the build script `build.py` in the project directory.
It has several options to build the project but for most cases, you want to do a release build.
To do so, run the following commands:

```bash
cd asexual-penna-model     # Go to the project directory
python build.py --build-type release --compiler-type gnu --clean
```

Note that this assumes that you use `gfortran`. To change it into Intel Fortran Compiler (`ifx`), replace `gnu` with `intel`:
```bash
python build.py ... --compiler-type intel ...
```

The output executable `penna` can be found in `bin/` in the project directory.
For information about the other build types, run the script with the `--help` option.
```bash
python build.py --help
```

#### NOTE :
So far, the build script only allows `gfortran` and `ifx` compilers.

Go to [Top](#asexual-penna-model).

## Usage

Running the executable in the `bin/` directory runs the Penna model program with the default parameters indicated in `bin/model.cfg`.

For more thorough and complete information, run the Penna model program with the `--help` option.

```bash
bin/penna --help
```

*WIP*

Go to [Top](#asexual-penna-model).

## Reference

1. Thadeu Penna. "A Bit-String Model for Biological Aging". In: *Journal of Statistical Physics* 78 (Mar. 1995). DOI: 10.1007/BF02180147.
2. S. Oliveira. "Evolution, ageing and speciation: Monte Carlo  simulations of biological systems", In: *Brazilian Journal of Physics* 34.3B (2004), pp. 1066-1076.
