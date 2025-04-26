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

Running the executable in `bin/` in the project directory runs the Penna model program with the default parameters indicated in `model.cfg` also in the `bin/` directory.

```bash
cd /path/to/penna-code/ 
## Assuming you are not yet in the program directory.

cd bin/
./penna
# Run the Penna model with default options.
```

### I. Specifying model parameters

The parameters related to the Penna model can be specified through command-line options or a custom configuration file.

For example,
```bash
./penna -B 1
# Run the Penna model with a birth rate (B) of 1

./penna -T 20 -r 16 -R 16
# Run with a mutation threshold (T) of 20, and min and max reproduction ages
# (r and R, respectively) of 16

./penna -f ./my-custom-parameters.txt
# Run using the parameters listed in the text file `my-custom-parameters.txt`.
```

Do note that the parameter values in the custom parameter file effectively becomes the new default parameters. As such, it must contain ALL modifiable parameters which are indicated in `model.cfg` in `bin/` of the program directory.

Options related to Penna model simulations are summarized in the following table:

| Model parameter name    | Command-line option                          |
| ----------------------- | -------------------------------------------- | 
| Birth  rate             | `-B` *\<int>* &ensp; `--birth-rate=`*\<int>* |
| Mutation rate           | `-M` *\<int>* &ensp; `--mttn-rate=`*\<int>*  |
| Mutation threshold      | `-T` *\<int>* &ensp; `--mttn-lim=`*\<int>*   |
| Min. reproduction age   | `-r` *\<int>* &ensp; `--r-age-min=`*\<int>*  |
| Max. reproduction age   | `-R` *\<int>* &ensp; `--r-age-max=`*\<int>*  |
| Initial population size | `-p` *\<int>* &ensp; `--pop-size=`*\<int>*   |
| Max. population size    | `-K` *\<int>* &ensp; `--pop-cap=`*\<int>*    |
| Init. mutations per individual<sup>1</sup>       | `-m` *\<int>* &ensp;  `--init-mttn=`*\<int>*     |
| Time-dependent model param.<sup>2</sup>          | `-P` *\<char>* &ensp; `--tmdp-param=`*\<char>*   |
| Time-dependent model param. interval<sup>2</sup> | `-Q` *\<int>* &ensp;  `--tmdp-param-dt=`*\<int>* |

*Table notes:*
- *\<int>* stands for an integer, *\<float>* for a floating point number, *\<char>* for a *single* character, and *\<str>* for a string or a sequence of characters.

- <sup>1</sup> - This is the number of random mutations each individual has at the beginning of a Penna model simulation. If this value is negative, then the number of random mutation is randomized for each individual.

- <sup>2</sup> - The choice of parameters for `-P` or `--tmdp-param` is specified by a character value. They are shown in the table below.

    | Name                 | Char value |
    | -------------------- | :--------: |
    | Birth rate           | `b`        |
    | Mutation rate        | `m`        |
    | Mutation threshold   | `t`        |
    | Reproduction age*    | `r`        |
    | None                 | `x`        |

    Note that "reproduction age" here refers to both minimum and maximum reproduction ages, such that their values increase at the same rate.

    The choices are rather limited due to programmatic reasons among others. The change in value is always increasing. And the rate of change is specified by the time interval at which the parameter increments. The default value is *none* (`x`) as specified in `model.cfg`.


### II. Outputing data

#### A. Kinds of data

Running the Penna model program like those above does not output any data just yet. To specify what kind of data to output, you might do some like this:

```bash
./penna -d pasb
# ... Or with the longer form
./penna --record-data=pasb
```

where the argument `pasb` is the sequence of char values each representing a kind of data to be recorded. The table below shows the data each of the characters represent.

| Data                                           | Char value |
| ---------------------------------------------- | :--------: |
| Population size over time                      | `p`        |
| Age distribution<sup>3</sup>                   | `a`        |
| Number of deaths over time                     | `d`        |
| Rényi entropy of genome over time<sup>4</sup>  | `s`        |
| Bad gene distribution over time<sup>3</sup>    | `b`        |
| Number of unique genomes over time             | `c`        |
| The program's average elapsed time             | `t`        |
| Record nothing (default)                       | `x`        |

*Table notes:*

* The order of the character values does not matter. And the default value is *record nothing* (`x`) as specified in `model.cfg`.

- <sup>3</sup> - The default range of ages for age distribution and bad gene distribution is the final 300 time steps, as specified in `model.cfg`. This can be modified in `model.cfg`, in the custom configuration file supplied to the program if present, or through the command-line options `-a` or `--age-dstrb-time`.

- <sup>4</sup> - Rényi entropy is a generalization of different entropy measures. To specify which specific entropy to be used for measuring genetic diversity, you need the corresponding *order* of the Rényi entropy. This value can be any float value. In this program, a value of `NaN`, `+inf` or `-inf` (note: "infinity" in the context of floating point arithmetic) defaults to the *normalized Shannon entropy*. By default as specified in `model.cfg`, the value is `NaN`. The Rényi entropy order can also be specified through the command-line option `-O` or `--ent-order`.

#### B. Running the Penna model

We might want to do multiple runs for the same set of parameters and initial conditions. To do so, you can specify the number of runs with the command-line option `-s` or `--sample-size`. This defaults to `1` as specified in `model.cfg`.

Each run will have different random number generator seed. For the first run, the seed is either the default value (`1` in `model.cfg`) or a user-specified value. Then for every new run, the new seed is obtained by incrementing the previous seed value. The starting RNG seed can be supplied through the command-line option `-S` or `--rng-seed`.

As for the random number generator itself, so far there are two available to the users: one provided by the compiler (`gfortran` or `ifx`) and 32-bit Mersenne twister. This is specified either through the command-line option `-g` or `--rng-choice`, or a configuration file. The RNG choices are represented as integers:

- Compiler-intrinsic RNG: `0`
- 32-bit Mersenne Twister: `1`

To illustrate what we know so far, here is what you might use:
```bash
## Assuming you are in the bin/ directory of the program.

./penna -f ./my-custom-parameters.txt -d psba -s 100 -S 42 -g 1
# Run the Penna model 100 times with 42 as the starting RNG seed while recording the data corresponding to p, s, b and a.
# Mersenne Twister is used for the RNG.
```

#### C. Formatting the output file

You can specify the path of the output file with `-o` or `--out`. Formatting the path and the output file name is also available. This is recommended for more complex cases such as the one above where you have multiple runs and multiple data sets. The formatting goes as follows:

1. `%[N]n` - Run number, e.g. the 6th run.
2. `%[N]f` - Character value of the data set, e.g. `p`, `s`, `b` and `a`

Note that `N` is the number of left padding. It is optional and can be omitted, hence its inclusion as being inside square brackets. In practice, you can do something like this: `run=%2n_data=%f.csv` which produces file names such as `run=09_data=p.csv` which should contain the data for population size over time (`p`) from the 9th run.

The format of the data itself can also be specified with the `-F` or `--out-format` command-line option. The available options are as follows:

1. `read` - Human-readable format.
2. `csv`  - "Comma-seperated values" format.
3. `bin`  - Binary format.

The `read` and `csv` formats are more human friendly in that the data points are represented as readable texts. Additionaly with `csv`, a simple script in, say Python, can be written to automatically parse CSV files and extract data for processing.
The `bin` option is more complex, having to extract and decode raw binary data. The advantage of this is its smaller size. You can find more information about this in the help message by running `./penna -h`.

To illustrate everything discussed here, here is an example:
```bash
./penna -f ./my-custom-parameters.txt -d psba -s 100 -S 42 -g 1 -o './data/run=%3n_data=%f.csv' -F csv
# Run the Penna model 100 times with 42 as the starting RNG seed while recording the data corresponding to p, s, b and a. Mersenne Twister is used for the RNG
# The output data is saved as CSV files in the directory `data`, all sorted by run number and the kind of data.
```

Listed below is a summary of command-line options relevant to data recording.

| Description             | Command-line options                          |
| ----------------------- | --------------------------------------------- |
| Record data             | `-d` *\<str>* &ensp; `--record-data=`*\<str>* |
| Sample size             | `-s` *\<int>* &ensp; `--sample-size=`*\<int>* |
| RNG seed                | `-S` *\<int>* &ensp; `--rng-seed=`*\<int>*    |
| Path to the output file | `-o` *\<str>* &ensp; `--out=`*\<str>*         |
| Data format             | `-F` *\<str>* &ensp; `--out-format=`*\<str>*  |
| Distribution age range from the final time step | `-a` *\<int>* &ensp; `--age-dstrb-time=`*\<int>* |
| Renyi entropy order                             | `-O` *\<float>* &ensp; `--ent-order=`*\<float>*  |

### III. More command-line options

Listed below are miscellaneous command-line options:

| Description           | Command-line option     |
| --------------------- | ----------------------- |
| No logging            | `-q` &ensp; `--quiet`   |
| Verbose logging       | `-v` &ensp; `--verbose` |
| Show program version  | `-V` &ensp; `--version` |
| Show the help message | `-h` &ensp; `--help`    |


For more information, run the Penna model program with the `--help` option.

```bash
./penna --help
# or...
./penna -h
```

Go to [Top](#asexual-penna-model).

## Reference

1. Thadeu Penna. "A Bit-String Model for Biological Aging". In: *Journal of Statistical Physics* 78 (Mar. 1995). DOI: 10.1007/BF02180147.
2. S. Oliveira. "Evolution, ageing and speciation: Monte Carlo  simulations of biological systems", In: *Brazilian Journal of Physics* 34.3B (2004), pp. 1066-1076.
