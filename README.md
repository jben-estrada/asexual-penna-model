# Asexual Penna model

## Model description

An implementation of a biological aging model in Fortran 2008 as first introduced by T.J.P. Penna \[[1](#Reference)\]. The model we consider here however conforms more with the description of S. Oliveira \[[2](#Reference)\] than that of the original with some important modifications in the Verhulst factor.

In Oliveira's description, Verhulst factor is constant with age of the individuals throughout the run. In some implementations, the Verhulst factor is disabled for newborn individuals and active throughout their lives. Here, we let the Verhulst factor freely vary with the age of the individuals.

<p align="center"><img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/42af354dfb85d874f633eecffaf97314.svg" align=middle width=136.28076pt height=37.190999999999995pt/></p>

where <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/d6039daedba33e27f167e97f709c0d31.svg" align=middle width=16.719450000000002pt height=22.46574pt/> is the Verhulst factor at age <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/44bc9d542a92714cac84e01cbbb7fd61.svg" align=middle width=8.689230000000004pt height=14.155350000000013pt/>, <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/f72abbd7e7a1e545a1cb820fda5991f6.svg" align=middle width=18.898935pt height=14.155350000000013pt/> is the *Verhulst weight* at age <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/44bc9d542a92714cac84e01cbbb7fd61.svg" align=middle width=8.689230000000004pt height=14.155350000000013pt/>, <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/bc26136196e30407c1303ffbe073b500.svg" align=middle width=33.721545000000006pt height=24.65759999999998pt/> is the population size at time <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/4f4f4e395762a3af4575de74c019ebb5.svg" align=middle width=5.936155500000004pt height=20.222069999999988pt/>, and <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/8c9ead68ffc4c3846ae9e59a42071752.svg" align=middle width=39.457440000000005pt height=22.46574pt/> is the carrying capacity. The *Verhulst weight* is a real number in the interval [0, 1].

We note the unity term in <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/d6039daedba33e27f167e97f709c0d31.svg" align=middle width=16.719450000000002pt height=22.46574pt/>. Oliveira defined the age-independent Verhulst factor to be <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/eb4be7fc042794cc5b9f32ccf806af85.svg" align=middle width=115.64454pt height=24.65759999999998pt/>. In his/her implementation of the model, individuals die if some real number <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/1eeaa1a03a97e12954f0c28afb46592a.svg" align=middle width=64.49388pt height=24.65759999999998pt/> is less than the <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/a9a3a4a202d80326bda413b5562d5cd1.svg" align=middle width=13.242075000000003pt height=22.46574pt/>. In our case, they die if <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/89f2e0d2d24bcf44db73aab8fc03252c.svg" align=middle width=7.873024500000003pt height=14.155350000000013pt/> is greater than <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/a9a3a4a202d80326bda413b5562d5cd1.svg" align=middle width=13.242075000000003pt height=22.46574pt/>. As such, both definitions of the Verhulst factor are equivalent given that the implementation of death corresponds with whatever definition we use. Here, we consider the following definition for the age-independent Verhulst factor from which the age-dependent one is derived.

<p align="center"><img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/df57f97687bb8061d56ce9f217bd366b.svg" align=middle width=105.72193499999999pt height=37.190999999999995pt/></p>

Now looking at <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/f72abbd7e7a1e545a1cb820fda5991f6.svg" align=middle width=18.898935pt height=14.155350000000013pt/> (which we have called *Verhulst weight*), if we let <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/69beda82e62145324b88a45f5838b7c4.svg" align=middle width=49.85771999999999pt height=21.18732pt/>, <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/d6039daedba33e27f167e97f709c0d31.svg" align=middle width=16.719450000000002pt height=22.46574pt/> is equal to 1 at age <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/44bc9d542a92714cac84e01cbbb7fd61.svg" align=middle width=8.689230000000004pt height=14.155350000000013pt/>. Since <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/89f2e0d2d24bcf44db73aab8fc03252c.svg" align=middle width=7.873024500000003pt height=14.155350000000013pt/> cannot be greater than 1, individuals at this age cannot randomly die due to the Verhulst factor. On the other hand, if <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/f19dc8cf1e2e99d0d17df91a06cd7fc0.svg" align=middle width=49.85771999999999pt height=21.18732pt/>, <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/d6039daedba33e27f167e97f709c0d31.svg" align=middle width=16.719450000000002pt height=22.46574pt/> becomes the original Verhulst factor. It is up to us what values for <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/f72abbd7e7a1e545a1cb820fda5991f6.svg" align=middle width=18.898935pt height=14.155350000000013pt/> we will use.

Varying <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/d6039daedba33e27f167e97f709c0d31.svg" align=middle width=16.719450000000002pt height=22.46574pt/> can be useful in, say, modelling *survivability* of some species where younger ones are more likely to die than the older ones due to environmental factors. An example utilizing this feature is by having <img src="https://github.com/jben-estrada/asexual-penna-model/blob/master/svgs/f72abbd7e7a1e545a1cb820fda5991f6.svg" align=middle width=18.898935pt height=14.155350000000013pt/> go down with the age of the individuals to make them more resilient to external pressures.

## Reference

1. Thadeu Penna. "A Bit-String Model for Biological Aging". In: *Journal of Statistical Physics* 78 (Mar. 1995). DOI: 10.1007/BF02180147.
2. S. Oliveira. "Evolution, ageing and speciation: Monte Carlo  simulations of biological systems", In: *Brazilian Journal of Physics* 34.3B (2004), pp. 1066-1076.