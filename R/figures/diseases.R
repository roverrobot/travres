ebola=list(
    name="Ebola",
    #parameters from observation 
    # from WHO response team, NEJM 2014
    # the mean latent period
    Tlat = 11.4,
    # the mean serial interval
    Tserial = 15.3,
    # the mean infectious period
    Tinf = 5.0,
    # the mean time from admission to death
    Tdeath = 4.2,
    # the mean time from admission to discharge
    Tdischarge = 11.8,
    # case fatality 
    f = 0.708,
    # doubling times
    Tdouble.guinea=15.7,
    Tdouble.liberia=23.6,
    Tdouble.sierraleone=30.2,
    # the basic reproduction number (uncontrolled) in the arrival region
    R0=1.8
)

# from Donnolley et al Lancet 2003
sars=list(
    name="SARS",
    Tlat = 6.4,
    Tinf = 5,
    Tdeath=35.9,
    Tdischarge=23.5,
    f=0.132,
    R0=1.8
)

N = 36*10^6

t = 0:120
I0 = 10
ps = c(1, 0.1, 0.01)
t0s = c(0, 7, 14)
cim = 1/7
