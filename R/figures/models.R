require(deSolve)

model.exp <- function(t, y, parms) {
    with(c(as.list(y), parms), {
        c = if (t >= t0) cim*p else cim
        beta = R/Tinf
        dEim = c - Eim*2/Tlat        
        dE = beta*I - E/Tlat
        dI = E/Tlat + Eim*2/Tlat- I/Tinf
        dHd = f*I/Tinf - Hd/Tdeath
        dHl = (1-f)*I/Tinf -Hl/Tdischarge
        dD = Hd/Tdeath
        list(c(dEim, dE, dI, dHd, dHl, dD))
    })
}

model.SEIR <- function(t, y, parms) {
    with(c(as.list(y), parms), {
        c = if (t >= t0) cim*p else cim
        beta = R/Tinf
        dS = -beta*S*I/N
        dEim = c - Eim*2/Tlat
        dE = -dS - E/Tlat
        dI = E/Tlat + Eim*2/Tlat- I/Tinf
        dHd = f*I/Tinf - Hd/Tdeath
        dHl = (1-f)*I/Tinf -Hl/Tdischarge
        dD = Hd/Tdeath
        list(c(dS, dEim, dE, dI, dHd, dHl, dD))
    })
}

# the solution to the SEIR model for time t, with:
#   travel restriction implemented at time t0
#   a leaking probability p, 
#   the reproduction number R in the arrival region
#   the number of initially infectious individuals I0
#   the daily import rate cim
#   and the disease parameters
#   linear=TRUE if intended to use the linear model
solution <- function(t, t0, p, R, I0, cim, disease, linear=TRUE) {
    parms = c(disease, list(cim=cim, p=p, t0=t0, R=R))
    X0 = c(Eim=0, E=0, I=I0, Hd=0, Hl=0, D=0)
    if (linear) {
        model = model.exp
    } else {
        model = model.SEIR
        X0 = c(S=N-I0, X0)
    }
    ode(y=X0, times=t, func=model, parms=parms, method="ode45")
}
