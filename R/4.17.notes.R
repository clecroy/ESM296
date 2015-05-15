#in class excercises

pwr = function(croll,m,g,v,a,p,cdrag){
  power = (croll*m*g*v)+((1/2)*a*p*cdrag)*v**3
  return(power)
}

pwrr= pwr(0.015,31752,9.8,seq(0,100),6,1.2,0.3)

plot(pwr(0.015,31752,9.8,seq(0,10000),6,1.2,0.3))

energytot = function(croll,m,g,v,a,p,cdrag,time){
  power = (croll*m*g*v)+((1/2)*a*p*cdrag)*v**3
  energy = power*time
  return(sum(energy))
}

speed = seq(0,10)
sum(pwrr*time)
time = c(1,2,3,4,2,2,5,3,2,1,3)

energytot(0.015,31752,9.8,speed,6,1.2,0.3,1)

energytotlist = function(croll,m,g,v,a,p,cdrag,time){
  power = (croll*m*g*v)+((1/2)*a*p*cdrag)*v**3
  energy = power*time
  return(list(total=sum(energy), avg=mean(energy), median=median(energy), table=cbind(power,time)))
}

energytotlist(0.015,31752,9.8,speed,6,1.2,0.3,time)


