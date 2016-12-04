

rm(list = ls())

#a - write powerf funciton
Powerf = function(){
  print(2^3)
}

Powerf()

#b - make power function agile
power2 = function(x,a){
  print(x^a)
}

power2(3,2)
power2(3,8)


#c - apply power2 on other things

power2(10,3)
power2(8,17)
power2(131,3)



#d - now return value using object rather than just printing

power3 = function(x,a){
  result = x^a
  return(result)
}

power3(9,2)

#e - plotting f(x) = x^2
x=1:10
y=power3(x,2)
plot(x,y)
plot(x,y,log = "x")
plot(x,y,log = "y")
plot(x,y,log = "xy")

#f - plotpower 
plotpower = function(x,a){
  y = x^a
  plot(x,y)
}

plotpower(1:10,3)













