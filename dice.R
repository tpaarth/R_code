roll = function() {
  die = 1:6
  diceTotal = sample(die, size = 1) + sample(die, size = 1)
  diceTotal
  
}
roll()

results = replicate(100, roll())
t1 = table(results)
t1
barplot(t1)
