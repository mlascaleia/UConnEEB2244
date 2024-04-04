# UConnEEB2244
Code that is used as the backend for certain discussion activity in UConn's General Ecology Course

Dear Future TAs,
I wrote this all and some (many) of my coding quirks are present. If you have a question about anything you can:
  1. Ask Miranda for my contact info and email me
  2. Create an issue in this repository
  3. Google me (Michael LaScaleia) and use the email you find there

Folder descriptions:

owlLifeTables:

I used code to generate 2 main things for this discussion. First is the balance of the probabilities. The idea is to have there be <50% chance of survival for the two years the owls are not breeding, and ~75% chance of survival each year they are breeding. Additionally, the fecundidty rate should be ~30% (.3 eggs per owl per year). I ran numbers to get a good balance of not making the game too complicated (not too many dice) and getting proper odds.
The second thing I used the code for is generating the owl cards. I used the csv in the folder to get the 1100 most popular girls' names in the U.S. then assign 600 of them randomly to owls. The other part was creating the 10 numbers that go on each card. I did this 500 times, then took the set that was the most random (highest entropy). All 500 sets were very random, so this was pretty unneccesary, but I wanted to be extra sure.
The way to translate the owls.csv to the actual cards is not in the folder. I used a mail merge to do that - see my note in the folder
