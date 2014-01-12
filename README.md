RSA
===

This program is based off the RSA algorithm I learned in my math 135 class.
I created a list of primes, two of which are randomly selected to help form the encryption and decryption keys.
However, in class we only encoded numbers, which isn't really all that fun.

So I made some functions that take a string and the encryption key and figure out the maximum string length that 
can be encoded (where each character will be converted into 3-digit ascii codes). This was important because there
is a maximum value of number that can be encoded using RSA, depending on how large the primes you're using.

Then the string is split up into a list of smaller strings, and each substring is converted into ascii code.
After this, there will be a list of integers which can each be converted using RSA.

You can convert any length string using this program.
When you run it, it will autmoatically create the public key (E, n) and the private key (d, n).
Make sure you refer to the public key with E instead of e (euler's constant) and write down the keys somewhere if you
encoded something and want to be able to decode it after you close the program, as a new set of keys are generated each 
time you run the program.
