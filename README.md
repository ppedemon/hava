## Hava: a toy JVM (this is vintage code!)

Hava is a toy JVM written in Haskell. I implemented this back in 2001 when 
writing my Licenciate (a degree roughly equivalent to a MSc) thesis. At 
some point I got bored, and took a two weeks "vacation" to write a Haskell 
JVM. Still not sure why I did this, but it was fun and I learned one or two
things about the innards of the (then) Sun's JVM.

The inspiration came from Jeroen Fokker's paper [Functional Specification of the JPEG algorithm](http://www.staff.science.uu.nl/~fokke101/article/jpeg/),
where he argues that the reference JPEG specification sucks and proposes a 
*the implemementation is the spec* approach, giving a very terse Haskell
implementation of the JPEG decompression algorithm. Somehow I thought it
would be nice to do the same with the operational semantics of the JVM,
and this is the result. Never got the nerve to try to publish this, though.

For no good reason, I decided to make this repository public. In the unlikely
event you end up reading this, keep in mind:

  1. The code is **old**. It's written for ghc 5.00.2 and any attempt to compile with a current 
     ghc will fail miserably. It should be trivial to port it to a modern ghc version, but I have
     better things to do.
  2. It was written when I was an undergraduate. So if you by any chance take a look to the code,
     please have mercy! 

