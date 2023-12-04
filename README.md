# Advent of code 2023

To setup: you'll need to go to advent of code, log in and get your input, then find your session cookie.
Instructions for how to do this can be found in https://www.reddit.com/r/adventofcode/comments/k4hd72/http_status_400_bad_request/

Create a file called Cookie.scala under utils/src/main/scala/advent/of/code/utils/ and paste the following code:
```scala
object Cookie {
  val value: String = "your-session-cookie"
}
```
Welcome to budget secrets management :)

## Why did I overengineer this to death?

This is my first Scala personal project. I've been meaning to learn the Typelevel stack for a while, and this seemed like a good
excuse to fight with http4s to get my puzzle inputs (and fight with gradle to set up my dependencies!)

The "test" directories are extremely optimistic given that I didn't get round to solving the first puzzle until the 4th, but
at least I've made a start this year!