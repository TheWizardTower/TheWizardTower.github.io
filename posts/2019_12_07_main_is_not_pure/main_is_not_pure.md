Many thanks to [Michael Snoyman](http://snoyman.com) for both helping to inspire, and then providing comments and feedback. Any remaining defects are my responsibility!

One of Haskell's defining feature is the fact that it tracks effects in its type system. As such, much ado is made around "pure functions" -- functions that do no I/O -- and functions that do perform I/O. However, if you stick around long enough, you'll hear something quite curious -- people will describe `main` as a pure function.

At first glance, this seems only true in some specific examples. Say:

```haskell 
main :: IO Int
main = return $ 4 + 5 -- No IO is actually done, even though we are in the IO Monad.
```

The above example is essentially `/usr/bin/true`, the only effect it has is changing the status variable (`$?` in bash-likes, `$status` in fish) to 0, overwriting the previous value.

(note that the exit code is *not* 4 + 5, as a C/C++ dev might assume. You have to set the exit code explicitly, otherwise it defaults to zero.)

Some people add a bit of nuance, and say that `main` is a value that describes I/O actions.

(It isn't a function, functions take arguments).


```haskell
main :: IO ()
main = print "Hello, World!"
```

This is true, as far as it goes, but I'm not fond of it. I'll get into why later.

Let's look at a few other examples and see if we can find some clarity. Consider the following code:

```javascript
let myError = new Error("Something went really wrong.");
myError.code = -127;
```

Does `myError` cause your JS program to crash? No, not in and of itself, it's merely a value, one we manipulate and later use. The user doesn't see it until you do this:

```javascript 
throw myError;
```

C++ works in a similar way.

```cpp
#include <iostream>
#include <exception>

struct MyException : public std::exception {
  const char * what () const throw () {
    return "Example C++ Exception!";
      }
};

int main() {
  std::cout << "The end is near!" << std::endl;
  MyException exception = MyException();
  std::cout << "But not here yet." << std::endl;
  throw exception;
  std::cout << "Lamentations, our process is cut short." << std::endl;

  return 0;
}
```

Let's look at awaitable functions. Hacklang has great syntax for this.

```hack
public async function f(): Awaitable<int> {
    // killer startup business logic here
    return 5;
}
```

Does f() represent an Int, or a computation, still to be performed, which will ultimately result in an Int? Technically, it's the latter. However, Hacklang gives us a very nice async/await syntax, which lets us sidestep this distinction in the general case:

```hack
public async function differentBusinessLogic(): Awaitable<void> {
    f_awaitable = f();
    f_value = await f_awaitable;
    return f_value + 100;
}
```

Both examples are illustrating the difference between a value immediately performing an action (declaring an exception vs throwing it, getting a promise/awaitable from a function vs the final result) or merely describing it. That difference being: you can mutate it.

Really this is the "big thing" that monads allow us to do. We can describe an action, manipulate it, mutate it, wrap it in timeout logic, retries, all before any IO happens.

```haskell
import           Control.Concurrent (threadDelay)
import           Control.Retry      (retryPolicyDefault, retrying)
import           Data.Maybe         (isNothing)
import           System.Random      (randomRIO)
import           System.Timeout     (timeout)

myLongFunc :: IO Int
myLongFunc = do
  -- roll 1d10
  num <- randomRIO (1, 10) :: IO Integer
  case num of
    10        -> threadDelay (2  * 10 ^ 6)    -- sometimes the DB answers really fast!
    otherwise -> threadDelay (10 * 10 ^ 6)  -- ...but mostly not so much.
  return 100

myActualAction :: Int -> IO ()
myActualAction = print -- log it, or something.

myComposedAction :: IO ()
myComposedAction = print "Trying!" >> myLongFunc >>= myActualAction

main :: IO ()
main = do
    _ <- retrying
      retryPolicyDefault
      (const $ return . isNothing)
      (\_ -> timeout (5 * 10 ^ 6) myComposedAction)
    return ()
```

The common thread with these examples is they all demonstrate manipulating values that later cause effects, 

I dislike referring to main (or impure functions in general) as pure functions, or "pure values that describe effects" because it makes things *less* clear than they were at the start, and obscures a tremendously valuable distinction.

However, we *can* still treat it as a value, one we can manipulate, pass to other functions, or even control how it runs. We can add timeouts, retries, we can short-circuit it if previous functions failed, and all the other way-cool stuff principled monads in a functional paradigm enable.

In *this* sense, looking at IO actions as values does useful work, and exposes something special about Haskell-like languages that is, at best, significantly more difficult to pull off in traditional languages like C++ or Java.

In my opinion, categorically saying that all IO functions in Haskell are merely pure values that describe effects is either 
  * Categorically false (the I/O is right there in the type signature!)
  * Vacuously true.

I say Vacuously true because, by the same token, all C++ binaries are merely values (bits on a disk!) that describe I/O actions. 

However, there is a tremendously useful distinction to be made in pointing out that, in a functional language, functions are ordinary values, and as such, can be manipulated and mucked with to a much greater extent than one would expect, coming from imperative/OO languages.

Monkey-wrench time. Consider the following snippets.


```cpp
#include <chrono>
#include <iostream>
#include <random>
#include <thread>

int myLongFunc() {
  std::random_device generator;
  std::uniform_int_distribution<int> distribution(1,10);
  int rand_int = distribution(generator);
  std::cout << "Random int result: " << rand_int << std::endl;

  if (rand_int == 10) {
    std::cout << "We're in luck, this will be fast." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(2));
  } else {
    std::cout << "We're less lucky, this will be slow." << std::endl;
    std::this_thread::sleep_for(std::chrono::seconds(10));
  }

  return 100;
}

int main() {
  std::cout << "Hello world, let's talk to a DB!" << std::endl;

  std::cout << myLongFunc() << std::endl;

  return 0;
}
```

```shell
#!/bin/bash

g++ 07_cpp_timeout.cpp -o example

for ii in $(seq 1 15); do
  timeout 5s ./example
  if [[ $? -eq 0 ]];
  then
    break;
  fi
done
```

What's the difference between this code and the Haskell example from before? Is this an example of using a C++ binary as a pure value?

Am I just trolling you? (maybe a little, yes)

I think it's a great example of patterns crossing paradigm boundaries. Retries and timeouts aren't anything new, Haskell doesn't have a monopoly on this market. It's just that it's much easier to set them up without either reaching for your OS's scripting tool (bash, in this case) or being intimately, obnoxiously familiar with your language's runtime, and exactly how and when statements are executed.

(Yes, Haskell certainly has places where knowledge of the Elder Things is essential. No language is perfect, and there ain't no such thing as a free lunch.)
