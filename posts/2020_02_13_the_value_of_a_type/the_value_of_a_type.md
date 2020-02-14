# The Value of a Type

> Testing can show the presence of bugs, but not their
> absence.
>
> Edsger W. Dijkstra, University of Texas 


> Programs are meant to be read by humans and only incidentally for computers to execute.
>
> Donald Knuth

## Complexity

The fundamental job of a programmer is to manage complexity. The ideal codebase is one that delivers maximum value with minimum complexity.

It's vital to define complexity as a cost. Complexity is the root cause of your customers seeing error pages, it's why changes take far longer than planned, it's why training juniors and new hires takes so long, it's why new startups run circles around the biggest tech cyberpunk megacorps around.

Easy example: DuckDuckGo. If you're not familiar, DDG is a privacy-focused search engine that's standing in sharp contrast to the surveillance-corporation practices Google have been pursuing.

According to some lazy DDG'ing, Alphabet had [98,771 employees](https://www.statista.com/statistics/273744/number-of-full-time-google-employees/) over the 2017-2018 year. According to some further DDG'ing, about [2-3,000](https://www.quora.com/How-many-engineers-does-Google-have-working-on-its-core-search-product?share=1) people work on the core search product. This number is pure speculation, but it lines up with my guess, having also worked at a large tech company in the bay area.

As of January 2020, DDG has [78 employees](https://duckduckgo.com/about).

Complexity matters.

Striking the right balance between features and complexity is a tough problem, and is a big part of why there are so many different programming languages in the marketplace. If you're writing a hard-real-time OS, Scheme or PHP are simply not fit for purpose. Similarly, if you're writing a web app, C or Assembly is not the right tool for your project.

But, finding that balance means that you're delivering the maximum value (features customers want) at minimal cost (complexity, measured by engineering time and effort). To that end, there are a few features that, especially in the long run, reduce complexity and help make the code base easier to change, more reliable, and more resistant to bitrot.

In my opinion, some of those features are:

* Compiled
* Statically typed
* Strongly typed
* No universal nulls (Optional/Maybe)
* Sum Types
* Discourages and avoids implicit casts (i.e., strictly typed)
* Immutable values by default
* A strong bias for pure functions

I like compiled languages because compilers check the entire codebase they're given. This means that all code paths get thoroughly examined and checked, which exposes a lot of gotchas. In particular, a compiler is very good at making sure that the definitions and uses of the functions and values in your program are all consistent, as well as syntactically correct.

I like type systems because they offer a way to verify things about values and functions in a very efficient way. Static type systems make it easy to state things you know or expect about your program, and have those be verified. Type annotations, like tests, do not come for free, and require careful thought, maintenance, and a degree of prudence and wisdom that comes with experience, but they pay significant dividends, especially as projects grow.

Strong typing means that you're describing your values in terms of what they represent (customer ID, UNIX time, username), rather than how they're stored in memory (int, float, string). Implicit casts can be convenient in small projects, but quickly become a nuisance as things turn into spooky action at a distance.

Sum types make reasoning about your code much more reasonable and explicit. If a function takes and returns a byte, you're working with something that, purely from a black box perspective, can do 256^256, different things. For reference, that is 32.317e615. Now imagine what functions that take multiple strings or integers can do. Furthermore, because sum types are both clearly defined and tightly constrained, it's easy to modify that definition, and have your compiler highlight all the places in your code where it's used, and refactor as appropriate.

Immutable values help in a number of ways. First, if a value is immutable, it can't be changed by some other function in your codebase. This becomes a lifesaver when you're writing parallel, concurrent code, as anyone that's tried to track down a race condition or an intermittent segfault can attest. Constants are predictable, and they remove places for bugs to hide. Similarly, pure functions remove an entire category of problems and bugs from your codebase.

Pure functions don't depend on global state, they don't do I/O, they don't crash on DST transitions or leap years. They're far easier to test, because you don't have to mock I/O functions or do any kind of sandbox setup or teardown -- you just feed values in and examine the results. This lack of setup also makes property checking feasible, something that brings enormous benefits. Then there's performance -- compilers can apply optimizations on pure code that border on spooky, and it's trivial to run these computations in parallel. Pure functions and immutable values are good. Use them!

The observant (or opinionated) among you will notice that I haven't said anything about tests. TDD devotees have often made the case that tests provide all the value needed to write and maintain reliable, robust code. I've also seen people on 'my' side of this discussion say that types are sufficient. I disagree with both points, and I'll explain why later on in this post, but the short version is: I like tests, I don't have a strong opinion on when you should write tests, just that they should be written. However, they don't replace what a compiler and a type system can give you.

## Why Compiled?

There's plenty of reason to dislike compilers. Like everything else in engineering, they aren't for free, and come with their own tradeoffs. In my opinion, the biggest cost is time. Compilers put a delay in the middle of the critical loop at the worst possible time: when your developers are actively engaged with the codebase, making changes -- when state and context have been [loaded into their mind](https://heeris.id.au/2013/this-is-why-you-shouldnt-interrupt-a-programmer/). Putting an interruption here, even a short one, means that engineers are very likely to switch to something else, destroying a lot of that context.

If the build takes a long time, [well...](https://www.xkcd.com/303/). Either way, that's time they aren't spending adding more features to beat the competition before your runway runs out.

And, like anything else, they can sometimes lead you down a primrose path. `g++` has some human-friendly error messages today, but that was certainly not always the case. C++ veterans can tell you horror stories about writing `map<string, list<int>>` instead of `map<string, list<int> >` (note the space between the `> >` in the second example) and getting multiple screen-fulls of error messages as C++ tried to fit a stream operator inside a type definition. Nowadays, either version will parse just fine and do the thing you expect. Even before the spec changed in C++'11, those screen-fulls of error messages were condensed down to a one-line error message that pointed at the problem. But this didn't happen overnight, and it doesn't come without a prodigious amount of effort and care.

As an aside, this is one place where Golang pulls ahead. The language is designed from the bottom-up to be fast to parse and compile, and have fairly human-friendly error messages. I have a fiery, burning contempt for Go, but. Credit where it's due.

In other facts that pain me, the aforementioned effort and care has _not_ been put into Haskell's error messages, and it really shows.

However, despite all of the above, I think that compilers are a source of complexity that's very well spent.

Consider the following Perl code.

```perl
sub main() {
    print("Step one");
    # This throws a warning and stops execution
    undefined_function_call();

    print("Step two, which never runs.");
}

sub uncalled_function() {
    # This does NOT throw a warning!
    undefined_function_call();
}


main();
```

When evaluated, this script runs, prints "Step one", and crashes with a warning at `undefined_function_call()`, saying (correctly) that the function is not defined.

However, if you correct this, the other `undefined_function_call()` is politely overlooked.

Compare that with this C++ code, which does not compile:

```cpp
void uncalled_function() {
  undefined_function_call();
}

int main() {
  return 0;
}
```

The 'moral' of these (admittedly trivial) examples is that C++ catches bugs that Perl (and many other scripting languages) swallow.

The value of this can be difficult to see if you're trying to prototype your way through something, but: bugs that are found in an engineer's terminal are bugs that don't cause customer support tickets, or (depending on the scale you're operating on), national headlines.

They're annoying, but they're a lot less annoying than getting paged. Pinky swear.

Now, some of you will have the (valid) complaint that these examples are ridiculously trivial, and that no engineer is capable of making a mistake like this.

I'll plead guilty to the first charge -- the point is that such things are possible. Given that it's possible, it's easy to imagine a reasonably sized repo where an equivalent arrangement could be created. In a situation like that, where either the developers are not completely familiar with every part of the repo (if that's even possible), your only line of defense is your build and test system.

> [When your repo gets to a certain size], everything that is syntatically legal, that the compiler will accept, will wind up in your codebase."
>
> John Carmack
[source](https://youtu.be/1PhArSujR_A?t=953)

Speaking as someone that has carried a pager for many years, my experience bears this out. I've seen some *really* silly things make it past both code review and CI/CD. Humans are bad at programming.

(By the way, the full video in that source link is well worth your time.)

## Why Types?

```python
def foo(a, b, c):
   # TODO
    pass
```

The function signature above doesn't tell us much. We know `foo` takes three arguments. Past that, we're in the dark. Setting aside the discussion about if this function does [I/O](https://hackage.haskell.org/package/acme-missiles-0.3/docs/Acme-Missiles.html) or not, let's just concentrate on calling the function correctly. This means putting the right values, in the correct order, at every callsite. As of right now, all we'd really have to rely on is looking at other callsites (and hoping those are correct) or reverse-engineering the function definition.

Furthermore, even if we add code, that doesn't constrain the argument types too much. Consider the following implementation:

```python
def foo(a, b, c):
  return a + b + c
  
x = foo(1, 2, 3)
y = foo("goodbye, ", "cruel ", "world")

print("Value of x: %d" % x)
print("Value of y: %s" % y)
```

This runs without the slightest comment. Even `mypy` doesn't complain about this. However, if we change the definition of foo to the following:

```python
def foo(a: float, b: float, d: float, c: float) -> float:
    return a + b + c
```

then `mypy` (well. `mypy --strict`) points out that the call at the definition of `y` has an invalid cast, as it should.

(side note, due to a quirk of implementation, python will happily run this without comment. The type annotations are only parsed by python, they aren't evaluated and compared. For that, you need to run `mypy`, and even then, you need `--strict` mode. sigh.)

What does this version have over the previous one? Why is a function explicitly taking and returning floats better than one that takes and returns `Any` ?

Answer: Even if the implementation isn't anything more particular than summing three numbers (or concatenating three strings), having a specific type like this helps communicate developer intent, along with comments, function and variable names, and so on.

## Why Strongly Typed?

What does 'strongly typed' mean?

Unsurprisingly, it's [complicated](https://en.wikipedia.org/wiki/Strong_and_weak_typing), and usage varies, but what I mean is this: say you have a function that takes three values, a Customer ID, a Product ID, and a timestamp, represented as a UNIX Epoch time. All of these are represented internally as integers. How would you write that out?

Well, one way would be:

```c
void bar(int cust_id, int prod_id, int epoch) {
  // TODO
  return;
}
```

There are a plethora of problems with this, though. First off, all of these values are, to the type system, completely identical, despite having very different semantic meanings. Among other things, this means that when we call this function, we're putting three values in a list at the callsite.

```c
void main() {
  int cust_id = 123;
  int prod_id = 456;
  int epoch_time = 1579968484;

  bar(cust_id, prod_id, epoch_time);
}
```

How easy is it to get the order wrong? Very.

How would you know if you did? Well, you'd have to either catch it in code review (which is another way of saying "don't mess up"), or see errors when you push the bad change to production, assuming it even generates one. Either way, the compiler is completely happy with this arrangement -- it doesn't know to tell you where you've made an error.

A further problem is that you're completely free to do some incoherent things to these values. If you have a DB row ID, the only thing that really matters is equality -- you have to be able to compare two IDs and see if they match. Beyond that, you shouldn't be able to do any mathematical operations on them, because that's simply nonsensical, it's not something that you'd ever actually want to do, and is almost certainly a logic error hiding in your code.

This is what I call "weakly typed". There is a type system, and it will catch things like putting a string where an integer was expected, but it also looks at things like Customer IDs, Product IDs, and timestamps as equivalent, because they happen share the same internal representation. Furthermore, because they're represented as an integer, the programming language cargo-cults all the operations you can do to integer values. You can sum together Customer IDs until the cows come home, despite that being completely insane.

So, what's the alternative? Let's look at an example.

```python
from typing import NewType

CustomerID = NewType("CustomerID", int)
ProductID = NewType("ProductID", int)
UnixTime = NewType("UnixTime", int)


def bar(cust_id: CustomerID, prod_id: ProductID, epoch_time: UnixTime) -> None:
    # TODO
    return
```

Here, the three values have a distinct type, which helps in a number of ways. First off, while we're just as likely to get the argument order wrong, we're far more likely to catch the problem before we ship that change to production, because `mypy` will catch the error.

Second off, newtypes solves the other problem we discussed.

```python
from typing import NewType

CustomerID = NewType("CustomerID", int)

def baz(cust_id: CustomerID):
    # many lines of code
    cust_id = cust_id + 1
    # many more lines
    return
```

`mypy` will tell us that `int` and `CustomerID` are incompatible types, which draws a big red circle around the thing we almost certainly did not want to do. One less place a bug can hide.

As you may have guessed, the previous two examples are what I call strong typing. You're describing what the values mean and what operations you can (or, importantly, cannot!) perform on them, rather than just how the bits are arranged in RAM.

## Why No Nulls?

[The billion dollar mistake](https://en.wikipedia.org/wiki/Null_pointer#History)! Let's get to it.

Consider the following C code.

```c
#include <stdio.h>
#include <stdlib.h>

void read_file(const char* filename) {
  FILE* fp = fopen(filename, "r");

  int c;
  while ((c = fgetc(fp)) != EOF) {
    putchar(c);
  }

  fclose(fp);
}

int main(void) {
  read_file("file.txt");

  return 0;
}
```

The C coders among you know there's a gotcha hiding in this code -- what if the file doesn't exist? `fopen()` returns a null pointer. If you pass a null pointer to `fgetc()`, your program will segfault.

C++ gets around this by throwing exceptions and expecting you to catch them. Exceptions have their uses, especially as a way to unroll the execution stack, but I don't think they're a catch-all for error handling.

Haskell handles this with `Maybe`, and I vastly prefer it. It's explicit in the return value of the function, so it isn't a surprise. It's very straightforward to handle, and doesn't require deeper code knowledge to know what exceptions to catch.

```cpp
#include <iostream>
#include <optional>

using std::cout;
using std::endl;
using std::optional;

optional<int> foo() {
  return {};
}

optional<int> bar() {
  return optional<int> {42};
}

int main() {
  cout << foo().has_value() << " value or: " << foo().value_or(-1) << endl;
  cout << bar().value() << endl;
  // segfaults from de-referencing a null pointer.
  cout << foo().value() << endl;

  return 0;
}
```

If you want to be able to pass an error message up, `Either` is an easy way to do so. C++ spells this `std::variant`, as shown below.

```cpp
#include <exception>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <variant>

using std::cout;
using std::domain_error;
using std::endl;
using std::exception;
using std::get;
using std::make_unique;
using std::unique_ptr;
using std::variant;

typedef unique_ptr<exception> upException;
typedef variant<upException, int> Either_Int;

Either_Int foo() { return 12; }

Either_Int bar(int) {

  return make_unique<domain_error>(domain_error("Make your functions pure!"));
}

int main() {
  auto f = foo();
  auto f_prime = get<int>(f);

  auto b = bar(f_prime);
  auto b_prime = get<upException>(b).get()->what();

  cout << b_prime << endl;
  return 0;
}
```

## Why Sum Types?

Let's look at the above C file-open example. You're writing a function that can result in one of three things. Ordinarily, your options are try to collapse two of the results together and use a Boolean, or explode it out into a larger type, like byte (type cardinality of 256), int (cardinality of 4294967295, if not more), or (worst of all), String (effectively infinite).

Furthermore, we want the ability to add or remove values from this sum type, and have the compiler catch places where either we aren't accounting for the new values, or are still referencing old ones. This keeps our code correct and robust, and makes errors easier to find.

So, we want a few things:

1. Clear names for the values in the type. Boolean blindness is a thing, and expanding the values to byte or int only makes the problem worse. Don't expect your value to clearly know what 0, 1, 2, 3, or -127 mean in the context of your functions.
1. Control over values are in the sum type. The only interface for values in the sum type should be the type definition. Don't let me substitute other values just because it's implicitly convertible to some other type.
1. Compiler errors when we remove values from the sum type, showing us where the type is used.
1. Compiler errors when we add values to the sum type, showing us where the type is used.

Haskell's way of doing this is via Sum Types. Other languages have been slowly adding the basic idea via enums. C++ carries a lot of baggage from C, so the regular enums are little more than constants defined on top of a base type. However, enum classes do what we want.

Look at this example:

```cpp
#include <iostream>

using std::cout;
using std::endl;
using std::ostream;
using std::string;

enum class MySumType {
  Success,
  NoChange,
  Failed,
};

// `deriving (Show)` in C++
string show_my_sum_type(const MySumType &e) {
  string result = "";
  switch (e) {
  case MySumType::Success:
    result = "Success";
    break;
  case MySumType::NoChange:
    result = "NoChange";
    break;
  case MySumType::Failed:
    result = "Failed";
    break;
  }

  return result;
}

ostream &operator<<(ostream &os, const MySumType &e) {
  os << show_my_sum_type(e);
  return os;
}

MySumType foo() { return MySumType::NoChange; }

int main() {
  cout << foo() << endl;

  return 0;
}
```

Now, suppose we delete `NoChange`, and immediately send it to the compiler. `g++` will point out that the references to `NoChange` in `show_my_sum_type` and `foo` are invalid, which is what we want.

Let's further suppose that we undo this change, and add a fourth value, and send that to `g++`. By default, it will compile, but the FP-literate among you will spot the fact that `show_my_sum_type` is now a partial function, because it isn't mapping all values of the input type to the output type. However, if you run `g++` with `-Wall` (like you should!), it will point out the issue I described above.

## Why Strict Typing?

Strict typing is a simple concept with a lot of implications -- it says that the language will not coerce types when they don't line up.

Sometimes, this can be a convenience. I have memories of dealing with Java in 2007/2008 or so, and the frustration of trying to add an int and a long, or a float and a double. This kind of bookkeeping trivia is immensely frustrating. However, it can also save your bacon. Consider the following code snippet.

```php
<?php

$mock_db = array();

function populate_db() {
    $ids = range(0,9);
    $result;
    foreach ($ids as $id) {
        $result[$id] = "1970/1/1";
    }
    return $result;
}


$mock_db = populate_db();

function set_marriage($id, $delta, $db) {
    $current_year = date("Y");
    $target_year = $current_year - $delta;
    $db[$id] = "$target_year/1/1";
    return $db;
}

?>
```

Suppose further that, scattered throughout the codebase, the `set_marriage()` function is called in a variety of ways.

```php
$mock_db = set_marriage(0, "18", $mock_db);         # 0
$mock_db = set_marriage(1, "1970", $mock_db);       # 1
$mock_db = set_marriage(2, 35, $mock_db);           # 2
$mock_db = set_marriage(3, "1987/11/14", $mock_db); # 3
```

First question: Which one is correct?

#0 is correct, but the wrong type. However, this will set the wedding anniversary 18 years in the past, which seems sane. PHP helpfully auto-casts this to the correct type (from string to int), so smooth sailing so far.

#1 is also the wrong type, but unless we're talking about a wedding around 50 CE, we've probably misunderstood the semantic meaning of the argument -- i.e., we passed in the year the wedding was supposed to happen, rather than how long ago the wedding happened. However, logic errors happen, and again, the implicit cast hasn't bitten us yet.

#2 is correct across the board. It's recorded as the correct type, and is passing in a delta value.

#3 is right out. Wrong type (string instead of int), and the wrong format, to boot. It's not representing an offset, or even a calendar year, but a specific date.

The worst part of this is how PHP happens to handle this situation. It will try to parse the string as a number (assuming base ten), so it will read out '1', '9', '8', and '7' without issue, getting int 1987. However, when it reaches the '/' delimiter in the date, it can't parse slash as a number. So it says "I must be done now!" and returns the value it's gotten so far.

In full fairness, it does emit a notice. Not an error, not even a warning... a notice. This exposes two problems with not just this call, but also calls 0 and 1 in the above example.

Problem one: There are many many many values in the String type that are completely invalid for this function, and we should constrain them as much as possible. 

Problem two: Converting from String to Int is a very error-prone process, and we should expose that as an explicit part of this algorithm. Even if we are getting this information from a string-y source (reading from stdin, for example), we should make the conversion process (i.e., parsing an integer out of said string) an explicit step, with things like "what base is it in?" and "how do we handle invalid values?"

All of these things reduce the cognitive overhead of engineers that must understand, debug, or extend this code later on. It makes bugs easier to spot, and problems easier to triage and diagnose.

All of these things put money back in your pocket.

## Why Immutable Values?

Compare these two examples:

```cpp
#include <iostream>
#include <vector>
#include <thread>
#include <random>

using std::cout;
using std::endl;
using std::random_device;
using std::thread;
using std::uniform_int_distribution;
using std::vector;

int global_mutable_state = 0;

int get_d20() {
  random_device generator;
  uniform_int_distribution<int> distribution(1, 20);
  int die_roll = distribution(generator);

  return die_roll;
}

void func(int) {
  auto die_roll = get_d20();

  if (die_roll >= 10 && global_mutable_state != 0) {
    throw;
  }
}

void monkey_wrench(int) {
  auto die_roll = get_d20();

  if (die_roll == 20) {
    cout << "Scored a critical hit, setting flag..." << endl;
    global_mutable_state = 1;
  }
}

int main() {
  vector<thread> thread_pool;
  for (int i = 1; i <= 20; i++) {
    auto die_roll = get_d20();

    if (die_roll == 5) {
      thread_pool.push_back(thread(&monkey_wrench, i));
    } else {
      thread_pool.push_back(thread(&func, i));
    }
  }


  for (auto &thread : thread_pool) {
    thread.join();
  }

  cout << "Success!" << endl;
  return 0;
}
```

Unsurprisingly, given the program has a function named `monkey_wrench`, this program has a race condition in it that will crash the program. A few things have to happen for this to happen, though.

1. `monkey_wrench` has to be scheduled, which requires a die roll of 1 in the thread pool loop.
1. `monkey_wrench` has to roll a 20, which sets `global_mutable_state` to 1.
1. _after this is done_, `func` must be scheduled.
1. Assuming it is scheduled, it must then roll a 10 or greater. Only then will an empty exception be thrown, crashing the program.

I made this extra-convoluted to simulate a real production system. Race conditions often require a number of things lining up just-so before things really start exploding. But, if you let a system run for long enough, perfect storms emerge. And, sure enough, if you compile this code and run it 1,000 times, you see quite a number of failures crop out.

1,000 runs on a single dev machine is one thing. Scale that up to even something modest, like 100,000 concurrent logins, and the possibility for finding perfect storms the likes of which Elder Gods dream of starts to be a *very* real thing.

The real issue here is that `monkey_wrench()` and `func()` could poke `global_mutable_state` willy-nilly. This kind of state leakage means that understanding and debugging your program gets exponentially harder, which costs you money -- often lots of it.

Concurrent code often needs to coordinate and communicate. However, those channels of communication *must* be strictly regemented. If you can poke whatever memory addresses you happen to have access to, trying to track down what function trashed your data structure at the wrong time, or what *other* function de-referenced a pointer that had already been freed, or even just how a configuration variable got mis-set can be an enormous headache. The problem with all concurrent code is that you're debugging along a third axis that is *beyond* non-obvious when you're just working with code: Time. It isn't just "what does the code say", but "what instructions got executed, in what order?"

Writing a concurrent application that is also safe means, in essence, writing your program in such a way that the Kernel scheduler can interrupt or arrange any atomic operation in your codebase, and the code will still perform correctly. You achieve this by two means:

1. Limit the ways one thread can affect another.
1. Ensure that the remaining, essential operations have adequate logic and locks to ensure correct behavior.

Doing things in this order makes this problem tractable. Being able to poke bits in everything you see leads to a factorial explosion of ways that you can, and will, foot-cannon yourself.

## Why Pure Functions?

Immutable values are well and good, but why are pure functions useful?

Consider the following C++ code:

```cpp
#include <iostream>
#include <cmath>
#include <tuple>

using std::cout;
using std::endl;
using std::get;
using std::make_tuple;
using std::string;
using std::tuple;

tuple<int, int, int> read_from_file(string) {
  return make_tuple(2, 3, 4);
}

void write_to_file(tuple<double, double>) {
  return;
}

void function_zero (string db_key) {
  auto three_tuple = read_from_file(db_key);

  auto a = get<0>(three_tuple);
  auto b = get<1>(three_tuple);
  auto c = get<2>(three_tuple);
  auto sqrt_value = sqrt(b * b + 4 * a * c);
  auto sum_value = (0 - b) + sqrt_value;
  auto diff_value = (0 - b) - sqrt_value;

  auto sum_result = sum_value / (2 * a);
  auto diff_result = diff_value / (2 * a);

  write_to_file(make_tuple(sum_result, diff_result));

              return;
}

int main() {
  auto key = "Example!";
  function_zero(key);

  return 0;

}
```

Let's ask a few questions about `function_zero()`.

* How easy would it be to run this concurrently?
  * Not very. You'd have to add some locking logic to make sure reads and writes to the file don't collide.
* How easy would this be to test?
  * Not very. You'd have to mock `read_from_file()` and `write_to_file()`, and in my experience, mocking is a hideous hack that's a walking nightmare to get right. Don't do it if you don't have to.
* Does this affect global state?
  * `g++` may not think so, but yes, it is -- that function is fully empowered to corrupt your file, log garbage, trash in-memory data structures, and generally ruin your day.
* Can race condition bugs hide here?
  * Absolutely.
  
None of these problems exist for pure functions.

Counter-example:

```cpp
#include <iostream>
#include <cmath>
#include <tuple>

using std::cout;
using std::endl;
using std::get;
using std::make_tuple;
using std::string;
using std::tuple;

tuple<int, int, int> read_from_file(string) {
  return make_tuple(2, 3, 4);
}

void write_to_file(tuple<double, double>) {
  return;
}

tuple<double, double> __attribute__((const)) function_zero (float a, float b, float c) {
  auto sqrt_value = sqrt(b * b + 4 * a * c);
  auto sum_value = (0 - b) + sqrt_value;
  auto diff_value = (0 - b) - sqrt_value;

  auto sum_result = sum_value / (2 * a);
  auto diff_result = diff_value / (2 * a);

  return make_tuple(sum_result, diff_result);
}

int main() {
  auto key = "Example!";
  auto three_tuple = read_from_file(key);

  auto a = get<0>(three_tuple);
  auto b = get<1>(three_tuple);
  auto c = get<2>(three_tuple);

  auto result = function_zero(a, b, c);

  write_to_file(result);

  return 0;
}
```

As a side note, the `__attribute__((const))` is largely aspirational. It will catch a few things, but it lets you get away with a **lot** more than a pure function in a language like Haskell would.

Separately, there's been a big push for [Microservices](https://en.wikipedia.org/wiki/Microservices) as a way to organize a production deployment. This has much to recommend it:

* Teams aren't responsible for enormous monoliths, so they can be smaller
* Team responsibilities are clearer, and it's easier to hold them accountable for the things they are responsible for
* Teams can vary architectures and language choices to suit the needs of their endpoints and the skills of the team
* Small services are easier to refactor and rewrite, which makes refactoring, or experimenting with different architectures and languages much cheaper than rewriting an entire monolith.

This is not to say that Microservices are free, or an unalloyed good (remember, everything has trade offs!), but, these things are worth pursuing.

Pure functions and immutable values offer similar benefits, just in a different context. Pure functions aren't caught in a spider's web of inter-dependencies on global state, which makes them very easy to move around, split off into a different microservice, or refactor and bugfix as needed.

## Types or Tests?

Make no mistake, I am very pro-tests. I've had plenty of cases where bugs were hiding in code that typechecked just fine, but did something wrong with the values. Maybe I was off by one, or I wasn't building my lists properly, or I was mis-using a key in the transformation I was doing. Regardless, having code you are completely, utterly, 1,000% sure is correct fail tests is an experience I feel every developer needs to have.

I don't have a strong opinion on **when** tests should be written, only that they *should be*, and they should be written and maintained as carefully as mainline production code.

[QuickCheck](https://www.stackage.org/nightly-2020-01-29/package/tasty-quickcheck-0.10.1) is a phenomenally powerful piece of software, doubly so when combined with Haskell's type system. Many other languages have stolen the basic idea, but it is inevitably not as powerful, because the type system in other languages isn't as powerful or as well exposed. There's even a [package](https://hackage.haskell.org/package/quickcheck-classes) to make sure your typeclass instances are lawful and sane, or another to test [monadic](https://www.stackage.org/haddock/nightly-2020-02-08/QuickCheck-2.13.2/Test-QuickCheck-Monadic.html) code.

However, just as types do not replace tests, tests do not replace types. I tend to look at compilers as a massive suite of tests that I get for very little effort.

Questions like

* Is my program syntactically valid?
* Do the definitions of the functions and values in my program line up?
  * Are all my function callsites correct?
  * Am I using their results correctly?
  * Do I catch places that can fail, and handle them correctly? (Maybe/Optional!)
* Are there any unused values or assignments in my code?
* Are there uncalled functions? (dead code)
* Am I ignoring a possible error or other failure?

On and on. Let's return to an earlier example:


```python
from typing import NewType

CustomerID = NewType("CustomerID", int)
ProductID = NewType("ProductID", int)
UnixTime = NewType("UnixTime", int)


def bar(cust_id: CustomerID, prod_id: ProductID, epoch_time: UnixTime) -> None:
    # TODO
    return
```

`bar` has a type signature built into it. It takes a `CustomerID`, a `ProductID`, and a `UnixTime`, in that order, and returns `None`. Having written that definition, I now get a test, for free, that every time `bar` is called, it is called with the appropriate values, and the result (`None`) is used (or, rather, **not** used) appropriately.

However, as mentioned in the section this snippet originally appeared in, this kind of resolution takes some extra effort. It requires some careful thought, but it pays dividends in the long run.

Keeping in that theme, however, tests are not an unalloyed good. Much like production code, effort must be expended to understand them -- which is a polite way of saying that when a developer is confronted with a failing test, they must then deduce what is in error, their code, the test, or both.

A good test makes the failure obvious. It does not merely enshrine old behavior, but describe an invariant property of the code under test. Tests, like code, should be pure whenever possible, and neither rely on nor affect global mutable state.

Intermittent or flaky tests are worse than no tests at all. All they do is train your developers to ignore failures in their CI/CD system, which leads to outages, which robs money from your account.

Making tests reliable and non-flaky takes a prodigious amount of effort. Plan for this.

## Conclusion, or, how does complexity scale?

If you studied data structures and algorithms, the first sorting algorithm you studied was Insertion Sort. This is because it's a very easy and straightforward algorithm to study and understand, and a great place to get started on studying space and time complexity.

For all of these virtues, you quickly realize that insertion sort is terrible. It's average complexity is n squared. More damning, it's average time complexity is also n squared.

However, it is usually the fastest if you're [sorting small sets](https://en.wikipedia.org/wiki/Insertion_sort#Relation_to_other_sorting_algorithms) -- lists of 50 elements or less. It's not a recursive algorithm, so you aren't paying the cost of function calls in your inner loop, and lists like that aren't long enough for the quadratic complexity to start to really hurt.

I think a similar phenomenon obtains for the language design choices I've spent a few thousand words arguing against. They start out being very lightweight and efficient. Developers can spike prototypes quickly, and get a lot of results for their efforts in a short time.

But, at some point, the codebase crosses an invisible threshold, and things get sluggish. Changes get harder to ship, reliability gets to be more of a problem, and eventually, you're not managing complexity so much as trying to keep your head above quicksand. Diving headfirst into static analysis can feel overwhelming and frustrating. But, not only does it make things more stable in the long run, it has it's own kind of fun attached to it.

In conclusion:

* Static analysis is your friend.
* Pin as many things about your code down as early as you can.
  * This is not to say write it in *stone*, but describe it in a way the computer can understand, so it can catch problems sooner rather than later.
* Automatically fixing problems is useful, but often leads to massive confusion if one is not careful.

All of this static analysis and type checking, especially in a language like Haskell where Monadic Algebra is standing between you and "Hello, World!" can feel like turning programming into standing in line at the DMV. I still remember spending years reading "Monads Are Burritos" tutorial blog posts before [Haskell Programming From First Principles](https://haskellbook.com/) came out, and getting exactly nowhere.

However, I've also carried a pager for a PHP app, and tried to maintain code in Bash, Perl, Python, legacy C++, Embedded C, Golang, PHP, Haskell, and more besides. Two observations have come from this.

One. Many of the code practices I've argued against can work (if one is careful) in small projects. Something where one engineer can fit the entire codebase or project in their head.

Two. Small projects turn into big ones.
