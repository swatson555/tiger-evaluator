# tiger-evaluator

Interpreter for the Tiger language from the book Modern Compiler Implementation in C by Andrew W. Appel (ISBN 0-521-60765-5).

```shell
tiger filename
tiger --help           - Prints this message.
tiger --repl           - Creates a tiger repl session.
tiger --print-ast      - Prints abstract syntax tree to stdout.
tiger --eval syntax    - Evaluates tiger syntax string.
tiger --file filename  - Evaluates a file.
```

## [The Ruby Hacking Guide: Evaluator](https://ruby-hacking-guide.github.io/evaluator.html)

An excerpt from the The Ruby Hacking Guide:

> We are not familiar with the word「評価器」“Hyo-ka-ki” (evaluator). Literally, it must be a “hyo-ka” (evaluating) “-ki” (device). Then, what is “hyo-ka”?
>
> “Hyo-ka” is the definitive translation of “evaluate”. However, if the premise is describing about programing languages, it can be considered as an error in translation. It’s hard to avoid that the word “hyo-ka” gives the impression of “whether it is good or bad”.
>
> “Evaluate” in the context of programing languages has nothing to do with “good or bad”, and its meaning is more close to “speculating” or “executing”. The origin of “evaluate” is a Latin word “ex+value+ate”. If I translate it directly, it is “turn it into a value”. This may be the simplest way to understand: to determine the value from an expression expressed in text.
