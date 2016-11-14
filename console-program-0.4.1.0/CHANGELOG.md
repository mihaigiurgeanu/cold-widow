# Changelog

### 0.4.1.0

* Add the possibility to customize the prompt in interactive mode.
* Eliminate dependency on unix package on Win32.

## 0.4.0.0

* API change: the `Command` constructor now takes an extra argument
  (specifying whether the command can be abbreviated on the command line).
  The function `command` can be used instead of the old constructor.
* Better handling of exceptions in interactive mode.

### 0.3.2

* Generalise actions to allow any instance of MonadIO.
