# Change Log for xalg

## 0.1.0.0

* First version. Released on an unsuspecting world.

### TO DO:



#### Parsing

* ~~Allow spaces between operators and subexpressions in parsers~~
* Make it so that `2x` is interpreted as `2*x`
* Probably parse `xy` to `x*y`. Then allow `x_999` to get plenty of vars.

#### Printing

* Better printing of Complex literals
* Distinguish somehow between parameters (assumed constant though unspecified) and actual variables in Env

#### Other

* ~~Use bound context in evaluation~~
* Abstract language facilities to enable multiple languages
* Symbolic derivation
* Enable saving/loading files
* Continue to use Fix or vanilla data type? Use TH to convert between the two? A type class?
