# udemy-lec
* hot reloading run
```
  stack build --fast --file-watch --exec udemy-lec-exe
```
* package install
  * add package in package.yml
```
dependencies:
- base >= 4.7 && < 5
- optparse-applicative
```
