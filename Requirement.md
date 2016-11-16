# Requirement List

## Required syntaxes

### Required statements

1. Function definition statement

### Required expressions

1. Lambda Expression
1. If Else Expression
1. Let In Expression

## Examples

### Function definition

```
fun x = x * x
```

### Lambda Expression

```
fun = _\x y. x + y
```

### If Else Expression

```
fun x =
  if x == 0
  then 1
  else fun (x - 1)
```

### Let In Expression

```
fun x =
  let y = x
  in x + y
```

### Comments

```
/* This is comment */
// Just the way of C-style comment
```
