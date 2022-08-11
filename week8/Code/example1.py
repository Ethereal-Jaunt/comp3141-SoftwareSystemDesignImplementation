# referential transparency:
# - we can evaluate a function by substituting
#   its argument directly into the function body;
# - equivalently, the only thing that matters about
#   an argument is its value
# (showing this equivalence is not all that easy, though).


# referential transparency fails in Python
# example 1

## PROGRAM 1
def f(x):
    return int(x) - int(x)

print("VERSION 1")
y = f(input("% "))
print(y)
## END PROGRAM 1


## PROGRAM 2
def fP():
    return int(input("% ")) - int(input("% "))

print("VERSION 2")
y = fP()
print(y)
## END PROGRAM 2


# If Python was referentially transparent, the two
# programs would _behave exactly the same way_.
# But they don't.

# Failure of referential transparency means that you
# cannot prove things about your programs using
# equational reasoning. E.g. saying x - x = 0 is not valid!
