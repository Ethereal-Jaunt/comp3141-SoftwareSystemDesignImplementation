
# referential transparency fails in Python
# example 2

## PROGRAM 1
print("VERSION 1")
def g():
    print("ha", end='')
    print("ha", end='')
    print()
g()
## END PROGRAM 1


# We try to factor out the repetition:

## PROGRAM 2
print("VERSION 2")
def gP(x):
    x
    x
    print()

def h(x,y):
    return x+y

gP(h(1,2))  # x=3 != h(1,2)


gP(print("ha", end=''))

    # print("ha", end='')
    # print("ha", end='')
    # print()

## END PROGRAM 2


# If Python was referentially transparent, the two
# programs would _behave exactly the same way_.
# But the joke's on us: they don't.
