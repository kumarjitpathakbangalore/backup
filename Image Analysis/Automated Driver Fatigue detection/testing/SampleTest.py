from __future__ import print_function


classval = 18


def replaceVal(x):
    global classval
    orig = classval
    classval = x
    print('Old Value is {1} is replaced with {2}',orig,classval)

if __name__ == '__main__':
    global classval
    print(' The global value of class is ',classval)
    replaceVal(int(input('Enter new Value:')))
    print(' The global value of class is ', classval)
