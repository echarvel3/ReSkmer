
def return_args(x):
    return(x*2, x*3, x*4)

if __name__ == '__main__':
    values = [return_args(x) for x in range(0,3)]
    for val in values:
        (one, two, three) = val
        print(one)
        print(two)
        print(three)

    print('{0}: hello {1}'.format(5, "Ana"))
