
# assemble fails on imbalanced trees
tree = [1, [2, 4, 5], [3, 6, 7]]
#tree = [1, [2, 4, [5, "a", "b"]], [3, 6, 7]]

print('tree', tree)

def bft(tree):
    ret = []
    queue = tree
    while queue:
        x = queue.pop(0)

        if isinstance(x, list):
            r = x.pop(0)
            ret.append(r)
            queue.extend(x)
        else:
            ret.append(x)

    return ret


bflist = bft(tree)
print(bflist)

def assemble(queue):
    level = [[], []]
    tree = [queue.pop(0)]
    tree.extend(level)
    try:
        while queue:
            newlevel = []
            for sub in level:
                sub.append(queue.pop(0))
                left = []
                newlevel.append(left)
                right = []
                newlevel.append(right)
                sub.append(left)
                sub.append(right)
            level = newlevel
    except IndexError:
        print("warning: unbalanced tree")
    finally:
        return tree

print(assemble(list(bflist)))
