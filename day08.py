from collections import namedtuple


def parse(data):
    children = next(data)
    metadata_len = next(data)
    children = [parse(data) for _ in range(children)]
    metadata = [next(data) for _ in range(metadata_len)]
    return Node(metadata, children)


def metadata_sum(tree):
    return sum(tree.metadata) + sum(map(metadata_sum, tree.children))


def node_value(tree):
    if not tree.children:
        return sum(tree.metadata)
    return sum(
        node_value(tree.children[x - 1]) if x <= len(tree.children) else 0
        for x in tree.metadata
    )


Node = namedtuple("Node", "metadata, children")
tree = parse(map(int, open("inputs/day08.txt").read().split()))
print(metadata_sum(tree))
print(node_value(tree))
