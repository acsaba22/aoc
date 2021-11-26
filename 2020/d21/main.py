# 2493 ok
def main():
    fname = 'd21/input.txt'
    sources = {}
    with open(fname) as f:
        for line in f:
            itemsS, allergensS = line.split('(')
            items = set(itemsS.split())
            allergens = allergensS.strip()[len('contains '):-1].split(', ')
            print(items, allergens)
            for allergen in allergens:
                if allergen in sources:
                    sources[allergen] &= items
                else:
                    sources[allergen] = set(items)
            print('updated:', sources)
    print(sources)
    badsources = set(x for v in sources.values() for x in v )
    p1 = 0
    with open(fname) as f:
        for line in f:
            itemsS, allergensS = line.split('(')
            items = itemsS.split()
            for i in items:
                if i not in badsources:
                    p1 += 1
    print(p1)
    pairs = []
    while len(sources):
        for allergen, items in sources.items():
            if len(items) == 1:
                item = items.pop()
                del sources [allergen]
                print('pair:', allergen, item)
                pairs.append((allergen, item))
                for itemsOthers in sources.values():
                    itemsOthers.discard(item)
                break
        print(sources)
    print(",".join(x[1] for x in sorted(pairs)))

if __name__ == '__main__':
    main()
