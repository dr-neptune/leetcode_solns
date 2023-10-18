"""
Is this the knapsack problem?

unconstrained knapsack problem
"""

from dataclasses import dataclass

@dataclass()
class LootItem:
    name: str
    gold: int
    height: int
    width: int

items = [LootItem(name, gold, height, width)
         for name, height, width, gold in
         [("Potion of Potionentiality", 1, 1, 30),
          ("Jeweled Dog Draught Excluder", 3, 1, 150),
          ("Spartan Shield", 2, 2, 300),
          ("Palindromic Sword o’Drows", 1, 3, 120),
          ("Unobsidian Armor", 2, 3, 540),
          ("Wardrobe of Infinite Lions", 20, 10, 1337),]]

@dataclass()
class Treasure:
    items: tuple[LootItem]


def get_best_rpg_inventory(width, height, treasure):
    pass

"""
idea

greedy approximation
sort items by value descending
fit as many items of the most valuable sort into bag
then choose next most valuable, and so on
"""
loot_bag = []
loot_bag_width = 5
loot_bag_height = 4
loot_bag_area = loot_bag_width * loot_bag_height

desc_value = sorted(items, key=lambda l: l.gold, reverse=True)

"""
probably not worth the time commitment

idea:
after adding each item, we have a series of rectangles that show the remaining area
we can make a list of spaces that are available after each addition
then if any future items are <= those spaces in wxh, we can add another item
"""

for item in desc_value:
    print(f"now looting: {item.name} with bag: {loot_bag_width} x {loot_bag_height} : {loot_bag_area}")
    # this is still incorrect, because it doesn't constrain on height/width
    w, h = item.width, item.height
    item_area = w * h
    while item_area < loot_bag_area:
        loot_bag.append(item)
        loot_bag_area -= item_area
        # loot_bag_width -= w
        # loot_bag_height -= h
        # maybe calculate the new bag area here

pprint(loot_bag)

# we need to constrict the width and height as we add items






def main():
    treasure = Treasure(
        (
            LootItem("Potion of Potionentiality", 30, 1, 1),
            LootItem("Jeweled Dog Draught Excluder", 150, 3, 1),
            LootItem("Spartan Shield", 300, 2, 2),
            LootItem("Palindromic Sword o'Drows", 120, 1, 3),
            LootItem("Unobsidian Armor", 540, 2, 3),
            LootItem("Wardrobe of Infinite Lions", 1337, 20, 10),
        )
    )
    best_rpg_inventory = get_best_rpg_inventory(5, 4, treasure)
    print(best_rpg_inventory.get_str_description())

main()
