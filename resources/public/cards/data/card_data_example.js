var card_data_example = [
    {
        "count": 1,
        "color": "maroon",
        "title": "Burning Hands",
        "icon": "white-book-1",
        "icon_back": "robe",
        "contents": [
            "subtitle | 1st level evocation",
            "rule",
            "property | Casting time | 1 action",
            "property | Range | Self (15ft cone)",
            "property | Components | V,S",
            "rule",
            "fill | 2",
            "text | Each creature in a 15-foot cone must make a Dexterity saving throw. A creature takes <b>3d6 fire damage</b> on a failed save, or half as much damage on a successful one.",
            "text | The fire ignites any flammable objects in the area that aren't being worn or carried.",
            "fill | 3",
            "section | At higher levels",
            "text | +1d6 damage for each slot above 1st"
        ],
        "tags": ["spell", "mage"]
    },
    {
        "count": 1,
        "color": "indigo",
        "title": "Cunning Action",
        "icon": "white-book",
        "icon_back": "cloak-dagger",
        "contents": [
            "subtitle | Rogue feature",
            "rule",
            "fill | 2",
            "text | You can take a <b>bonus action on each of your turns</b> in combat. This action can be used only to take the <b>Dash, Disengage, or Hide</b> action.",
            "fill | 2",
            "section | Fast hands (Thief 3rd)",
            "text | You can also use the bonus action to make a Dexterity (<b>Sleight of Hand</b>) check, use your thieves' tools to <b>disarm a trap</b> or <b>open a lock</b>, or take the <b>Use an Object</b> action."
        ],
        "tags": ["feature", "rogue"]
    },
    {
        "count": 1,
        "color": "dimgray",
        "title": "Full Plate",
        "icon": "breastplate",
        "contents": [
            "subtitle | Heavy armor (1500gp)",
            "rule",
            "property | AC | 18",
            "property | Strength required | 15",
            "property | Stealth | Disadvantage",
            "rule",
            "fill | 2",
            "description | Heavy | Unless you have the required strength, your speed is reduced by 10 feet.",
            "description | Stealth | You have disadvantage on Dexterity (Stealth) checks.",
            "fill | 3"
        ],
        "tags": ["item", "armor"]
    },
    {
        "count": 1,
        "color": "dimgray",
        "title": "Dagger",
        "icon": "mixed-swords",
        "contents": [
            "subtitle | Simple melee weapon (2gp)",
            "rule",
            "property | Damage | 1d4 piercing",
            "property | Modifier | Strength or Dexterity",
            "property | Properties | Light, Finesse, Thrown (20/60)",
            "rule",
            "fill | 2",
            "description | Finesse | Use your choice of Strength or Dexterity modifier for attack and damage.",
            "description | Light | When you attack while dual wielding light weapons, you may use a bonus action to attack with your off hand.",
            "description | Thrown | You can throw the weapon to make a ranged attack with the given range.",
            "fill | 3"
        ],
        "tags": ["item", "weapon"]
    },
    {
        "count": 1,
        "color": "dimgray",
        "title": "Shortsword of Very Long Names",
        "title_size": "10",
        "icon": "crossed-swords",
        "contents": [
            "subtitle | Simple melee weapon (10gp)",
            "rule",
            "property | Damage | 1d6 piercing",
            "property | Modifier | Strength or Dexterity",
            "property | Properties | Light, Finesse",
            "rule",
            "fill | 2",
            "description | Finesse | Use your choice of Strength or Dexterity modifier for attack and damage.",
            "description | Light | When you attack while dual wielding light weapons, you may use a bonus action to attack with your off hand.",
            "fill | 3"
        ],
        "tags": ["item", "weapon", "magic"]
    },
    {
        "count": 1,
        "color": "dimgray",
        "title": "Wand of Magic Missiles",
        "icon": "crystal-wand",
        "contents": [
            "subtitle | Wondrous item",
            "rule",
            "property | Maximum charges | 7",
            "property | Recharge | 1d6+1 each day",
            "property | Depletion | If you expend the last charge, roll a d20. On a 1, the item is destroyed.",
            "rule",
            "fill | 2",
            "description | Spells | You can use your action to cast the following spells:",
            "bullet | magic missile, 1st level (1 charge)",
            "bullet | magic missile, 2nd level (2 charges)",
            "bullet | magic missile, 3rd level (3 charges)",
            "fill | 3",
            "boxes | 7 | 2.5"
        ],
        "tags": ["item", "wondrous-item", "magic"]
    },
    {
        "count": 2,
        "color": "dimgray",
        "title": "Potion of Healing",
        "icon": "drink-me",
        "contents": [
            "subtitle | Potion (50gp)",
            "rule",
            "property | Use time | 1 action",
            "property | Hit points restored | 2d4+2",
            "rule",
            "fill | 2",
            "text | When you drink this potion, you regain 2d4+2 hitpoints.",
            "text | Drinking or administering a potion takes 1 action.",
            "fill | 3"
        ],
        "tags": ["item", "consumable"]
    },
    {
        "count": 1,
        "color": "black",
        "title": "Goblin",
        "icon": "imp-laugh",
        "contents": [
            "subtitle | Small humanoid (goblinoid)",
            "rule",
            "property | Armor class | 15 (leather armor, shield)",
            "property | Hit points | 7 (2d6)",
            "rule",
            "dndstats | 8 | 14 | 10 | 10 | 8 | 8",
            "rule",
            "property | Skills | Stealth +6",
            "property | Challenge | 1/4 (50 XP)",
            "rule",
            "description | Nimble escape | Disengage or Hide as bonus action",
            "fill | 2",
            "section | Actions",
            "description | Scimitar | Melee Weapon Attack: +4 to hit, reach 5 ft., one target. Hit: 5 (1d6 + 2) slashing damage"
        ],
        "tags": ["creature", "humanoid"]
    }
]